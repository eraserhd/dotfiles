/*
 * The daemon and its clients.  A client sends its arguments, each ending in a
 * NUL, then an empty one, and the daemon answers with "status outlen errlen\n"
 * and then what `sigils` would print to stdout and stderr.  The daemon's -s
 * and -i are the only ones.
 *
 * Only one daemon runs per socket, the one holding a lock on the socket's
 * path with ".lock" added, which also holds its pid.  `sigils --daemon` stops
 * one which is running, but one which a query starts leaves it be.
 */
#define _DEFAULT_SOURCE
#define _DARWIN_C_SOURCE

#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/un.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

#include "sigils.h"

#define MAX_REQUEST 4096
#define START_TIMEOUT_MS 5000
#define STOP_TIMEOUT_MS 2000

/*
 * The default mustn't depend on the environment, which differs between what
 * GUI applications and shells are started with, or each would start its own
 * daemon.  On macOS, that's the per-user temporary directory $TMPDIR is set
 * from, and elsewhere $XDG_RUNTIME_DIR, which the session sets for everything.
 */
static int socket_path(char *path, size_t size)
{
	const char *env = getenv("SIGILS_SOCKET");
	int n;

	if (env && *env) {
		n = snprintf(path, size, "%s", env);
	} else {
#ifdef __APPLE__
		char dir[PATH_MAX];
		size_t len = confstr(_CS_DARWIN_USER_TEMP_DIR, dir, sizeof dir);
		if (len > 0 && len <= sizeof dir)
			n = snprintf(path, size, "%s%ssigils.sock", dir,
				     dir[strlen(dir) - 1] == '/' ? "" : "/");
		else
			n = snprintf(path, size, "/tmp/sigils-%ld.sock", (long)getuid());
#else
		const char *dir = getenv("XDG_RUNTIME_DIR");
		if (dir && *dir)
			n = snprintf(path, size, "%s/sigils.sock", dir);
		else
			n = snprintf(path, size, "/tmp/sigils-%ld.sock", (long)getuid());
#endif
	}
	if (n < 0 || (size_t)n >= size ||
	    (size_t)n >= sizeof ((struct sockaddr_un *)0)->sun_path) {
		fprintf(stderr, "sigils: the socket path %s is too long\n", path);
		return -1;
	}
	return 0;
}

static struct sockaddr_un address(const char *path)
{
	struct sockaddr_un addr = { .sun_family = AF_UNIX };

	strncpy(addr.sun_path, path, sizeof addr.sun_path - 1);
	return addr;
}

static int write_all(int fd, const char *data, size_t len)
{
	while (len > 0) {
		ssize_t n = write(fd, data, len);
		if (n < 0 && errno == EINTR)
			continue;
		if (n <= 0)
			return -1;
		data += n;
		len -= (size_t)n;
	}
	return 0;
}

/* Reads until EOF, into a buffer which is grown with realloc. */
static ssize_t read_all(int fd, char **data, size_t limit)
{
	size_t len = 0, size = 0;

	*data = NULL;
	for (;;) {
		if (len == size) {
			size = size ? 2 * size : 1024;
			if (limit && size > limit)
				size = limit;
			if (len == size)
				break;
			char *grown = realloc(*data, size + 1);
			if (!grown)
				goto fail;
			*data = grown;
		}
		ssize_t n = read(fd, *data + len, size - len);
		if (n < 0 && errno == EINTR)
			continue;
		if (n < 0)
			goto fail;
		if (n == 0)
			break;
		len += (size_t)n;
	}
	if (!*data && !(*data = malloc(1)))
		return -1;
	(*data)[len] = '\0';
	return (ssize_t)len;
fail:
	free(*data);
	*data = NULL;
	return -1;
}

static void respond(int fd, int status, const char *out, size_t outlen,
		    const char *err, size_t errlen)
{
	char header[64];
	int n = snprintf(header, sizeof header, "%d %zu %zu\n", status, outlen, errlen);

	if (write_all(fd, header, (size_t)n) == 0 && write_all(fd, out, outlen) == 0)
		write_all(fd, err, errlen);
}

void handle_client(int fd, const struct options *defaults)
{
	struct timeval timeout = { .tv_sec = 2 };
	char *request = NULL, *out = NULL, *err = NULL;
	size_t outlen = 0, errlen = 0;
	char *argv[MAX_REQUEST / 2 + 2];
	int argc = 0;

	setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
	ssize_t len = read_all(fd, &request, MAX_REQUEST);
	if (len < 1 || request[len - 1] != '\0') {
		free(request);
		return;
	}

	argv[argc++] = "sigils";
	for (char *arg = request; arg < request + len && *arg; arg += strlen(arg) + 1)
		argv[argc++] = arg;
	argv[argc] = NULL;

	FILE *outf = open_memstream(&out, &outlen);
	FILE *errf = open_memstream(&err, &errlen);
	int status = 1;
	if (outf && errf) {
		struct options options;
		status = parse_options(argc, argv, &options, errf);
		if (status == 0) {
			options.sigils = defaults->sigils;
			options.reserved = defaults->reserved;
			status = answer(&options, outf, errf);
		}
	}
	if (outf)
		fclose(outf);
	if (errf)
		fclose(errf);
	respond(fd, status, out ? out : "", out ? outlen : 0, err ? err : "",
		err ? errlen : 0);
	free(out);
	free(err);
	free(request);
}

int serve_clients(int listen_fd, const struct options *defaults)
{
	for (;;) {
		int fd = accept(listen_fd, NULL, NULL);
		if (fd < 0) {
			if (errno == EINTR || errno == ECONNABORTED)
				continue;
			perror("sigils: accept");
			return 1;
		}
		handle_client(fd, defaults);
		close(fd);
	}
}

/* Stops the daemon holding the lock, and takes the lock, or returns -1. */
static int stop_daemon(int lock)
{
	char pid_text[32] = "";
	ssize_t n = pread(lock, pid_text, sizeof pid_text - 1, 0);
	long pid = n > 0 ? strtol(pid_text, NULL, 10) : 0;

	if (pid <= 0 || pid == getpid())
		return -1;
	kill((pid_t)pid, SIGTERM);
	for (int waited = 0; waited < 2 * STOP_TIMEOUT_MS; waited += 10) {
		struct timespec ten_ms = { 0, 10 * 1000 * 1000 };
		if (flock(lock, LOCK_EX | LOCK_NB) == 0)
			return 0;
		if (waited == STOP_TIMEOUT_MS)
			kill((pid_t)pid, SIGKILL);
		nanosleep(&ten_ms, NULL);
	}
	fprintf(stderr, "sigils: the daemon %ld wouldn't stop\n", pid);
	return -1;
}

int run_daemon(const struct options *defaults)
{
	char path[PATH_MAX], lock_path[PATH_MAX + 8];

	if (socket_path(path, sizeof path) < 0)
		return 1;
	snprintf(lock_path, sizeof lock_path, "%s.lock", path);

	umask(077);
	int lock = open(lock_path, O_RDWR | O_CREAT, 0600);
	if (lock < 0) {
		fprintf(stderr, "sigils: can't open %s: %s\n", lock_path, strerror(errno));
		return 1;
	}
	if (flock(lock, LOCK_EX | LOCK_NB) < 0) {
		if (errno != EWOULDBLOCK)
			return 1;
		if (!defaults->replace || stop_daemon(lock) < 0)
			return 0;
	}
	if (ftruncate(lock, 0) == 0)
		dprintf(lock, "%ld\n", (long)getpid());

	/* Holding the lock, any socket left there is from a daemon which died. */
	unlink(path);
	int fd = socket(AF_UNIX, SOCK_STREAM, 0);
	struct sockaddr_un addr = address(path);
	if (fd < 0 || bind(fd, (struct sockaddr *)&addr, sizeof addr) < 0 ||
	    listen(fd, 16) < 0) {
		fprintf(stderr, "sigils: can't listen on %s: %s\n", path, strerror(errno));
		return 1;
	}
	signal(SIGPIPE, SIG_IGN);
	return serve(fd, defaults);
}

static int executable_path(char *path, size_t size)
{
#ifdef __APPLE__
	char unresolved[PATH_MAX];
	uint32_t n = sizeof unresolved;

	if (_NSGetExecutablePath(unresolved, &n) != 0 || !realpath(unresolved, path))
		return -1;
	(void)size;
	return 0;
#else
	ssize_t n = readlink("/proc/self/exe", path, size - 1);
	if (n < 0)
		return -1;
	path[n] = '\0';
	return 0;
#endif
}

/* Starts a daemon, detached from this process, with the default -s and -i. */
static void start_daemon(void)
{
	char exe[PATH_MAX];
	char *argv[] = { exe, "--daemon-unless-running", NULL };

	if (executable_path(exe, sizeof exe) < 0)
		return;

	pid_t pid = fork();
	if (pid == 0) {
		setsid();
		if (fork() != 0)
			_exit(0);
		int null = open("/dev/null", O_RDWR);
		if (null >= 0) {
			dup2(null, STDIN_FILENO);
			dup2(null, STDOUT_FILENO);
			dup2(null, STDERR_FILENO);
		}
		execv(exe, argv);
		_exit(127);
	}
	if (pid > 0)
		waitpid(pid, NULL, 0);
}

static int connect_to(const char *path)
{
	int fd = socket(AF_UNIX, SOCK_STREAM, 0);
	struct sockaddr_un addr = address(path);

	if (fd < 0)
		return -1;
	if (connect(fd, (struct sockaddr *)&addr, sizeof addr) < 0) {
		int saved = errno;
		close(fd);
		errno = saved;
		return -1;
	}
	return fd;
}

static int connect_or_start(const char *path)
{
	int fd = connect_to(path);

	if (fd >= 0 || (errno != ENOENT && errno != ECONNREFUSED))
		return fd;
	start_daemon();
	for (int waited = 0; waited < START_TIMEOUT_MS; waited += 10) {
		struct timespec ten_ms = { 0, 10 * 1000 * 1000 };
		if ((fd = connect_to(path)) >= 0)
			return fd;
		nanosleep(&ten_ms, NULL);
	}
	return -1;
}

int ask_daemon(const struct options *options)
{
	char path[PATH_MAX];
	char *response = NULL;
	ssize_t got;
	int status, n = 0;
	size_t outlen, errlen;

	if (socket_path(path, sizeof path) < 0)
		return 1;
	int fd = connect_or_start(path);
	if (fd < 0) {
		fprintf(stderr, "sigils: can't reach the daemon at %s: %s\n", path,
			strerror(errno));
		return 1;
	}

	char request[MAX_REQUEST];
	size_t len = 0;
	const char *args[] = {
		options->sigil ? "--" : NULL, options->sigil,
	};
	for (size_t i = 0; i < sizeof args / sizeof args[0]; i += 2) {
		if (!args[i])
			continue;
		for (size_t j = i; j < i + 2; j++) {
			size_t arglen = strlen(args[j]) + 1;
			if (len + arglen + 1 > sizeof request) {
				fprintf(stderr, "sigils: arguments are too long\n");
				close(fd);
				return 2;
			}
			memcpy(request + len, args[j], arglen);
			len += arglen;
		}
	}
	request[len++] = '\0';

	signal(SIGPIPE, SIG_IGN);
	if (write_all(fd, request, len) < 0 || shutdown(fd, SHUT_WR) < 0 ||
	    (got = read_all(fd, &response, 0)) < 0 ||
	    sscanf(response, "%d %zu %zu\n%n", &status, &outlen, &errlen, &n) != 3 ||
	    n == 0 || (size_t)(got - n) < outlen + errlen) {
		fprintf(stderr, "sigils: the daemon at %s didn't answer\n", path);
		free(response);
		close(fd);
		return 1;
	}
	fwrite(response + n, 1, outlen, stdout);
	fwrite(response + n + outlen, 1, errlen, stderr);
	free(response);
	close(fd);
	return status;
}
