/*
 * sigils - list windows with a one-character name each, or find one by it.
 *
 *   sigils                sigil id x y width height app title
 *   sigils <sigil>        the id of the window with that sigil
 *
 * Empty spaces between the windows get sigils too, with id 0 and app "-".
 * The answers come from a daemon (see below), so that it can be started
 * with the permissions to see the windows, and keep the sigils drawn.
 * Sigils are handed out in order of position, left to right and then top to
 * bottom, so they stay put while the windows do.
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
/* The windows come from fake ones for tests, macOS, or else X. */
#if defined(SIGILS_FAKE)
#elif defined(__APPLE__) && !defined(SIGILS_X11)
#include <ApplicationServices/ApplicationServices.h>
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <dispatch/dispatch.h>
#include <libproc.h>
#include <objc/message.h>
#include <objc/runtime.h>
#else
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

/* --- declarations ------------------------------------------------------- */

/* Windows and empty spaces smaller than this, either way, are left out. */
#define MIN_SIZE 20

struct window
{
    unsigned long id; /* 0 for an empty space */
    int x, y, width, height;
    char* app; /* bundle identifier or window class, without spaces */
    char* title;
    char sigil;
    int z; /* front to back, among the windows */
};

struct rect
{
    int x, y, width, height;
};

/* Pixels along the edges of every display which are never empty space. */
struct insets
{
    int top, bottom, left, right;
};

struct options
{
    const char* sigils; /* NULL for the default */
    const char* insets; /* as given to -i, or NULL */
    struct insets reserved;
    const char* sigil; /* the one to look up, or NULL to list them all */
    int daemon;
    int replace; /* the daemon replaces one which is running */
    int help;
};

/* Fills in *options from the arguments, or returns 2 after printing the usage to err. */
int parse_options(int argc, char** argv, struct options* options, FILE* err);

/*
 * Lists the windows and empty spaces in order of position, with their sigils,
 * and returns 0, or returns -1 after printing why not to err.
 */
int sigil_windows(const struct options* options, struct window** windows, size_t* count, FILE* err);

void free_windows(struct window* windows, size_t count);

/* Prints what `sigils` prints for these options, and returns its exit status. */
int answer(const struct options* options, FILE* out, FILE* err);

/* Runs the daemon with these defaults for its queries, and returns its exit status. */
int run_daemon(const struct options* defaults);

/* Asks the daemon, starting it if it isn't running, and returns the exit status. */
int ask_daemon(const struct options* options);

/* Answers a client which connected to the daemon. */
void handle_client(int fd, const struct options* defaults);

/* Answers clients on the daemon's listening socket until something goes wrong. */
int serve_clients(int listen_fd, const struct options* defaults);

/* The backend's way of answering clients, which may also draw the sigils. */
int serve(int listen_fd, const struct options* defaults);

/*
 * Appends the windows which are visible on a display to *windows, which is
 * grown with realloc, and returns 0, or returns -1 after printing why not.
 * Windows are not sorted or given sigils.
 */
int list_windows(struct window** windows, size_t* count);

/* Like list_windows, but for the bounds of the displays. */
int list_displays(struct rect** displays, size_t* count);

/*
 * Appends the empty spaces on the displays which no window covers, as
 * windows with id 0, the same way as list_windows.
 */
int add_empty_spaces(struct window** windows, size_t* count, const struct rect* displays,
                     size_t ndisplays, struct insets reserved);

/* --- options and listing ------------------------------------------------ */

static const char DEFAULT_SIGILS[] = "abcdefgimnopqrstuvwxyz;,/";

static int by_position(const void* a, const void* b)
{
    const struct window *l = a, *r = b;

    if (l->x != r->x)
        return l->x < r->x ? -1 : 1;
    if (l->y != r->y)
        return l->y < r->y ? -1 : 1;
    if (l->id != r->id)
        return l->id < r->id ? -1 : 1;
    return 0;
}

static void usage(FILE* out)
{
    fputs("usage: sigils [sigil]\n"
          "       sigils --daemon [-s sigils] [-i edge=pixels,...]\n"
          "\n"
          "Without an argument, lists the visible windows as\n"
          "\n"
          "  sigil id x y width height app title\n"
          "\n"
          "where app is the bundle identifier (macOS) or window class (X),\n"
          "and title is the rest of the line.  Empty spaces between windows\n"
          "are listed too, with id 0 and app -.  With a sigil, prints just the\n"
          "id of its window, and fails if there isn't one.\n"
          "\n"
          "The windows come from a daemon, which is started if it isn't\n"
          "running, on the socket $SIGILS_SOCKET, or else sigils.sock in the\n"
          "user's temporary directory (macOS) or $XDG_RUNTIME_DIR (X).\n"
          "With --daemon, it runs that daemon, replacing one which is running,\n"
          "and on macOS it also draws the sigils over the windows, with\n"
          "\n"
          "  -s sigils   the characters to use, in order\n"
          "              (default: abcdefgimnopqrstuvwxyz;,/)\n"
          "  -i insets   strips along the edges of every display which are\n"
          "              never empty space, e.g. top=40 for a status bar;\n"
          "              the edges are top, bottom, left and right\n",
          out);
}

static void print_clean(FILE* out, const char* s, int keep_spaces)
{
    for (; *s; s++)
    {
        unsigned char c = (unsigned char)*s;
        if (c == '\n' || c == '\r' || c == '\t')
            putc(' ', out);
        else if (c == ' ' && !keep_spaces)
            putc('_', out);
        else
            putc(c, out);
    }
}

static void print_window(FILE* out, const struct window* w)
{
    fprintf(out, "%c %lu %d %d %d %d ", w->sigil, w->id, w->x, w->y, w->width, w->height);
    print_clean(out, w->app, 0);
    putc(' ', out);
    print_clean(out, w->title, 1);
    putc('\n', out);
}

/* Parses edge=pixels[,edge=pixels...], where edge is top, bottom, left or right. */
static int parse_insets(const char* spec, struct insets* insets)
{
    while (*spec)
    {
        int* edge;
        size_t len = strcspn(spec, "=");

        if (len == 3 && !strncmp(spec, "top", len))
            edge = &insets->top;
        else if (len == 6 && !strncmp(spec, "bottom", len))
            edge = &insets->bottom;
        else if (len == 4 && !strncmp(spec, "left", len))
            edge = &insets->left;
        else if (len == 5 && !strncmp(spec, "right", len))
            edge = &insets->right;
        else
            return -1;
        spec += len;
        if (*spec++ != '=')
            return -1;

        char* end;
        long pixels = strtol(spec, &end, 10);
        if (end == spec || pixels < 0 || (*end && *end != ','))
            return -1;
        *edge = (int)pixels;
        spec = *end ? end + 1 : end;
    }
    return 0;
}

int parse_options(int argc, char** argv, struct options* options, FILE* err)
{
    int opt;

    *options = (struct options){ 0 };
    for (opt = 1; opt < argc && argv[opt][0] == '-' && argv[opt][1]; opt++)
    {
        if (strcmp(argv[opt], "--") == 0)
        {
            opt++;
            break;
        }
        else if (strcmp(argv[opt], "-s") == 0 && opt + 1 < argc)
        {
            options->sigils = argv[++opt];
        }
        else if (strcmp(argv[opt], "-i") == 0 && opt + 1 < argc)
        {
            options->insets = argv[++opt];
            options->reserved = (struct insets){ 0 };
            if (parse_insets(options->insets, &options->reserved) < 0)
            {
                fprintf(err, "sigils: bad insets %s\n", options->insets);
                return 2;
            }
        }
        else if (strcmp(argv[opt], "--daemon") == 0)
        {
            options->daemon = 1;
            options->replace = 1;
        }
        else if (strcmp(argv[opt], "--daemon-unless-running") == 0)
        {
            /* How queries start one. */
            options->daemon = 1;
        }
        else if (strcmp(argv[opt], "-h") == 0)
        {
            options->help = 1;
        }
        else
        {
            usage(err);
            return 2;
        }
    }
    if (argc - opt > 1 || (argc - opt == 1 && strlen(argv[opt]) != 1) ||
        (options->daemon && argc - opt == 1))
    {
        usage(err);
        return 2;
    }
    if (!options->daemon && (options->sigils || options->insets))
    {
        fprintf(err, "sigils: -s and -i are only for --daemon\n");
        return 2;
    }
    if (argc - opt == 1)
        options->sigil = argv[opt];
    return 0;
}

void free_windows(struct window* windows, size_t count)
{
    for (size_t i = 0; i < count; i++)
    {
        free(windows[i].app);
        free(windows[i].title);
    }
    free(windows);
}

int sigil_windows(const struct options* options, struct window** windows, size_t* count, FILE* err)
{
    const char* sigils = options->sigils ? options->sigils : DEFAULT_SIGILS;
    struct rect* displays = NULL;
    size_t ndisplays = 0;
    int result = -1;

    *windows = NULL;
    *count = 0;
    if (list_windows(windows, count) < 0 || list_displays(&displays, &ndisplays) < 0 ||
        add_empty_spaces(windows, count, displays, ndisplays, options->reserved) < 0)
        goto done;
    qsort(*windows, *count, sizeof(*windows)[0], by_position);

    size_t nsigils = strlen(sigils);
    for (size_t i = 0; i < *count; i++)
        (*windows)[i].sigil = i < nsigils ? sigils[i] : '\0';
    result = 0;
done:
    if (result < 0)
        fprintf(err, "sigils: can't list the windows\n");
    free(displays);
    return result;
}

int answer(const struct options* options, FILE* out, FILE* err)
{
    struct window* windows;
    size_t count;
    int status = 1;

    if (sigil_windows(options, &windows, &count, err) < 0)
        goto done;

    if (options->sigil)
    {
        for (size_t i = 0; i < count; i++)
        {
            if (windows[i].sigil == options->sigil[0])
            {
                fprintf(out, "%lu\n", windows[i].id);
                status = 0;
                goto done;
            }
        }
        fprintf(err, "sigils: no window has the sigil %s\n", options->sigil);
        goto done;
    }

    for (size_t i = 0; i < count && windows[i].sigil; i++)
        print_window(out, &windows[i]);
    status = 0;
done:
    free_windows(windows, count);
    return status;
}

/* --- empty spaces ------------------------------------------------------- */

/*
 * Empty spaces: the edges of the displays and windows cut the screen into a
 * grid, and runs of empty cells are grown into rectangles, first down, then
 * right, from the top left of each display.  As in hs.geometry, a rectangle
 * spans x through x + width, so it takes up width + 1 columns of the grid.
 */

struct coordinates
{
    int* v;
    size_t n;
};

static int ascending(const void* a, const void* b)
{
    int l = *(const int*)a, r = *(const int*)b;
    return (l > r) - (l < r);
}

static void add_edges(struct coordinates* xs, struct coordinates* ys, struct rect r)
{
    xs->v[xs->n++] = r.x;
    xs->v[xs->n++] = r.x + r.width + 1;
    ys->v[ys->n++] = r.y;
    ys->v[ys->n++] = r.y + r.height + 1;
}

static void sort_unique(struct coordinates* c)
{
    size_t n = 0;

    qsort(c->v, c->n, sizeof c->v[0], ascending);
    for (size_t i = 0; i < c->n; i++)
        if (n == 0 || c->v[n - 1] != c->v[i])
            c->v[n++] = c->v[i];
    c->n = n;
}

static size_t offset(const struct coordinates* c, int value)
{
    const int* found = bsearch(&value, c->v, c->n, sizeof c->v[0], ascending);
    return (size_t)(found - c->v);
}

struct grid
{
    struct coordinates xs, ys;
    char* occupied;
};

#define CELL(g, row, col) ((g)->occupied[(row) * (g)->xs.n + (col)])

static void occupy(struct grid* g, size_t top, size_t left, size_t bottom, size_t right)
{
    for (size_t row = top; row <= bottom; row++)
        for (size_t col = left; col <= right; col++)
            CELL(g, row, col) = 1;
}

static void occupy_rect(struct grid* g, struct rect r)
{
    occupy(g, offset(&g->ys, r.y), offset(&g->xs, r.x), offset(&g->ys, r.y + r.height + 1) - 1,
           offset(&g->xs, r.x + r.width + 1) - 1);
}

static size_t row_right_extent(struct grid* g, size_t row, size_t left, size_t right_limit)
{
    size_t right = left;

    while (right + 1 <= right_limit && !CELL(g, row, right + 1))
        right++;
    return right;
}

static void empty_extent(struct grid* g, size_t top, size_t left, size_t bottom_limit,
                         size_t right_limit, size_t* bottom, size_t* right)
{
    *bottom = top;
    while (*bottom + 1 <= bottom_limit && !CELL(g, *bottom + 1, left))
        (*bottom)++;
    *right = row_right_extent(g, top, left, right_limit);
    for (size_t row = top + 1; row <= *bottom; row++)
    {
        size_t r = row_right_extent(g, row, left, right_limit);
        if (r < *right)
            *right = r;
    }
}

static int append_space(struct window** windows, size_t* count, struct rect r)
{
    struct window* grown = realloc(*windows, (*count + 1) * sizeof **windows);
    char *app = strdup("-"), *title = strdup("");

    if (!grown || !app || !title)
    {
        perror("sigils");
        free(app);
        free(title);
        if (grown)
            *windows = grown;
        return -1;
    }
    *windows = grown;
    grown[*count] = (struct window){
        .id = 0,
        .x = r.x,
        .y = r.y,
        .width = r.width,
        .height = r.height,
        .app = app,
        .title = title,
    };
    (*count)++;
    return 0;
}

static size_t reserved_strips(struct rect d, struct insets in, struct rect* strips)
{
    struct rect all[] = {
        { d.x, d.y, d.width, in.top },
        { d.x, d.y + d.height - in.bottom, d.width, in.bottom },
        { d.x, d.y, in.left, d.height },
        { d.x + d.width - in.right, d.y, in.right, d.height },
    };
    size_t n = 0;

    for (size_t i = 0; i < 4; i++)
        if (all[i].width > 0 && all[i].height > 0)
            strips[n++] = all[i];
    return n;
}

int add_empty_spaces(struct window** windows, size_t* count, const struct rect* displays,
                     size_t ndisplays, struct insets reserved)
{
    size_t nwindows = *count;
    size_t nstrips = 0;
    struct rect* strips = malloc((4 * ndisplays + 1) * sizeof *strips);
    size_t most = 2 * (nwindows + ndisplays + 4 * ndisplays) + 1;
    struct grid g = {
        .xs = { malloc(most * sizeof(int)), 0 },
        .ys = { malloc(most * sizeof(int)), 0 },
    };
    int result = -1;

    if (!strips || !g.xs.v || !g.ys.v)
    {
        perror("sigils");
        goto done;
    }
    for (size_t i = 0; i < ndisplays; i++)
    {
        add_edges(&g.xs, &g.ys, displays[i]);
        nstrips += reserved_strips(displays[i], reserved, strips + nstrips);
    }
    for (size_t i = 0; i < nwindows; i++)
        add_edges(&g.xs, &g.ys,
                  (struct rect){ (*windows)[i].x, (*windows)[i].y, (*windows)[i].width,
                                 (*windows)[i].height });
    for (size_t i = 0; i < nstrips; i++)
        add_edges(&g.xs, &g.ys, strips[i]);
    sort_unique(&g.xs);
    sort_unique(&g.ys);

    g.occupied = calloc(g.xs.n * g.ys.n + 1, 1);
    if (!g.occupied)
    {
        perror("sigils");
        goto done;
    }
    for (size_t i = 0; i < nwindows; i++)
        occupy_rect(&g, (struct rect){ (*windows)[i].x, (*windows)[i].y, (*windows)[i].width,
                                       (*windows)[i].height });
    for (size_t i = 0; i < nstrips; i++)
        occupy_rect(&g, strips[i]);

    for (size_t i = 0; i < ndisplays; i++)
    {
        struct rect d = displays[i];
        size_t top = offset(&g.ys, d.y);
        size_t left = offset(&g.xs, d.x);
        size_t bottom_limit = offset(&g.ys, d.y + d.height + 1) - 1;
        size_t right_limit = offset(&g.xs, d.x + d.width + 1) - 1;

        for (size_t row = top; row <= bottom_limit; row++)
        {
            for (size_t col = left; col <= right_limit; col++)
            {
                size_t bottom, right;

                if (CELL(&g, row, col))
                    continue;
                empty_extent(&g, row, col, bottom_limit, right_limit, &bottom, &right);
                occupy(&g, row, col, bottom, right);
                struct rect r = {
                    .x = g.xs.v[col],
                    .y = g.ys.v[row],
                    .width = g.xs.v[right + 1] - 1 - g.xs.v[col],
                    .height = g.ys.v[bottom + 1] - 1 - g.ys.v[row],
                };
                if (r.width >= MIN_SIZE && r.height >= MIN_SIZE &&
                    append_space(windows, count, r) < 0)
                    goto done;
            }
        }
    }
    result = 0;
done:
    free(strips);
    free(g.xs.v);
    free(g.ys.v);
    free(g.occupied);
    return result;
}

/* --- daemon and clients ------------------------------------------------- */

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


#define MAX_REQUEST 4096
#define START_TIMEOUT_MS 5000
#define STOP_TIMEOUT_MS 2000

/*
 * The default mustn't depend on the environment, which differs between what
 * GUI applications and shells are started with, or each would start its own
 * daemon.  On macOS, that's the per-user temporary directory $TMPDIR is set
 * from, and elsewhere $XDG_RUNTIME_DIR, which the session sets for everything.
 */
static int socket_path(char* path, size_t size)
{
    const char* env = getenv("SIGILS_SOCKET");
    int n;

    if (env && *env)
    {
        n = snprintf(path, size, "%s", env);
    }
    else
    {
#ifdef __APPLE__
        char dir[PATH_MAX];
        size_t len = confstr(_CS_DARWIN_USER_TEMP_DIR, dir, sizeof dir);
        if (len > 0 && len <= sizeof dir)
            n = snprintf(path, size, "%s%ssigils.sock", dir,
                         dir[strlen(dir) - 1] == '/' ? "" : "/");
        else
            n = snprintf(path, size, "/tmp/sigils-%ld.sock", (long)getuid());
#else
        const char* dir = getenv("XDG_RUNTIME_DIR");
        if (dir && *dir)
            n = snprintf(path, size, "%s/sigils.sock", dir);
        else
            n = snprintf(path, size, "/tmp/sigils-%ld.sock", (long)getuid());
#endif
    }
    if (n < 0 || (size_t)n >= size || (size_t)n >= sizeof((struct sockaddr_un*)0)->sun_path)
    {
        fprintf(stderr, "sigils: the socket path %s is too long\n", path);
        return -1;
    }
    return 0;
}

static struct sockaddr_un address(const char* path)
{
    struct sockaddr_un addr = { .sun_family = AF_UNIX };

    strncpy(addr.sun_path, path, sizeof addr.sun_path - 1);
    return addr;
}

static int write_all(int fd, const char* data, size_t len)
{
    while (len > 0)
    {
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
static ssize_t read_all(int fd, char** data, size_t limit)
{
    size_t len = 0, size = 0;

    *data = NULL;
    for (;;)
    {
        if (len == size)
        {
            size = size ? 2 * size : 1024;
            if (limit && size > limit)
                size = limit;
            if (len == size)
                break;
            char* grown = realloc(*data, size + 1);
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

static void respond(int fd, int status, const char* out, size_t outlen, const char* err,
                    size_t errlen)
{
    char header[64];
    int n = snprintf(header, sizeof header, "%d %zu %zu\n", status, outlen, errlen);

    if (write_all(fd, header, (size_t)n) == 0 && write_all(fd, out, outlen) == 0)
        write_all(fd, err, errlen);
}

void handle_client(int fd, const struct options* defaults)
{
    struct timeval timeout = { .tv_sec = 2 };
    char *request = NULL, *out = NULL, *err = NULL;
    size_t outlen = 0, errlen = 0;
    char* argv[MAX_REQUEST / 2 + 2];
    int argc = 0;

    setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof timeout);
    ssize_t len = read_all(fd, &request, MAX_REQUEST);
    if (len < 1 || request[len - 1] != '\0')
    {
        free(request);
        return;
    }

    argv[argc++] = "sigils";
    for (char* arg = request; arg < request + len && *arg; arg += strlen(arg) + 1)
        argv[argc++] = arg;
    argv[argc] = NULL;

    FILE* outf = open_memstream(&out, &outlen);
    FILE* errf = open_memstream(&err, &errlen);
    int status = 1;
    if (outf && errf)
    {
        struct options options;
        status = parse_options(argc, argv, &options, errf);
        if (status == 0)
        {
            options.sigils = defaults->sigils;
            options.reserved = defaults->reserved;
            status = answer(&options, outf, errf);
        }
    }
    if (outf)
        fclose(outf);
    if (errf)
        fclose(errf);
    respond(fd, status, out ? out : "", out ? outlen : 0, err ? err : "", err ? errlen : 0);
    free(out);
    free(err);
    free(request);
}

int serve_clients(int listen_fd, const struct options* defaults)
{
    for (;;)
    {
        int fd = accept(listen_fd, NULL, NULL);
        if (fd < 0)
        {
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
    for (int waited = 0; waited < 2 * STOP_TIMEOUT_MS; waited += 10)
    {
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

int run_daemon(const struct options* defaults)
{
    char path[PATH_MAX], lock_path[PATH_MAX + 8];

    if (socket_path(path, sizeof path) < 0)
        return 1;
    snprintf(lock_path, sizeof lock_path, "%s.lock", path);

    umask(077);
    int lock = open(lock_path, O_RDWR | O_CREAT, 0600);
    if (lock < 0)
    {
        fprintf(stderr, "sigils: can't open %s: %s\n", lock_path, strerror(errno));
        return 1;
    }
    if (flock(lock, LOCK_EX | LOCK_NB) < 0)
    {
        if (errno != EWOULDBLOCK)
            return 1;
        if (!defaults->replace || stop_daemon(lock) < 0)
            return 0;
    }
    if (ftruncate(lock, 0) == 0)
    {
        dprintf(lock, "%ld\n", (long)getpid());
        /* For a daemon which a query starts after this one dies. */
        if (defaults->sigils)
            dprintf(lock, "-s\n%s\n", defaults->sigils);
        if (defaults->insets)
            dprintf(lock, "-i\n%s\n", defaults->insets);
    }

    /* Holding the lock, any socket left there is from a daemon which died. */
    unlink(path);
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr = address(path);
    if (fd < 0 || bind(fd, (struct sockaddr*)&addr, sizeof addr) < 0 || listen(fd, 16) < 0)
    {
        fprintf(stderr, "sigils: can't listen on %s: %s\n", path, strerror(errno));
        return 1;
    }
    signal(SIGPIPE, SIG_IGN);
    return serve(fd, defaults);
}

static int executable_path(char* path, size_t size)
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

/* The -s and -i of the last daemon on this socket, from its lock, after its pid. */
static int last_daemon_options(const char* path, char* saved, size_t size, char** argv, int max)
{
    char lock_path[PATH_MAX + 8];
    int argc = 0;

    snprintf(lock_path, sizeof lock_path, "%s.lock", path);
    FILE* lock = fopen(lock_path, "r");
    if (!lock)
        return 0;
    size_t n = fread(saved, 1, size - 1, lock);
    fclose(lock);
    saved[n] = '\0';

    char* line = strchr(saved, '\n');
    char* end;
    if (!line)
        return 0;
    for (line++; argc < max && (end = strchr(line, '\n')); line = end + 1)
    {
        *end = '\0';
        argv[argc++] = line;
    }
    return argc;
}

/* Starts a daemon, detached from this process, with the last one's -s and -i. */
static void start_daemon(const char* path)
{
    char exe[PATH_MAX];
    char saved[MAX_REQUEST];
    char* argv[8] = { exe, "--daemon-unless-running" };
    int argc = 2;

    if (executable_path(exe, sizeof exe) < 0)
        return;
    argc += last_daemon_options(path, saved, sizeof saved, argv + argc, 4);
    argv[argc] = NULL;

    pid_t pid = fork();
    if (pid == 0)
    {
        setsid();
        if (fork() != 0)
            _exit(0);
        int null = open("/dev/null", O_RDWR);
        if (null >= 0)
        {
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

static int connect_to(const char* path)
{
    int fd = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr = address(path);

    if (fd < 0)
        return -1;
    if (connect(fd, (struct sockaddr*)&addr, sizeof addr) < 0)
    {
        int saved = errno;
        close(fd);
        errno = saved;
        return -1;
    }
    return fd;
}

static int connect_or_start(const char* path)
{
    int fd = connect_to(path);

    if (fd >= 0 || (errno != ENOENT && errno != ECONNREFUSED))
        return fd;
    start_daemon(path);
    for (int waited = 0; waited < START_TIMEOUT_MS; waited += 10)
    {
        struct timespec ten_ms = { 0, 10 * 1000 * 1000 };
        if ((fd = connect_to(path)) >= 0)
            return fd;
        nanosleep(&ten_ms, NULL);
    }
    return -1;
}

int ask_daemon(const struct options* options)
{
    char path[PATH_MAX];
    char* response = NULL;
    ssize_t got;
    int status, n = 0;
    size_t outlen, errlen;

    if (socket_path(path, sizeof path) < 0)
        return 1;
    int fd = connect_or_start(path);
    if (fd < 0)
    {
        fprintf(stderr, "sigils: can't reach the daemon at %s: %s\n", path, strerror(errno));
        return 1;
    }

    char request[MAX_REQUEST];
    size_t len = 0;
    const char* args[] = {
        options->sigil ? "--" : NULL,
        options->sigil,
    };
    for (size_t i = 0; i < sizeof args / sizeof args[0]; i += 2)
    {
        if (!args[i])
            continue;
        for (size_t j = i; j < i + 2; j++)
        {
            size_t arglen = strlen(args[j]) + 1;
            if (len + arglen + 1 > sizeof request)
            {
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
        sscanf(response, "%d %zu %zu\n%n", &status, &outlen, &errlen, &n) != 3 || n == 0 ||
        (size_t)(got - n) < outlen + errlen)
    {
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

#if defined(SIGILS_FAKE)

/* --- fake windows, for tests -------------------------------------------- */

/*
 * For tests: windows and displays from the file named by $SIGILS_FAKE_WINDOWS,
 * read again for every query, with lines like
 *
 *   display x y width height
 *   window id x y width height app title...
 */

static FILE* open_fake(void)
{
    const char* path = getenv("SIGILS_FAKE_WINDOWS");
    FILE* f = path ? fopen(path, "r") : NULL;

    if (!f)
        fprintf(stderr, "sigils: can't read $SIGILS_FAKE_WINDOWS\n");
    return f;
}

int list_windows(struct window** windows, size_t* count)
{
    FILE* f = open_fake();
    char line[1024];

    if (!f)
        return -1;
    while (fgets(line, sizeof line, f))
    {
        struct window w = { 0 };
        char app[256];
        int title = 0;

        line[strcspn(line, "\n")] = '\0';
        if (sscanf(line, "window %lu %d %d %d %d %255s %n", &w.id, &w.x, &w.y, &w.width, &w.height,
                   app, &title) < 6)
            continue;
        w.app = strdup(app);
        w.title = strdup(title ? line + title : "");
        struct window* grown = realloc(*windows, (*count + 1) * sizeof **windows);
        if (!grown || !w.app || !w.title)
        {
            fclose(f);
            return -1;
        }
        *windows = grown;
        w.z = (int)*count;
        grown[(*count)++] = w;
    }
    fclose(f);
    return 0;
}

int list_displays(struct rect** displays, size_t* count)
{
    FILE* f = open_fake();
    char line[1024];

    if (!f)
        return -1;
    while (fgets(line, sizeof line, f))
    {
        struct rect r;

        if (sscanf(line, "display %d %d %d %d", &r.x, &r.y, &r.width, &r.height) != 4)
            continue;
        struct rect* grown = realloc(*displays, (*count + 1) * sizeof **displays);
        if (!grown)
        {
            fclose(f);
            return -1;
        }
        *displays = grown;
        grown[(*count)++] = r;
    }
    fclose(f);
    return 0;
}

int serve(int listen_fd, const struct options* defaults)
{
    return serve_clients(listen_fd, defaults);
}

#elif defined(__APPLE__) && !defined(SIGILS_X11)

/* --- macOS windows ------------------------------------------------------ */

/*
 * macOS: windows from the window server, with their titles from the
 * accessibility API, which needs Accessibility to be allowed for whatever
 * runs this.  The window server only gives other applications' titles with
 * Screen Recording allowed, so they're the fallback.
 */

#define MAX_DISPLAYS 16

static char* copy_string(CFStringRef s)
{
    if (!s)
        return strdup("");
    CFIndex size =
        CFStringGetMaximumSizeForEncoding(CFStringGetLength(s), kCFStringEncodingUTF8) + 1;
    char* out = malloc(size);
    if (!out)
        return NULL;
    if (!CFStringGetCString(s, out, size, kCFStringEncodingUTF8))
        out[0] = '\0';
    return out;
}

static char* bundle_id_of_pid(int pid)
{
    char path[PROC_PIDPATHINFO_MAXSIZE];

    if (proc_pidpath(pid, path, sizeof path) <= 0)
        return NULL;
    char* app = strstr(path, ".app/");
    if (!app)
        return NULL;
    app[4] = '\0';

    CFURLRef url =
        CFURLCreateFromFileSystemRepresentation(NULL, (const UInt8*)path, strlen(path), true);
    if (!url)
        return NULL;
    CFBundleRef bundle = CFBundleCreate(NULL, url);
    CFRelease(url);
    if (!bundle)
        return NULL;
    char* id = NULL;
    CFStringRef ident = CFBundleGetIdentifier(bundle);
    if (ident)
        id = copy_string(ident);
    CFRelease(bundle);
    return id;
}

/* Private, but how Hammerspoon matches accessibility windows to the window server's. */
extern AXError _AXUIElementGetWindow(AXUIElementRef element, CGWindowID* id);

struct app_titles
{
    pid_t pid;
    CFDictionaryRef titles; /* window number -> title */
};

static CFDictionaryRef copy_app_titles(pid_t pid)
{
    CFMutableDictionaryRef titles = CFDictionaryCreateMutable(
        NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
    AXUIElementRef app = AXUIElementCreateApplication(pid);
    CFArrayRef windows = NULL;

    if (app && AXUIElementCopyAttributeValues(app, kAXWindowsAttribute, 0, 100, &windows) ==
                   kAXErrorSuccess)
    {
        for (CFIndex i = 0; i < CFArrayGetCount(windows); i++)
        {
            AXUIElementRef window = CFArrayGetValueAtIndex(windows, i);
            CGWindowID id;
            CFTypeRef title = NULL;

            if (_AXUIElementGetWindow(window, &id) != kAXErrorSuccess)
                continue;
            if (AXUIElementCopyAttributeValue(window, kAXTitleAttribute, &title) ==
                    kAXErrorSuccess &&
                CFGetTypeID(title) == CFStringGetTypeID())
            {
                long n = id;
                CFNumberRef key = CFNumberCreate(NULL, kCFNumberLongType, &n);
                CFDictionarySetValue(titles, key, title);
                CFRelease(key);
            }
            if (title)
                CFRelease(title);
        }
        CFRelease(windows);
    }
    if (app)
        CFRelease(app);
    return titles;
}

/* The title of a window, from its application's accessibility windows. */
static CFStringRef ax_title(struct app_titles** apps, size_t* napps, pid_t pid, long number)
{
    struct app_titles* app = NULL;

    for (size_t i = 0; i < *napps; i++)
        if ((*apps)[i].pid == pid)
            app = &(*apps)[i];
    if (!app)
    {
        struct app_titles* grown = realloc(*apps, (*napps + 1) * sizeof **apps);
        if (!grown)
            return NULL;
        *apps = grown;
        app = &grown[(*napps)++];
        *app = (struct app_titles){ pid, copy_app_titles(pid) };
    }

    CFNumberRef key = CFNumberCreate(NULL, kCFNumberLongType, &number);
    CFStringRef title = CFDictionaryGetValue(app->titles, key);
    CFRelease(key);
    return title;
}

static void free_app_titles(struct app_titles* apps, size_t napps)
{
    for (size_t i = 0; i < napps; i++)
        CFRelease(apps[i].titles);
    free(apps);
}

static int excluded(const char* app, const char* title)
{
    /* The same ones WindowSigils leaves out. */
    if (!strcmp(app, "com.apple.notificationcenterui") ||
        !strcmp(app, "com.apple.UserNotificationCenter") ||
        !strcmp(app, "com.contextsformac.Contexts"))
        return 1;
    if (!strcmp(app, "com.pop.pop.app") && !strcmp(title, "overlay"))
        return 1;
    if ((!strcmp(app, "us.zoom.xos") || !strcmp(app, "com.1password.1password")) && !*title)
        return 1;
    return 0;
}

static int on_a_display(CGRect bounds, const struct rect* displays, size_t ndisplays)
{
    CGPoint center = CGPointMake(CGRectGetMidX(bounds), CGRectGetMidY(bounds));

    for (size_t i = 0; i < ndisplays; i++)
    {
        CGRect d = CGRectMake(displays[i].x, displays[i].y, displays[i].width, displays[i].height);
        if (CGRectContainsPoint(d, center))
            return 1;
    }
    return 0;
}

static int number(CFDictionaryRef dict, CFStringRef key, long* out)
{
    CFNumberRef n = CFDictionaryGetValue(dict, key);
    return n && CFNumberGetValue(n, kCFNumberLongType, out);
}

int list_displays(struct rect** displays, size_t* count)
{
    CGDirectDisplayID ids[MAX_DISPLAYS];
    uint32_t n = 0;

    if (CGGetActiveDisplayList(MAX_DISPLAYS, ids, &n) != kCGErrorSuccess)
    {
        fprintf(stderr, "sigils: can't list displays\n");
        return -1;
    }
    struct rect* grown = realloc(*displays, (*count + n) * sizeof **displays);
    if (!grown)
    {
        perror("sigils");
        return -1;
    }
    *displays = grown;
    for (uint32_t i = 0; i < n; i++)
    {
        CGRect b = CGDisplayBounds(ids[i]);
        grown[(*count)++] = (struct rect){
            (int)b.origin.x,
            (int)b.origin.y,
            (int)b.size.width,
            (int)b.size.height,
        };
    }
    return 0;
}

int list_windows(struct window** windows, size_t* count)
{
    struct rect* displays = NULL;
    size_t ndisplays = 0;
    struct app_titles* apps = NULL;
    size_t napps = 0;
    int result = -1;

    if (list_displays(&displays, &ndisplays) < 0)
        return -1;

    CFArrayRef info = CGWindowListCopyWindowInfo(
        kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements, kCGNullWindowID);
    if (!info)
    {
        fprintf(stderr, "sigils: can't list windows\n");
        free(displays);
        return -1;
    }

    for (CFIndex i = 0; i < CFArrayGetCount(info); i++)
    {
        CFDictionaryRef dict = CFArrayGetValueAtIndex(info, i);
        long layer, number_, pid;
        CGRect bounds;

        if (!number(dict, kCGWindowLayer, &layer) || layer != 0)
            continue;
        if (!number(dict, kCGWindowNumber, &number_) || !number(dict, kCGWindowOwnerPID, &pid))
            continue;
        CFDictionaryRef b = CFDictionaryGetValue(dict, kCGWindowBounds);
        if (!b || !CGRectMakeWithDictionaryRepresentation(b, &bounds))
            continue;
        if (bounds.size.width < MIN_SIZE || bounds.size.height < MIN_SIZE)
            continue;
        if (!on_a_display(bounds, displays, ndisplays))
            continue;

        char* app = bundle_id_of_pid((int)pid);
        if (!app)
            app = copy_string(CFDictionaryGetValue(dict, kCGWindowOwnerName));
        CFStringRef title_ref = ax_title(&apps, &napps, (pid_t)pid, number_);
        if (!title_ref)
            title_ref = CFDictionaryGetValue(dict, kCGWindowName);
        char* title = copy_string(title_ref);
        if (!app || !title || excluded(app, title))
        {
            free(app);
            free(title);
            continue;
        }

        struct window* grown = realloc(*windows, (*count + 1) * sizeof **windows);
        if (!grown)
        {
            perror("sigils");
            free(app);
            free(title);
            goto done;
        }
        *windows = grown;
        grown[*count] = (struct window){
            .id = (unsigned long)number_,
            .x = (int)bounds.origin.x,
            .y = (int)bounds.origin.y,
            .width = (int)bounds.size.width,
            .height = (int)bounds.size.height,
            .app = app,
            .title = title,
            .z = (int)i,
        };
        (*count)++;
    }
    result = 0;
done:
    CFRelease(info);
    free(displays);
    free_app_titles(apps, napps);
    return result;
}

/* --- macOS drawing ------------------------------------------------------ */

/*
 * macOS: the daemon draws the sigils over the windows, the way the
 * WindowSigils spoon did with hs.canvas, and redraws them when windows,
 * applications, Spaces or screens change, watching what hs.window.filter
 * watches.
 *
 * The canvas windows and their drawing follow Hammerspoon's hs.canvas
 * (extensions/canvas/libcanvas.m), and the placement of the sigils follows
 * WindowSigils (Source/WindowSigils.spoon/init.lua in eraserhd/Spoons).
 * Hammerspoon is under the MIT License:
 *
 *   Copyright (c) 2014-2025 Various Contributors
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a
 *   copy of this software and associated documentation files (the
 *   "Software"), to deal in the Software without restriction, including
 *   without limitation the rights to use, copy, modify, merge, publish,
 *   distribute, sublicense, and/or sell copies of the Software, and to
 *   permit persons to whom the Software is furnished to do so, subject to
 *   the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included
 *   in all copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/* objc_msgSend, cast to the method's type. */
#define MSG(ret, ...) ((ret (*)(id, SEL, ##__VA_ARGS__))objc_msgSend)
#if defined(__x86_64__)
#define MSG_STRET(ret, ...) ((ret (*)(id, SEL, ##__VA_ARGS__))objc_msgSend_stret)
#else
#define MSG_STRET MSG
#endif

static SEL sel(const char* name)
{
    return sel_registerName(name);
}
static id cls(const char* name)
{
    return (id)objc_getClass(name);
}

extern void* objc_autoreleasePoolPush(void);
extern void objc_autoreleasePoolPop(void* pool);

extern id NSForegroundColorAttributeName;
extern id NSFontAttributeName;
extern id NSParagraphStyleAttributeName;
extern id NSWorkspaceApplicationKey;
extern id NSWorkspaceDidLaunchApplicationNotification;
extern id NSWorkspaceDidTerminateApplicationNotification;
extern id NSWorkspaceDidActivateApplicationNotification;
extern id NSWorkspaceDidHideApplicationNotification;
extern id NSWorkspaceDidUnhideApplicationNotification;
extern id NSWorkspaceActiveSpaceDidChangeNotification;
extern id NSApplicationDidChangeScreenParametersNotification;

enum
{
    NSApplicationActivationPolicyAccessory = 1,
    NSApplicationActivationPolicyProhibited = 2,
    NSWindowStyleMaskBorderless = 0,
    NSBackingStoreBuffered = 2,
    NSWindowAnimationBehaviorNone = 2,
    NSLineBreakByTruncatingTail = 4,
    NSTextAlignmentNatural = 4,
};

/* From WindowSigils. */
#define SIGIL_WIDTH 20
#define SIGIL_HEIGHT 19
#define SIGIL_MARGIN 5

struct box
{
    double x, y;
    unsigned long id;
    int focus_order;
    int top;
    char sigil;
};

struct canvas
{
    id window;
    id view;
    double x, y; /* the top left of the screen's frame */
};

static const struct options* drawing_options;
static struct box* boxes;
static size_t nboxes;
static struct canvas* canvases;
static size_t ncanvases;
static id fill_color, stroke_color, text_attributes;

static int by_box_position(const void* a, const void* b)
{
    const struct box *l = a, *r = b;

    if (l->x != r->x)
        return l->x < r->x ? -1 : 1;
    if (l->y != r->y)
        return l->y < r->y ? -1 : 1;
    return (l->id > r->id) - (l->id < r->id);
}

static int by_box_position_then_sigil(const void* a, const void* b)
{
    const struct box *l = a, *r = b;

    if (l->x != r->x)
        return l->x < r->x ? -1 : 1;
    if (l->y != r->y)
        return l->y < r->y ? -1 : 1;
    return (l->sigil > r->sigil) - (l->sigil < r->sigil);
}

static int overlapping(const struct box* a, const struct box* b)
{
    if (b->y >= a->y + SIGIL_HEIGHT)
        return 0;
    if (a->y >= b->y + SIGIL_HEIGHT)
        return 0;
    if (b->x >= a->x + SIGIL_WIDTH + SIGIL_MARGIN)
        return 0;
    if (a->x >= b->x + SIGIL_WIDTH + SIGIL_MARGIN)
        return 0;
    return 1;
}

/* Of the boxes in [start, end), which share a position, the most recently focused is on top. */
static void set_top(size_t start, size_t end)
{
    size_t top = start;

    for (size_t i = start + 1; i < end; i++)
        if (boxes[i].focus_order < boxes[top].focus_order)
            top = i;
    boxes[top].top = 1;
}

/* WindowSigils' _makeSigilBoxes. */
static void make_boxes(void)
{
    struct window* windows;
    size_t count;

    nboxes = 0;
    if (sigil_windows(drawing_options, &windows, &count, stderr) < 0)
        return;
    struct box* grown = realloc(boxes, (count + 1) * sizeof *boxes);
    if (!grown)
    {
        free_windows(windows, count);
        return;
    }
    boxes = grown;
    for (size_t i = 0; i < count && windows[i].sigil; i++)
    {
        boxes[nboxes++] = (struct box){
            .x = windows[i].x + 70,
            .y = windows[i].y + 1,
            .id = windows[i].id,
            /* The window server lists windows front to back, so most recently focused first. */
            .focus_order = windows[i].id ? windows[i].z : INT_MAX,
            .sigil = windows[i].sigil,
        };
    }
    free_windows(windows, count);

    qsort(boxes, nboxes, sizeof *boxes, by_box_position);
    size_t start = 0;
    for (size_t end = 1; end < nboxes; end++)
    {
        if (boxes[start].x != boxes[end].x || boxes[start].y != boxes[end].y)
        {
            set_top(start, end);
            start = end;
        }
    }
    if (nboxes)
        set_top(start, nboxes);

    /* move_overlapping_boxes */
    qsort(boxes, nboxes, sizeof *boxes, by_box_position_then_sigil);
    for (size_t i = 0; i < nboxes; i++)
        for (size_t j = i + 1; j < nboxes && overlapping(&boxes[i], &boxes[j]); j++)
            boxes[j].x = boxes[i].x + SIGIL_WIDTH + SIGIL_MARGIN;
}

static id make_string(const char* s)
{
    return (id)CFStringCreateWithCString(NULL, s, kCFStringEncodingUTF8);
}

/* The rectangle and text elements WindowSigils gave hs.canvas for a sigil, drawn as hs.canvas draws them. */
static void draw_box(const struct box* b, const struct canvas* c)
{
    CGRect frame = { { b->x - c->x, b->y - c->y }, { SIGIL_WIDTH, SIGIL_HEIGHT } };
    id path = MSG(id)(cls("NSBezierPath"), sel("bezierPath"));

    MSG(void, CGRect, CGFloat,
        CGFloat)(path, sel("appendBezierPathWithRoundedRect:xRadius:yRadius:"), frame, 0, 0);
    MSG(void)(fill_color, sel("setFill"));
    MSG(void)(path, sel("fill"));
    if (b->top)
    {
        MSG(void)(stroke_color, sel("setStroke"));
        MSG(void)(path, sel("stroke"));
    }

    char text[2] = { b->sigil, '\0' };
    id string = make_string(text);
    CGRect text_frame = { { frame.origin.x + 3, frame.origin.y - 4 },
                          { SIGIL_WIDTH - 3, SIGIL_HEIGHT + 7 } };
    MSG(void, CGRect, id)(string, sel("drawInRect:withAttributes:"), text_frame, text_attributes);
    CFRelease((CFTypeRef)string);
}

static void draw_rect(id self, SEL cmd, CGRect dirty)
{
    (void)cmd;
    (void)dirty;
    for (size_t i = 0; i < ncanvases; i++)
    {
        if (canvases[i].view != self)
            continue;
        MSG(void, CGFloat)(cls("NSBezierPath"), sel("setDefaultLineWidth:"), 1.0);
        for (size_t j = 0; j < nboxes; j++)
            draw_box(&boxes[j], &canvases[i]);
    }
}

static BOOL is_flipped(id self, SEL cmd)
{
    (void)self;
    (void)cmd;
    return YES;
}

static id view_class(void)
{
    static Class view;

    if (!view)
    {
        view = objc_allocateClassPair(objc_getClass("NSView"), "SigilsCanvasView", 0);
        class_addMethod(view, sel("drawRect:"), (IMP)draw_rect,
                        "v@:{CGRect={CGPoint=dd}{CGSize=dd}}");
        class_addMethod(view, sel("isFlipped"), (IMP)is_flipped, "B@:");
        objc_registerClassPair(view);
    }
    return (id)view;
}

static void make_drawing_attributes(void)
{
    fill_color = MSG(id, CGFloat, CGFloat, CGFloat, CGFloat)(
        cls("NSColor"), sel("colorWithCalibratedRed:green:blue:alpha:"), 0.0, 1.0, 1.0, 0.3);
    MSG(id)(fill_color, sel("retain"));
    stroke_color =
        MSG(id, CGFloat, CGFloat)(cls("NSColor"), sel("colorWithCalibratedWhite:alpha:"), 1.0, 1.0);
    MSG(id)(stroke_color, sel("retain"));

    id text_color =
        MSG(id, CGFloat, CGFloat)(cls("NSColor"), sel("colorWithCalibratedWhite:alpha:"), 1.0, 1.0);
    id font_name = make_string("Menlo Regular");
    id font = MSG(id, id, CGFloat)(cls("NSFont"), sel("fontWithName:size:"), font_name, 18.0);
    CFRelease((CFTypeRef)font_name);
    if (!font)
        font = MSG(id, CGFloat)(cls("NSFont"), sel("userFixedPitchFontOfSize:"), 18.0);
    id style =
        MSG(id)(MSG(id)(cls("NSParagraphStyle"), sel("defaultParagraphStyle")), sel("mutableCopy"));
    MSG(void, long)(style, sel("setAlignment:"), NSTextAlignmentNatural);
    MSG(void, unsigned long)(style, sel("setLineBreakMode:"), NSLineBreakByTruncatingTail);

    const void* keys[] = { NSForegroundColorAttributeName, NSFontAttributeName,
                           NSParagraphStyleAttributeName };
    const void* values[] = { text_color, font, style };
    text_attributes = (id)CFDictionaryCreate(NULL, keys, values, 3, &kCFTypeDictionaryKeyCallBacks,
                                             &kCFTypeDictionaryValueCallBacks);
    MSG(void)(style, sel("release"));
}

static void close_canvases(void)
{
    for (size_t i = 0; i < ncanvases; i++)
    {
        MSG(void, id)(canvases[i].window, sel("orderOut:"), nil);
        MSG(void)(canvases[i].window, sel("release"));
        MSG(void)(canvases[i].view, sel("release"));
    }
    free(canvases);
    canvases = NULL;
    ncanvases = 0;
}

/* WindowSigils' canvas for each screen, configured as hs.canvas configures its windows. */
static void make_canvases(void)
{
    close_canvases();

    id screens = MSG(id)(cls("NSScreen"), sel("screens"));
    size_t count = MSG(unsigned long)(screens, sel("count"));
    if (count == 0 || !(canvases = calloc(count, sizeof *canvases)))
        return;
    id primary = MSG(id, unsigned long)(screens, sel("objectAtIndex:"), 0);
    CGFloat primary_height = MSG_STRET(CGRect)(primary, sel("frame")).size.height;

    for (size_t i = 0; i < count; i++)
    {
        id screen = MSG(id, unsigned long)(screens, sel("objectAtIndex:"), i);
        CGRect frame = MSG_STRET(CGRect)(screen, sel("visibleFrame"));
        id window = MSG(id)(cls("NSWindow"), sel("alloc"));

        window = MSG(id, CGRect, unsigned long, unsigned long,
                     BOOL)(window, sel("initWithContentRect:styleMask:backing:defer:"), frame,
                           NSWindowStyleMaskBorderless, NSBackingStoreBuffered, YES);
        MSG(void, BOOL)(window, sel("setReleasedWhenClosed:"), NO);
        MSG(void, id)(window, sel("setBackgroundColor:"),
                      MSG(id)(cls("NSColor"), sel("clearColor")));
        MSG(void, BOOL)(window, sel("setOpaque:"), NO);
        MSG(void, BOOL)(window, sel("setHasShadow:"), NO);
        MSG(void, BOOL)(window, sel("setIgnoresMouseEvents:"), YES);
        MSG(void, BOOL)(window, sel("setRestorable:"), NO);
        MSG(void, BOOL)(window, sel("setHidesOnDeactivate:"), NO);
        MSG(void, long)(window, sel("setAnimationBehavior:"), NSWindowAnimationBehaviorNone);
        MSG(void, long)(window, sel("setLevel:"),
                        CGWindowLevelForKey(kCGScreenSaverWindowLevelKey));

        CGRect bounds = MSG_STRET(CGRect)(MSG(id)(window, sel("contentView")), sel("bounds"));
        id view =
            MSG(id, CGRect)(MSG(id)(view_class(), sel("alloc")), sel("initWithFrame:"), bounds);
        MSG(void, id)(window, sel("setContentView:"), view);
        MSG(void)(window, sel("orderFrontRegardless"));

        canvases[ncanvases++] = (struct canvas){
            .window = window,
            .view = view,
            .x = frame.origin.x,
            .y = primary_height - frame.origin.y - frame.size.height,
        };
    }
}

static void refresh(void)
{
    void* pool = objc_autoreleasePoolPush();

    make_boxes();
    for (size_t i = 0; i < ncanvases; i++)
        MSG(void, BOOL)(canvases[i].view, sel("setNeedsDisplay:"), YES);
    objc_autoreleasePoolPop(pool);
}

/* Windows change in bursts, so all the changes in one turn of the run loop are drawn once. */
static void schedule_refresh(void)
{
    static int pending;

    if (pending)
        return;
    pending = 1;
    dispatch_async(dispatch_get_main_queue(), ^{
        pending = 0;
        refresh();
    });
}

/* What hs.window.filter watches, through hs.uielement.watcher. */
static CFStringRef const app_notifications[] = {
    CFSTR("AXWindowCreated"),
    CFSTR("AXFocusedWindowChanged"),
};
static CFStringRef const window_notifications[] = {
    CFSTR("AXUIElementDestroyed"), CFSTR("AXWindowMoved"),          CFSTR("AXWindowResized"),
    CFSTR("AXWindowMiniaturized"), CFSTR("AXWindowDeminiaturized"), CFSTR("AXTitleChanged"),
};

struct watched_app
{
    pid_t pid;
    AXObserverRef observer;
    AXUIElementRef element;
};

static struct watched_app* watched;
static size_t nwatched;

static void watch_window(AXObserverRef observer, AXUIElementRef window)
{
    for (size_t i = 0; i < sizeof window_notifications / sizeof *window_notifications; i++)
        AXObserverAddNotification(observer, window, window_notifications[i], NULL);
}

static void observed(AXObserverRef observer, AXUIElementRef element, CFStringRef notification,
                     void* refcon)
{
    (void)refcon;
    if (CFEqual(notification, CFSTR("AXWindowCreated")))
        watch_window(observer, element);
    schedule_refresh();
}

static void watch_app(pid_t pid)
{
    AXObserverRef observer;

    if (pid == getpid())
        return;
    for (size_t i = 0; i < nwatched; i++)
        if (watched[i].pid == pid)
            return;
    if (AXObserverCreate(pid, observed, &observer) != kAXErrorSuccess)
        return;
    struct watched_app* grown = realloc(watched, (nwatched + 1) * sizeof *watched);
    if (!grown)
    {
        CFRelease(observer);
        return;
    }
    watched = grown;
    AXUIElementRef element = AXUIElementCreateApplication(pid);
    watched[nwatched++] = (struct watched_app){ pid, observer, element };

    for (size_t i = 0; i < sizeof app_notifications / sizeof *app_notifications; i++)
        AXObserverAddNotification(observer, element, app_notifications[i], NULL);
    CFArrayRef windows;
    if (AXUIElementCopyAttributeValues(element, kAXWindowsAttribute, 0, 100, &windows) ==
        kAXErrorSuccess)
    {
        for (CFIndex i = 0; i < CFArrayGetCount(windows); i++)
            watch_window(observer, CFArrayGetValueAtIndex(windows, i));
        CFRelease(windows);
    }
    CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer),
                       kCFRunLoopDefaultMode);
}

static void unwatch_app(pid_t pid)
{
    for (size_t i = 0; i < nwatched; i++)
    {
        if (watched[i].pid != pid)
            continue;
        CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(watched[i].observer),
                              kCFRunLoopDefaultMode);
        CFRelease(watched[i].observer);
        CFRelease(watched[i].element);
        watched[i] = watched[--nwatched];
        return;
    }
}

static id notification_app(id notification)
{
    id info = MSG(id)(notification, sel("userInfo"));
    return MSG(id, id)(info, sel("objectForKey:"), NSWorkspaceApplicationKey);
}

static pid_t app_pid(id app)
{
    return app ? MSG(pid_t)(app, sel("processIdentifier")) : -1;
}

/* Like hs.window.filter, leave out applications which can't have windows. */
static void watch_running_app(id app)
{
    if (app && MSG(long)(app, sel("activationPolicy")) != NSApplicationActivationPolicyProhibited)
        watch_app(app_pid(app));
}

static void observe(id center, id name, void (^block)(id notification))
{
    MSG(id, id, id, id, id)(center, sel("addObserverForName:object:queue:usingBlock:"), name, nil,
                            MSG(id)(cls("NSOperationQueue"), sel("mainQueue")), (id)block);
}

/* What hs.window.filter watches, through hs.application.watcher, hs.spaces.watcher and hs.screen.watcher. */
static void watch_workspace(void)
{
    id workspace = MSG(id)(cls("NSWorkspace"), sel("sharedWorkspace"));
    id center = MSG(id)(workspace, sel("notificationCenter"));

    observe(center, NSWorkspaceDidLaunchApplicationNotification, ^(id note) {
        watch_running_app(notification_app(note));
        schedule_refresh();
    });
    observe(center, NSWorkspaceDidTerminateApplicationNotification, ^(id note) {
        unwatch_app(app_pid(notification_app(note)));
        schedule_refresh();
    });
    observe(center, NSWorkspaceDidActivateApplicationNotification, ^(id note) {
        (void)note;
        schedule_refresh();
    });
    observe(center, NSWorkspaceDidHideApplicationNotification, ^(id note) {
        (void)note;
        schedule_refresh();
    });
    observe(center, NSWorkspaceDidUnhideApplicationNotification, ^(id note) {
        (void)note;
        schedule_refresh();
    });
    observe(center, NSWorkspaceActiveSpaceDidChangeNotification, ^(id note) {
        (void)note;
        schedule_refresh();
    });
    observe(MSG(id)(cls("NSNotificationCenter"), sel("defaultCenter")),
            NSApplicationDidChangeScreenParametersNotification, ^(id note) {
                (void)note;
                make_canvases();
                schedule_refresh();
            });

    id running = MSG(id)(workspace, sel("runningApplications"));
    for (unsigned long i = 0, n = MSG(unsigned long)(running, sel("count")); i < n; i++)
    {
        watch_running_app(MSG(id, unsigned long)(running, sel("objectAtIndex:"), i));
    }
}

static void client_waiting(CFFileDescriptorRef ref, CFOptionFlags flags, void* info)
{
    int listen_fd = CFFileDescriptorGetNativeDescriptor(ref);
    int fd;

    (void)flags;
    (void)info;
    while ((fd = accept(listen_fd, NULL, NULL)) >= 0)
    {
        void* pool = objc_autoreleasePoolPush();
        fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) & ~O_NONBLOCK);
        handle_client(fd, drawing_options);
        close(fd);
        objc_autoreleasePoolPop(pool);
    }
    CFFileDescriptorEnableCallBacks(ref, kCFFileDescriptorReadCallBack);
}

int serve(int listen_fd, const struct options* options)
{
    drawing_options = options;
    fcntl(listen_fd, F_SETFL, fcntl(listen_fd, F_GETFL) | O_NONBLOCK);

    id app = MSG(id)(cls("NSApplication"), sel("sharedApplication"));
    MSG(BOOL, long)(app, sel("setActivationPolicy:"), NSApplicationActivationPolicyAccessory);

    CFFileDescriptorRef ref = CFFileDescriptorCreate(NULL, listen_fd, false, client_waiting, NULL);
    CFRunLoopSourceRef source = CFFileDescriptorCreateRunLoopSource(NULL, ref, 0);
    CFRunLoopAddSource(CFRunLoopGetMain(), source, kCFRunLoopCommonModes);
    CFFileDescriptorEnableCallBacks(ref, kCFFileDescriptorReadCallBack);

    make_drawing_attributes();
    make_canvases();
    watch_workspace();
    refresh();

    MSG(void)(app, sel("run"));
    return 0;
}

#else

/* --- X windows ---------------------------------------------------------- */

/* X: the client windows which the window manager lists and which are mapped. */

static char* window_title(Display* dpy, Window win, Atom net_wm_name, Atom utf8)
{
    Atom type;
    int format;
    unsigned long n, after;
    unsigned char* data = NULL;
    char* title = NULL;

    if (XGetWindowProperty(dpy, win, net_wm_name, 0, 1024, False, utf8, &type, &format, &n, &after,
                           &data) == Success &&
        data && n)
    {
        title = strdup((char*)data);
    }
    if (data)
        XFree(data);
    if (title)
        return title;

    char* name = NULL;
    if (XFetchName(dpy, win, &name) && name)
    {
        title = strdup(name);
        XFree(name);
        return title;
    }
    return strdup("");
}

int list_displays(struct rect** displays, size_t* count)
{
    Display* dpy = XOpenDisplay(NULL);
    if (!dpy)
    {
        fprintf(stderr, "sigils: can't open the display\n");
        return -1;
    }
    struct rect* grown = realloc(*displays, (*count + 1) * sizeof **displays);
    if (!grown)
    {
        perror("sigils");
        XCloseDisplay(dpy);
        return -1;
    }
    *displays = grown;
    int screen = DefaultScreen(dpy);
    grown[(*count)++] = (struct rect){
        0,
        0,
        DisplayWidth(dpy, screen),
        DisplayHeight(dpy, screen),
    };
    XCloseDisplay(dpy);
    return 0;
}

int list_windows(struct window** windows, size_t* count)
{
    Display* dpy = XOpenDisplay(NULL);
    if (!dpy)
    {
        fprintf(stderr, "sigils: can't open the display\n");
        return -1;
    }
    Window root = DefaultRootWindow(dpy);
    Atom client_list = XInternAtom(dpy, "_NET_CLIENT_LIST", False);
    Atom net_wm_name = XInternAtom(dpy, "_NET_WM_NAME", False);
    Atom utf8 = XInternAtom(dpy, "UTF8_STRING", False);

    Atom type;
    int format;
    unsigned long n = 0, after;
    unsigned char* data = NULL;
    if (XGetWindowProperty(dpy, root, client_list, 0, 65536, False, XA_WINDOW, &type, &format, &n,
                           &after, &data) != Success ||
        !data)
    {
        fprintf(stderr, "sigils: the window manager doesn't list its windows\n");
        XCloseDisplay(dpy);
        return -1;
    }
    Window* wins = (Window*)data;

    for (unsigned long i = 0; i < n; i++)
    {
        XWindowAttributes attr;
        int x, y;
        Window child;

        if (!XGetWindowAttributes(dpy, wins[i], &attr) || attr.map_state != IsViewable)
            continue;
        if (attr.width < MIN_SIZE || attr.height < MIN_SIZE)
            continue;
        XTranslateCoordinates(dpy, wins[i], root, 0, 0, &x, &y, &child);

        XClassHint hint = { 0 };
        char* app = NULL;
        if (XGetClassHint(dpy, wins[i], &hint))
        {
            app = strdup(hint.res_class ? hint.res_class : "");
            if (hint.res_name)
                XFree(hint.res_name);
            if (hint.res_class)
                XFree(hint.res_class);
        }
        else
        {
            app = strdup("");
        }
        char* title = window_title(dpy, wins[i], net_wm_name, utf8);
        if (!app || !title)
        {
            free(app);
            free(title);
            continue;
        }

        struct window* grown = realloc(*windows, (*count + 1) * sizeof **windows);
        if (!grown)
        {
            perror("sigils");
            XFree(data);
            XCloseDisplay(dpy);
            return -1;
        }
        *windows = grown;
        grown[*count] = (struct window){
            .id = wins[i],
            .x = x,
            .y = y,
            .width = attr.width,
            .height = attr.height,
            .app = app,
            .title = title,
        };
        (*count)++;
    }
    XFree(data);
    XCloseDisplay(dpy);
    return 0;
}

int serve(int listen_fd, const struct options* defaults)
{
    return serve_clients(listen_fd, defaults);
}

#endif

/* --- main --------------------------------------------------------------- */

int main(int argc, char** argv)
{
    struct options options;
    int status = parse_options(argc, argv, &options, stderr);

    if (status)
        return status;
    if (options.help)
    {
        usage(stdout);
        return 0;
    }
    if (options.daemon)
        return run_daemon(&options);
    return ask_daemon(&options);
}
