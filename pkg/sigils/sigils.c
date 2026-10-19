/*
 * sigils - list windows with a one-character name each, or find one by it.
 *
 *   sigils                sigil id x y width height app title
 *   sigils <sigil>        the line for the window with that sigil
 *
 * Empty spaces between the windows get sigils too, with id 0 and app "-".
 * The answers come from a daemon (see socket.c), so that it can be started
 * with the permissions to see the windows, and keep the sigils drawn.
 * Sigils are handed out in order of position, left to right and then top to
 * bottom, so they stay put while the windows do.
 */
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigils.h"

static const char DEFAULT_SIGILS[] = "abcdefgimnopqrstuvwxyz;,/";

static int by_position(const void *a, const void *b)
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

static void usage(FILE *out)
{
	fputs("usage: sigils [-s sigils] [-i edge=pixels,...] [sigil]\n"
	      "       sigils --daemon [-s sigils] [-i edge=pixels,...]\n"
	      "\n"
	      "Without an argument, lists the visible windows as\n"
	      "\n"
	      "  sigil id x y width height app title\n"
	      "\n"
	      "where app is the bundle identifier (macOS) or window class (X),\n"
	      "and title is the rest of the line.  Empty spaces between windows\n"
	      "are listed too, with id 0 and app -.  With a sigil, prints just its\n"
	      "line, and fails if there isn't one.\n"
	      "\n"
	      "  -s sigils   the characters to use, in order\n"
	      "              (default: abcdefgimnopqrstuvwxyz;,/)\n"
	      "  -i insets   strips along the edges of every display which are\n"
	      "              never empty space, e.g. top=40 for a status bar;\n"
	      "              the edges are top, bottom, left and right\n"
	      "\n"
	      "The windows come from a daemon, which is started if it isn't\n"
	      "running, on the socket $SIGILS_SOCKET, or else sigils.sock in\n"
	      "$XDG_RUNTIME_DIR or $TMPDIR.  With --daemon, it runs that daemon,\n"
	      "whose -s and -i are the defaults for queries which don't give them.\n"
	      "On macOS, the daemon also draws the sigils over the windows.\n",
	      out);
}

static void print_clean(FILE *out, const char *s, int keep_spaces)
{
	for (; *s; s++) {
		unsigned char c = (unsigned char)*s;
		if (c == '\n' || c == '\r' || c == '\t')
			putc(' ', out);
		else if (c == ' ' && !keep_spaces)
			putc('_', out);
		else
			putc(c, out);
	}
}

static void print_window(FILE *out, const struct window *w)
{
	fprintf(out, "%c %lu %d %d %d %d ", w->sigil, w->id, w->x, w->y, w->width,
		w->height);
	print_clean(out, w->app, 0);
	putc(' ', out);
	print_clean(out, w->title, 1);
	putc('\n', out);
}

/* Parses edge=pixels[,edge=pixels...], where edge is top, bottom, left or right. */
static int parse_insets(const char *spec, struct insets *insets)
{
	while (*spec) {
		int *edge;
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

		char *end;
		long pixels = strtol(spec, &end, 10);
		if (end == spec || pixels < 0 || (*end && *end != ','))
			return -1;
		*edge = (int)pixels;
		spec = *end ? end + 1 : end;
	}
	return 0;
}

int parse_options(int argc, char **argv, struct options *options, FILE *err)
{
	int opt;

	*options = (struct options){0};
	for (opt = 1; opt < argc && argv[opt][0] == '-' && argv[opt][1]; opt++) {
		if (strcmp(argv[opt], "--") == 0) {
			opt++;
			break;
		} else if (strcmp(argv[opt], "-s") == 0 && opt + 1 < argc) {
			options->sigils = argv[++opt];
		} else if (strcmp(argv[opt], "-i") == 0 && opt + 1 < argc) {
			options->insets = argv[++opt];
			options->reserved = (struct insets){0};
			if (parse_insets(options->insets, &options->reserved) < 0) {
				fprintf(err, "sigils: bad insets %s\n", options->insets);
				return 2;
			}
		} else if (strcmp(argv[opt], "--daemon") == 0) {
			options->daemon = 1;
		} else if (strcmp(argv[opt], "-h") == 0) {
			options->help = 1;
		} else {
			usage(err);
			return 2;
		}
	}
	if (argc - opt > 1 || (argc - opt == 1 && strlen(argv[opt]) != 1) ||
	    (options->daemon && argc - opt == 1)) {
		usage(err);
		return 2;
	}
	if (argc - opt == 1)
		options->sigil = argv[opt];
	return 0;
}

void free_windows(struct window *windows, size_t count)
{
	for (size_t i = 0; i < count; i++) {
		free(windows[i].app);
		free(windows[i].title);
	}
	free(windows);
}

int sigil_windows(const struct options *options, struct window **windows, size_t *count,
		  FILE *err)
{
	const char *sigils = options->sigils ? options->sigils : DEFAULT_SIGILS;
	struct rect *displays = NULL;
	size_t ndisplays = 0;
	int result = -1;

	*windows = NULL;
	*count = 0;
	if (list_windows(windows, count) < 0 ||
	    list_displays(&displays, &ndisplays) < 0 ||
	    add_empty_spaces(windows, count, displays, ndisplays, options->reserved) < 0)
		goto done;
	qsort(*windows, *count, sizeof (*windows)[0], by_position);

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

int answer(const struct options *options, FILE *out, FILE *err)
{
	struct window *windows;
	size_t count;
	int status = 1;

	if (sigil_windows(options, &windows, &count, err) < 0)
		goto done;

	if (options->sigil) {
		for (size_t i = 0; i < count; i++) {
			if (windows[i].sigil == options->sigil[0]) {
				print_window(out, &windows[i]);
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

int main(int argc, char **argv)
{
	struct options options;
	int status = parse_options(argc, argv, &options, stderr);

	if (status)
		return status;
	if (options.help) {
		usage(stdout);
		return 0;
	}
	if (options.daemon)
		return run_daemon(&options);
	return ask_daemon(&options);
}
