/*
 * sigils - list windows with a one-character name each, or find one by it.
 *
 *   sigils                sigil id x y width height app title
 *   sigils <sigil>        the line for the window with that sigil
 *
 * Empty spaces between the windows get sigils too, with id 0 and app "-".
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
	      "              the edges are top, bottom, left and right\n",
	      out);
}

static void print_clean(const char *s, int keep_spaces)
{
	for (; *s; s++) {
		unsigned char c = (unsigned char)*s;
		if (c == '\n' || c == '\r' || c == '\t')
			putchar(' ');
		else if (c == ' ' && !keep_spaces)
			putchar('_');
		else
			putchar(c);
	}
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

static void print_window(const struct window *w)
{
	printf("%c %lu %d %d %d %d ", w->sigil, w->id, w->x, w->y, w->width,
	       w->height);
	print_clean(w->app, 0);
	putchar(' ');
	print_clean(w->title, 1);
	putchar('\n');
}

int main(int argc, char **argv)
{
	const char *sigils = DEFAULT_SIGILS;
	struct insets reserved = {0};
	struct window *windows = NULL;
	size_t count = 0;
	int opt;

	for (opt = 1; opt < argc && argv[opt][0] == '-' && argv[opt][1]; opt++) {
		if (strcmp(argv[opt], "--") == 0) {
			opt++;
			break;
		} else if (strcmp(argv[opt], "-s") == 0 && opt + 1 < argc) {
			sigils = argv[++opt];
		} else if (strcmp(argv[opt], "-i") == 0 && opt + 1 < argc) {
			if (parse_insets(argv[++opt], &reserved) < 0) {
				fprintf(stderr, "sigils: bad insets %s\n", argv[opt]);
				return 2;
			}
		} else if (strcmp(argv[opt], "-h") == 0) {
			usage(stdout);
			return 0;
		} else {
			usage(stderr);
			return 2;
		}
	}
	if (argc - opt > 1 || (argc - opt == 1 && strlen(argv[opt]) != 1)) {
		usage(stderr);
		return 2;
	}

	struct rect *displays = NULL;
	size_t ndisplays = 0;
	if (list_windows(&windows, &count) < 0 ||
	    list_displays(&displays, &ndisplays) < 0 ||
	    add_empty_spaces(&windows, &count, displays, ndisplays, reserved) < 0)
		return 1;
	qsort(windows, count, sizeof windows[0], by_position);

	size_t nsigils = strlen(sigils);
	for (size_t i = 0; i < count; i++)
		windows[i].sigil = i < nsigils ? sigils[i] : '\0';

	if (argc - opt == 1) {
		for (size_t i = 0; i < count; i++) {
			if (windows[i].sigil == argv[opt][0]) {
				print_window(&windows[i]);
				return 0;
			}
		}
		fprintf(stderr, "sigils: no window has the sigil %s\n", argv[opt]);
		return 1;
	}

	for (size_t i = 0; i < count && windows[i].sigil; i++)
		print_window(&windows[i]);
	return 0;
}
