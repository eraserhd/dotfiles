/*
 * sigils - list windows with a one-character name each, or find one by it.
 *
 *   sigils                sigil id x y width height app title
 *   sigils <sigil>        the id of the window with that sigil
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
	fputs("usage: sigils [-s sigils] [sigil]\n"
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
	      "  -s sigils   the characters to use, in order\n"
	      "              (default: abcdefgimnopqrstuvwxyz;,/)\n",
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

int main(int argc, char **argv)
{
	const char *sigils = DEFAULT_SIGILS;
	struct window *windows = NULL;
	size_t count = 0;
	int opt;

	for (opt = 1; opt < argc && argv[opt][0] == '-' && argv[opt][1]; opt++) {
		if (strcmp(argv[opt], "--") == 0) {
			opt++;
			break;
		} else if (strcmp(argv[opt], "-s") == 0 && opt + 1 < argc) {
			sigils = argv[++opt];
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
	    add_empty_spaces(&windows, &count, displays, ndisplays, (struct insets){0}) < 0)
		return 1;
	qsort(windows, count, sizeof windows[0], by_position);

	size_t nsigils = strlen(sigils);
	for (size_t i = 0; i < count; i++)
		windows[i].sigil = i < nsigils ? sigils[i] : '\0';

	if (argc - opt == 1) {
		for (size_t i = 0; i < count; i++) {
			if (windows[i].sigil == argv[opt][0]) {
				printf("%lu\n", windows[i].id);
				return 0;
			}
		}
		fprintf(stderr, "sigils: no window has the sigil %s\n", argv[opt]);
		return 1;
	}

	for (size_t i = 0; i < count && windows[i].sigil; i++) {
		const struct window *w = &windows[i];

		printf("%c %lu %d %d %d %d ", w->sigil, w->id, w->x, w->y,
		       w->width, w->height);
		print_clean(w->app, 0);
		putchar(' ');
		print_clean(w->title, 1);
		putchar('\n');
	}
	return 0;
}
