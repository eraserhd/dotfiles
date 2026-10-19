#ifndef SIGILS_H
#define SIGILS_H

#include <stddef.h>

struct window {
	unsigned long id;    /* 0 for an empty space */
	int x, y, width, height;
	char *app;   /* bundle identifier or window class, without spaces */
	char *title;
	char sigil;
};

struct rect {
	int x, y, width, height;
};

/* Pixels along the edges of every display which are never empty space. */
struct insets {
	int top, bottom, left, right;
};

/*
 * Appends the windows which are visible on a display to *windows, which is
 * grown with realloc, and returns 0, or returns -1 after printing why not.
 * Windows are not sorted or given sigils.
 */
int list_windows(struct window **windows, size_t *count);

/* Like list_windows, but for the bounds of the displays. */
int list_displays(struct rect **displays, size_t *count);

/*
 * Appends the empty spaces on the displays which no window covers, as
 * windows with id 0, the same way as list_windows.
 */
int add_empty_spaces(struct window **windows, size_t *count,
		     const struct rect *displays, size_t ndisplays,
		     struct insets reserved);

#endif
