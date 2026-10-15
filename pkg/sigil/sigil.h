#ifndef SIGIL_H
#define SIGIL_H

#include <stddef.h>

struct window {
	unsigned long id;
	int x, y, width, height;
	char *app;   /* bundle identifier or window class, without spaces */
	char *title;
	char sigil;
};

/*
 * Appends the windows which are visible on a display to *windows, which is
 * grown with realloc, and returns 0, or returns -1 after printing why not.
 * Windows are not sorted or given sigils.
 */
int list_windows(struct window **windows, size_t *count);

#endif
