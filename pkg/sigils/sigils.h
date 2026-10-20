#ifndef SIGILS_H
#define SIGILS_H

#include <stddef.h>
#include <stdio.h>

struct window {
	unsigned long id;    /* 0 for an empty space */
	int x, y, width, height;
	char *app;   /* bundle identifier or window class, without spaces */
	char *title;
	char sigil;
	int z;               /* front to back, among the windows */
};

struct rect {
	int x, y, width, height;
};

/* Pixels along the edges of every display which are never empty space. */
struct insets {
	int top, bottom, left, right;
};

struct options {
	const char *sigils;      /* NULL for the default */
	const char *insets;      /* as given to -i, or NULL */
	struct insets reserved;
	const char *sigil;       /* the one to look up, or NULL to list them all */
	int daemon;
	int replace;             /* the daemon replaces one which is running */
	int help;
};

/* Fills in *options from the arguments, or returns 2 after printing the usage to err. */
int parse_options(int argc, char **argv, struct options *options, FILE *err);

/*
 * Lists the windows and empty spaces in order of position, with their sigils,
 * and returns 0, or returns -1 after printing why not to err.
 */
int sigil_windows(const struct options *options, struct window **windows, size_t *count,
		  FILE *err);

void free_windows(struct window *windows, size_t count);

/* Prints what `sigils` prints for these options, and returns its exit status. */
int answer(const struct options *options, FILE *out, FILE *err);

/* Runs the daemon with these defaults for its queries, and returns its exit status. */
int run_daemon(const struct options *defaults);

/* Asks the daemon, starting it if it isn't running, and returns the exit status. */
int ask_daemon(const struct options *options);

/* Answers a client which connected to the daemon. */
void handle_client(int fd, const struct options *defaults);

/* Answers clients on the daemon's listening socket until something goes wrong. */
int serve_clients(int listen_fd, const struct options *defaults);

/* The backend's way of answering clients, which may also draw the sigils. */
int serve(int listen_fd, const struct options *defaults);

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
