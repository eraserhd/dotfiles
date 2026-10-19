/*
 * For tests: windows and displays from the file named by $SIGILS_FAKE_WINDOWS,
 * read again for every query, with lines like
 *
 *   display x y width height
 *   window id x y width height app title...
 */
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigils.h"

static FILE *open_fake(void)
{
	const char *path = getenv("SIGILS_FAKE_WINDOWS");
	FILE *f = path ? fopen(path, "r") : NULL;

	if (!f)
		fprintf(stderr, "sigils: can't read $SIGILS_FAKE_WINDOWS\n");
	return f;
}

int list_windows(struct window **windows, size_t *count)
{
	FILE *f = open_fake();
	char line[1024];

	if (!f)
		return -1;
	while (fgets(line, sizeof line, f)) {
		struct window w = {0};
		char app[256];
		int title = 0;

		line[strcspn(line, "\n")] = '\0';
		if (sscanf(line, "window %lu %d %d %d %d %255s %n", &w.id, &w.x, &w.y,
			   &w.width, &w.height, app, &title) < 6)
			continue;
		w.app = strdup(app);
		w.title = strdup(title ? line + title : "");
		struct window *grown = realloc(*windows, (*count + 1) * sizeof **windows);
		if (!grown || !w.app || !w.title) {
			fclose(f);
			return -1;
		}
		*windows = grown;
		grown[(*count)++] = w;
	}
	fclose(f);
	return 0;
}

int list_displays(struct rect **displays, size_t *count)
{
	FILE *f = open_fake();
	char line[1024];

	if (!f)
		return -1;
	while (fgets(line, sizeof line, f)) {
		struct rect r;

		if (sscanf(line, "display %d %d %d %d", &r.x, &r.y, &r.width, &r.height) != 4)
			continue;
		struct rect *grown = realloc(*displays, (*count + 1) * sizeof **displays);
		if (!grown) {
			fclose(f);
			return -1;
		}
		*displays = grown;
		grown[(*count)++] = r;
	}
	fclose(f);
	return 0;
}

int serve(int listen_fd, const struct options *defaults)
{
	return serve_clients(listen_fd, defaults);
}
