/*
 * Empty spaces: the edges of the displays and windows cut the screen into a
 * grid, and runs of empty cells are grown into rectangles, first down, then
 * right, from the top left of each display.  As in hs.geometry, a rectangle
 * spans x through x + width, so it takes up width + 1 columns of the grid.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigils.h"

#define MIN_SIZE 20

struct coordinates {
	int *v;
	size_t n;
};

static int ascending(const void *a, const void *b)
{
	int l = *(const int *)a, r = *(const int *)b;
	return (l > r) - (l < r);
}

static void add_edges(struct coordinates *xs, struct coordinates *ys, struct rect r)
{
	xs->v[xs->n++] = r.x;
	xs->v[xs->n++] = r.x + r.width + 1;
	ys->v[ys->n++] = r.y;
	ys->v[ys->n++] = r.y + r.height + 1;
}

static void sort_unique(struct coordinates *c)
{
	size_t n = 0;

	qsort(c->v, c->n, sizeof c->v[0], ascending);
	for (size_t i = 0; i < c->n; i++)
		if (n == 0 || c->v[n - 1] != c->v[i])
			c->v[n++] = c->v[i];
	c->n = n;
}

static size_t offset(const struct coordinates *c, int value)
{
	const int *found = bsearch(&value, c->v, c->n, sizeof c->v[0], ascending);
	return (size_t)(found - c->v);
}

struct grid {
	struct coordinates xs, ys;
	char *occupied;
};

#define CELL(g, row, col) ((g)->occupied[(row) * (g)->xs.n + (col)])

static void occupy(struct grid *g, size_t top, size_t left, size_t bottom, size_t right)
{
	for (size_t row = top; row <= bottom; row++)
		for (size_t col = left; col <= right; col++)
			CELL(g, row, col) = 1;
}

static void occupy_rect(struct grid *g, struct rect r)
{
	occupy(g, offset(&g->ys, r.y), offset(&g->xs, r.x),
	       offset(&g->ys, r.y + r.height + 1) - 1,
	       offset(&g->xs, r.x + r.width + 1) - 1);
}

static size_t row_right_extent(struct grid *g, size_t row, size_t left, size_t right_limit)
{
	size_t right = left;

	while (right + 1 <= right_limit && !CELL(g, row, right + 1))
		right++;
	return right;
}

static void empty_extent(struct grid *g, size_t top, size_t left,
			 size_t bottom_limit, size_t right_limit,
			 size_t *bottom, size_t *right)
{
	*bottom = top;
	while (*bottom + 1 <= bottom_limit && !CELL(g, *bottom + 1, left))
		(*bottom)++;
	*right = row_right_extent(g, top, left, right_limit);
	for (size_t row = top + 1; row <= *bottom; row++) {
		size_t r = row_right_extent(g, row, left, right_limit);
		if (r < *right)
			*right = r;
	}
}

static int append_space(struct window **windows, size_t *count, struct rect r)
{
	struct window *grown = realloc(*windows, (*count + 1) * sizeof **windows);
	char *app = strdup("-"), *title = strdup("");

	if (!grown || !app || !title) {
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

static size_t reserved_strips(struct rect d, struct insets in, struct rect *strips)
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

int add_empty_spaces(struct window **windows, size_t *count,
		     const struct rect *displays, size_t ndisplays,
		     struct insets reserved)
{
	size_t nwindows = *count;
	size_t nstrips = 0;
	struct rect *strips = malloc((4 * ndisplays + 1) * sizeof *strips);
	size_t most = 2 * (nwindows + ndisplays + 4 * ndisplays) + 1;
	struct grid g = {
		.xs = { malloc(most * sizeof(int)), 0 },
		.ys = { malloc(most * sizeof(int)), 0 },
	};
	int result = -1;

	if (!strips || !g.xs.v || !g.ys.v) {
		perror("sigils");
		goto done;
	}
	for (size_t i = 0; i < ndisplays; i++) {
		add_edges(&g.xs, &g.ys, displays[i]);
		nstrips += reserved_strips(displays[i], reserved, strips + nstrips);
	}
	for (size_t i = 0; i < nwindows; i++)
		add_edges(&g.xs, &g.ys, (struct rect){ (*windows)[i].x, (*windows)[i].y,
						       (*windows)[i].width, (*windows)[i].height });
	for (size_t i = 0; i < nstrips; i++)
		add_edges(&g.xs, &g.ys, strips[i]);
	sort_unique(&g.xs);
	sort_unique(&g.ys);

	g.occupied = calloc(g.xs.n * g.ys.n + 1, 1);
	if (!g.occupied) {
		perror("sigils");
		goto done;
	}
	for (size_t i = 0; i < nwindows; i++)
		occupy_rect(&g, (struct rect){ (*windows)[i].x, (*windows)[i].y,
					       (*windows)[i].width, (*windows)[i].height });
	for (size_t i = 0; i < nstrips; i++)
		occupy_rect(&g, strips[i]);

	for (size_t i = 0; i < ndisplays; i++) {
		struct rect d = displays[i];
		size_t top = offset(&g.ys, d.y);
		size_t left = offset(&g.xs, d.x);
		size_t bottom_limit = offset(&g.ys, d.y + d.height + 1) - 1;
		size_t right_limit = offset(&g.xs, d.x + d.width + 1) - 1;

		for (size_t row = top; row <= bottom_limit; row++) {
			for (size_t col = left; col <= right_limit; col++) {
				size_t bottom, right;

				if (CELL(&g, row, col))
					continue;
				empty_extent(&g, row, col, bottom_limit, right_limit,
					     &bottom, &right);
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
