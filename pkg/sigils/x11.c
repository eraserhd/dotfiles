/* X: the client windows which the window manager lists and which are mapped. */
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigils.h"

#define MIN_SIZE 20

static char *window_title(Display *dpy, Window win, Atom net_wm_name, Atom utf8)
{
	Atom type;
	int format;
	unsigned long n, after;
	unsigned char *data = NULL;
	char *title = NULL;

	if (XGetWindowProperty(dpy, win, net_wm_name, 0, 1024, False, utf8, &type,
			       &format, &n, &after, &data) == Success && data && n) {
		title = strdup((char *)data);
	}
	if (data)
		XFree(data);
	if (title)
		return title;

	char *name = NULL;
	if (XFetchName(dpy, win, &name) && name) {
		title = strdup(name);
		XFree(name);
		return title;
	}
	return strdup("");
}

int list_windows(struct window **windows, size_t *count)
{
	Display *dpy = XOpenDisplay(NULL);
	if (!dpy) {
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
	unsigned char *data = NULL;
	if (XGetWindowProperty(dpy, root, client_list, 0, 65536, False, XA_WINDOW,
			       &type, &format, &n, &after, &data) != Success || !data) {
		fprintf(stderr, "sigils: the window manager doesn't list its windows\n");
		XCloseDisplay(dpy);
		return -1;
	}
	Window *wins = (Window *)data;

	for (unsigned long i = 0; i < n; i++) {
		XWindowAttributes attr;
		int x, y;
		Window child;

		if (!XGetWindowAttributes(dpy, wins[i], &attr) ||
		    attr.map_state != IsViewable)
			continue;
		if (attr.width < MIN_SIZE || attr.height < MIN_SIZE)
			continue;
		XTranslateCoordinates(dpy, wins[i], root, 0, 0, &x, &y, &child);

		XClassHint hint = {0};
		char *app = NULL;
		if (XGetClassHint(dpy, wins[i], &hint)) {
			app = strdup(hint.res_class ? hint.res_class : "");
			if (hint.res_name)
				XFree(hint.res_name);
			if (hint.res_class)
				XFree(hint.res_class);
		} else {
			app = strdup("");
		}
		char *title = window_title(dpy, wins[i], net_wm_name, utf8);
		if (!app || !title) {
			free(app);
			free(title);
			continue;
		}

		struct window *grown =
			realloc(*windows, (*count + 1) * sizeof **windows);
		if (!grown) {
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
