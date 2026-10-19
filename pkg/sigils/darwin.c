/*
 * macOS: windows from the window server, with their titles from the
 * accessibility API, which needs Accessibility to be allowed for whatever
 * runs this.  The window server only gives other applications' titles with
 * Screen Recording allowed, so they're the fallback.
 */
#include <ApplicationServices/ApplicationServices.h>
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <libproc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigils.h"

#define MAX_DISPLAYS 16
#define MIN_SIZE 20

static char *copy_string(CFStringRef s)
{
	if (!s)
		return strdup("");
	CFIndex size = CFStringGetMaximumSizeForEncoding(
		CFStringGetLength(s), kCFStringEncodingUTF8) + 1;
	char *out = malloc(size);
	if (!out)
		return NULL;
	if (!CFStringGetCString(s, out, size, kCFStringEncodingUTF8))
		out[0] = '\0';
	return out;
}

static char *bundle_id_of_pid(int pid)
{
	char path[PROC_PIDPATHINFO_MAXSIZE];

	if (proc_pidpath(pid, path, sizeof path) <= 0)
		return NULL;
	char *app = strstr(path, ".app/");
	if (!app)
		return NULL;
	app[4] = '\0';

	CFURLRef url = CFURLCreateFromFileSystemRepresentation(
		NULL, (const UInt8 *)path, strlen(path), true);
	if (!url)
		return NULL;
	CFBundleRef bundle = CFBundleCreate(NULL, url);
	CFRelease(url);
	if (!bundle)
		return NULL;
	char *id = NULL;
	CFStringRef ident = CFBundleGetIdentifier(bundle);
	if (ident)
		id = copy_string(ident);
	CFRelease(bundle);
	return id;
}

/* Private, but how Hammerspoon matches accessibility windows to the window server's. */
extern AXError _AXUIElementGetWindow(AXUIElementRef element, CGWindowID *id);

struct app_titles {
	pid_t pid;
	CFDictionaryRef titles;   /* window number -> title */
};

static CFDictionaryRef copy_app_titles(pid_t pid)
{
	CFMutableDictionaryRef titles = CFDictionaryCreateMutable(
		NULL, 0, &kCFTypeDictionaryKeyCallBacks, &kCFTypeDictionaryValueCallBacks);
	AXUIElementRef app = AXUIElementCreateApplication(pid);
	CFArrayRef windows = NULL;

	if (app && AXUIElementCopyAttributeValues(app, kAXWindowsAttribute, 0, 100,
						   &windows) == kAXErrorSuccess) {
		for (CFIndex i = 0; i < CFArrayGetCount(windows); i++) {
			AXUIElementRef window = CFArrayGetValueAtIndex(windows, i);
			CGWindowID id;
			CFTypeRef title = NULL;

			if (_AXUIElementGetWindow(window, &id) != kAXErrorSuccess)
				continue;
			if (AXUIElementCopyAttributeValue(window, kAXTitleAttribute, &title) ==
				    kAXErrorSuccess &&
			    CFGetTypeID(title) == CFStringGetTypeID()) {
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
static CFStringRef ax_title(struct app_titles **apps, size_t *napps, pid_t pid, long number)
{
	struct app_titles *app = NULL;

	for (size_t i = 0; i < *napps; i++)
		if ((*apps)[i].pid == pid)
			app = &(*apps)[i];
	if (!app) {
		struct app_titles *grown = realloc(*apps, (*napps + 1) * sizeof **apps);
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

static void free_app_titles(struct app_titles *apps, size_t napps)
{
	for (size_t i = 0; i < napps; i++)
		CFRelease(apps[i].titles);
	free(apps);
}

static int excluded(const char *app, const char *title)
{
	/* The same ones WindowSigils leaves out. */
	if (!strcmp(app, "com.apple.notificationcenterui") ||
	    !strcmp(app, "com.apple.UserNotificationCenter") ||
	    !strcmp(app, "com.contextsformac.Contexts"))
		return 1;
	if (!strcmp(app, "com.pop.pop.app") && !strcmp(title, "overlay"))
		return 1;
	if ((!strcmp(app, "us.zoom.xos") ||
	     !strcmp(app, "com.1password.1password")) && !*title)
		return 1;
	return 0;
}

static int on_a_display(CGRect bounds, const struct rect *displays, size_t ndisplays)
{
	CGPoint center = CGPointMake(CGRectGetMidX(bounds), CGRectGetMidY(bounds));

	for (size_t i = 0; i < ndisplays; i++) {
		CGRect d = CGRectMake(displays[i].x, displays[i].y,
				      displays[i].width, displays[i].height);
		if (CGRectContainsPoint(d, center))
			return 1;
	}
	return 0;
}

static int number(CFDictionaryRef dict, CFStringRef key, long *out)
{
	CFNumberRef n = CFDictionaryGetValue(dict, key);
	return n && CFNumberGetValue(n, kCFNumberLongType, out);
}

int list_displays(struct rect **displays, size_t *count)
{
	CGDirectDisplayID ids[MAX_DISPLAYS];
	uint32_t n = 0;

	if (CGGetActiveDisplayList(MAX_DISPLAYS, ids, &n) != kCGErrorSuccess) {
		fprintf(stderr, "sigils: can't list displays\n");
		return -1;
	}
	struct rect *grown = realloc(*displays, (*count + n) * sizeof **displays);
	if (!grown) {
		perror("sigils");
		return -1;
	}
	*displays = grown;
	for (uint32_t i = 0; i < n; i++) {
		CGRect b = CGDisplayBounds(ids[i]);
		grown[(*count)++] = (struct rect){
			(int)b.origin.x, (int)b.origin.y,
			(int)b.size.width, (int)b.size.height,
		};
	}
	return 0;
}

int list_windows(struct window **windows, size_t *count)
{
	struct rect *displays = NULL;
	size_t ndisplays = 0;
	struct app_titles *apps = NULL;
	size_t napps = 0;
	int result = -1;

	if (list_displays(&displays, &ndisplays) < 0)
		return -1;

	CFArrayRef info = CGWindowListCopyWindowInfo(
		kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements,
		kCGNullWindowID);
	if (!info) {
		fprintf(stderr, "sigils: can't list windows\n");
		free(displays);
		return -1;
	}

	for (CFIndex i = 0; i < CFArrayGetCount(info); i++) {
		CFDictionaryRef dict = CFArrayGetValueAtIndex(info, i);
		long layer, number_, pid;
		CGRect bounds;

		if (!number(dict, kCGWindowLayer, &layer) || layer != 0)
			continue;
		if (!number(dict, kCGWindowNumber, &number_) ||
		    !number(dict, kCGWindowOwnerPID, &pid))
			continue;
		CFDictionaryRef b = CFDictionaryGetValue(dict, kCGWindowBounds);
		if (!b || !CGRectMakeWithDictionaryRepresentation(b, &bounds))
			continue;
		if (bounds.size.width < MIN_SIZE || bounds.size.height < MIN_SIZE)
			continue;
		if (!on_a_display(bounds, displays, ndisplays))
			continue;

		char *app = bundle_id_of_pid((int)pid);
		if (!app)
			app = copy_string(CFDictionaryGetValue(dict, kCGWindowOwnerName));
		CFStringRef title_ref = ax_title(&apps, &napps, (pid_t)pid, number_);
		if (!title_ref)
			title_ref = CFDictionaryGetValue(dict, kCGWindowName);
		char *title = copy_string(title_ref);
		if (!app || !title || excluded(app, title)) {
			free(app);
			free(title);
			continue;
		}

		struct window *grown =
			realloc(*windows, (*count + 1) * sizeof **windows);
		if (!grown) {
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

int serve_and_draw(int listen_fd, const struct options *defaults);

int serve(int listen_fd, const struct options *defaults)
{
	return serve_and_draw(listen_fd, defaults);
}
