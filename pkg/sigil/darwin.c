/*
 * macOS: windows from the window server.  Their titles, which are only given
 * for other applications' windows once Screen Recording is allowed for
 * whatever is running this, come out empty until then.
 */
#include <CoreFoundation/CoreFoundation.h>
#include <CoreGraphics/CoreGraphics.h>
#include <libproc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sigil.h"

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

static int on_a_display(CGRect bounds, const CGRect *displays, uint32_t ndisplays)
{
	CGPoint center = CGPointMake(CGRectGetMidX(bounds), CGRectGetMidY(bounds));

	for (uint32_t i = 0; i < ndisplays; i++)
		if (CGRectContainsPoint(displays[i], center))
			return 1;
	return 0;
}

static int number(CFDictionaryRef dict, CFStringRef key, long *out)
{
	CFNumberRef n = CFDictionaryGetValue(dict, key);
	return n && CFNumberGetValue(n, kCFNumberLongType, out);
}

int list_windows(struct window **windows, size_t *count)
{
	CGDirectDisplayID ids[MAX_DISPLAYS];
	CGRect displays[MAX_DISPLAYS];
	uint32_t ndisplays = 0;

	if (CGGetActiveDisplayList(MAX_DISPLAYS, ids, &ndisplays) != kCGErrorSuccess) {
		fprintf(stderr, "sigil: can't list displays\n");
		return -1;
	}
	for (uint32_t i = 0; i < ndisplays; i++)
		displays[i] = CGDisplayBounds(ids[i]);

	CFArrayRef info = CGWindowListCopyWindowInfo(
		kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements,
		kCGNullWindowID);
	if (!info) {
		fprintf(stderr, "sigil: can't list windows\n");
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
		char *title = copy_string(CFDictionaryGetValue(dict, kCGWindowName));
		if (!app || !title || excluded(app, title)) {
			free(app);
			free(title);
			continue;
		}

		struct window *grown =
			realloc(*windows, (*count + 1) * sizeof **windows);
		if (!grown) {
			perror("sigil");
			CFRelease(info);
			return -1;
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
		};
		(*count)++;
	}
	CFRelease(info);
	return 0;
}
