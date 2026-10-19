/*
 * macOS: the daemon draws the sigils over the windows, the way the
 * WindowSigils spoon did with hs.canvas, and redraws them when windows,
 * applications, Spaces or screens change, watching what hs.window.filter
 * watches.
 *
 * The canvas windows and their drawing follow Hammerspoon's hs.canvas
 * (extensions/canvas/libcanvas.m), and the placement of the sigils follows
 * WindowSigils (Source/WindowSigils.spoon/init.lua in eraserhd/Spoons).
 * Hammerspoon is under the MIT License:
 *
 *   Copyright (c) 2014-2025 Various Contributors
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a
 *   copy of this software and associated documentation files (the
 *   "Software"), to deal in the Software without restriction, including
 *   without limitation the rights to use, copy, modify, merge, publish,
 *   distribute, sublicense, and/or sell copies of the Software, and to
 *   permit persons to whom the Software is furnished to do so, subject to
 *   the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included
 *   in all copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *   CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *   TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
#include <ApplicationServices/ApplicationServices.h>
#include <CoreFoundation/CoreFoundation.h>
#include <dispatch/dispatch.h>
#include <fcntl.h>
#include <limits.h>
#include <objc/message.h>
#include <objc/runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <unistd.h>

#include "sigils.h"

/* objc_msgSend, cast to the method's type. */
#define MSG(ret, ...) ((ret (*)(id, SEL, ##__VA_ARGS__))objc_msgSend)
#if defined(__x86_64__)
#define MSG_STRET(ret, ...) ((ret (*)(id, SEL, ##__VA_ARGS__))objc_msgSend_stret)
#else
#define MSG_STRET MSG
#endif

static SEL sel(const char *name) { return sel_registerName(name); }
static id cls(const char *name) { return (id)objc_getClass(name); }

extern void *objc_autoreleasePoolPush(void);
extern void objc_autoreleasePoolPop(void *pool);

extern id NSForegroundColorAttributeName;
extern id NSFontAttributeName;
extern id NSParagraphStyleAttributeName;
extern id NSWorkspaceApplicationKey;
extern id NSWorkspaceDidLaunchApplicationNotification;
extern id NSWorkspaceDidTerminateApplicationNotification;
extern id NSWorkspaceDidActivateApplicationNotification;
extern id NSWorkspaceDidHideApplicationNotification;
extern id NSWorkspaceDidUnhideApplicationNotification;
extern id NSWorkspaceActiveSpaceDidChangeNotification;
extern id NSApplicationDidChangeScreenParametersNotification;

enum {
	NSApplicationActivationPolicyAccessory = 1,
	NSApplicationActivationPolicyProhibited = 2,
	NSWindowStyleMaskBorderless = 0,
	NSBackingStoreBuffered = 2,
	NSWindowAnimationBehaviorNone = 2,
	NSLineBreakByTruncatingTail = 4,
	NSTextAlignmentNatural = 4,
};

/* From WindowSigils. */
#define SIGIL_WIDTH 20
#define SIGIL_HEIGHT 19
#define SIGIL_MARGIN 5

struct box {
	double x, y;
	unsigned long id;
	int focus_order;
	int top;
	char sigil;
};

struct canvas {
	id window;
	id view;
	double x, y;   /* the top left of the screen's frame */
};

static const struct options *defaults;
static struct box *boxes;
static size_t nboxes;
static struct canvas *canvases;
static size_t ncanvases;
static id fill_color, stroke_color, text_attributes;

static int by_box_position(const void *a, const void *b)
{
	const struct box *l = a, *r = b;

	if (l->x != r->x)
		return l->x < r->x ? -1 : 1;
	if (l->y != r->y)
		return l->y < r->y ? -1 : 1;
	return (l->id > r->id) - (l->id < r->id);
}

static int by_box_position_then_sigil(const void *a, const void *b)
{
	const struct box *l = a, *r = b;

	if (l->x != r->x)
		return l->x < r->x ? -1 : 1;
	if (l->y != r->y)
		return l->y < r->y ? -1 : 1;
	return (l->sigil > r->sigil) - (l->sigil < r->sigil);
}

static int overlapping(const struct box *a, const struct box *b)
{
	if (b->y >= a->y + SIGIL_HEIGHT) return 0;
	if (a->y >= b->y + SIGIL_HEIGHT) return 0;
	if (b->x >= a->x + SIGIL_WIDTH + SIGIL_MARGIN) return 0;
	if (a->x >= b->x + SIGIL_WIDTH + SIGIL_MARGIN) return 0;
	return 1;
}

/* Of the boxes in [start, end), which share a position, the most recently focused is on top. */
static void set_top(size_t start, size_t end)
{
	size_t top = start;

	for (size_t i = start + 1; i < end; i++)
		if (boxes[i].focus_order < boxes[top].focus_order)
			top = i;
	boxes[top].top = 1;
}

/* WindowSigils' _makeSigilBoxes. */
static void make_boxes(void)
{
	struct window *windows;
	size_t count;

	nboxes = 0;
	if (sigil_windows(defaults, &windows, &count, stderr) < 0)
		return;
	struct box *grown = realloc(boxes, (count + 1) * sizeof *boxes);
	if (!grown) {
		free_windows(windows, count);
		return;
	}
	boxes = grown;
	for (size_t i = 0; i < count && windows[i].sigil; i++) {
		boxes[nboxes++] = (struct box){
			.x = windows[i].x + 70,
			.y = windows[i].y + 1,
			.id = windows[i].id,
			/* The window server lists windows front to back, so most recently focused first. */
			.focus_order = windows[i].id ? windows[i].z : INT_MAX,
			.sigil = windows[i].sigil,
		};
	}
	free_windows(windows, count);

	qsort(boxes, nboxes, sizeof *boxes, by_box_position);
	size_t start = 0;
	for (size_t end = 1; end < nboxes; end++) {
		if (boxes[start].x != boxes[end].x || boxes[start].y != boxes[end].y) {
			set_top(start, end);
			start = end;
		}
	}
	if (nboxes)
		set_top(start, nboxes);

	/* move_overlapping_boxes */
	qsort(boxes, nboxes, sizeof *boxes, by_box_position_then_sigil);
	for (size_t i = 0; i < nboxes; i++)
		for (size_t j = i + 1; j < nboxes && overlapping(&boxes[i], &boxes[j]); j++)
			boxes[j].x = boxes[i].x + SIGIL_WIDTH + SIGIL_MARGIN;
}

static id make_string(const char *s)
{
	return (id)CFStringCreateWithCString(NULL, s, kCFStringEncodingUTF8);
}

/* The rectangle and text elements WindowSigils gave hs.canvas for a sigil, drawn as hs.canvas draws them. */
static void draw_box(const struct box *b, const struct canvas *c)
{
	CGRect frame = { { b->x - c->x, b->y - c->y }, { SIGIL_WIDTH, SIGIL_HEIGHT } };
	id path = MSG(id)(cls("NSBezierPath"), sel("bezierPath"));

	MSG(void, CGRect, CGFloat, CGFloat)(path, sel("appendBezierPathWithRoundedRect:xRadius:yRadius:"),
					     frame, 0, 0);
	MSG(void)(fill_color, sel("setFill"));
	MSG(void)(path, sel("fill"));
	if (b->top) {
		MSG(void)(stroke_color, sel("setStroke"));
		MSG(void)(path, sel("stroke"));
	}

	char text[2] = { b->sigil, '\0' };
	id string = make_string(text);
	CGRect text_frame = { { frame.origin.x + 3, frame.origin.y - 4 },
			      { SIGIL_WIDTH - 3, SIGIL_HEIGHT + 7 } };
	MSG(void, CGRect, id)(string, sel("drawInRect:withAttributes:"), text_frame, text_attributes);
	CFRelease((CFTypeRef)string);
}

static void draw_rect(id self, SEL cmd, CGRect dirty)
{
	(void)cmd;
	(void)dirty;
	for (size_t i = 0; i < ncanvases; i++) {
		if (canvases[i].view != self)
			continue;
		MSG(void, CGFloat)(cls("NSBezierPath"), sel("setDefaultLineWidth:"), 1.0);
		for (size_t j = 0; j < nboxes; j++)
			draw_box(&boxes[j], &canvases[i]);
	}
}

static BOOL is_flipped(id self, SEL cmd)
{
	(void)self;
	(void)cmd;
	return YES;
}

static id view_class(void)
{
	static Class view;

	if (!view) {
		view = objc_allocateClassPair(objc_getClass("NSView"), "SigilsCanvasView", 0);
		class_addMethod(view, sel("drawRect:"), (IMP)draw_rect, "v@:{CGRect={CGPoint=dd}{CGSize=dd}}");
		class_addMethod(view, sel("isFlipped"), (IMP)is_flipped, "B@:");
		objc_registerClassPair(view);
	}
	return (id)view;
}

static void make_drawing_attributes(void)
{
	fill_color = MSG(id, CGFloat, CGFloat, CGFloat, CGFloat)(
		cls("NSColor"), sel("colorWithCalibratedRed:green:blue:alpha:"), 0.0, 1.0, 1.0, 0.3);
	MSG(id)(fill_color, sel("retain"));
	stroke_color = MSG(id, CGFloat, CGFloat)(cls("NSColor"), sel("colorWithCalibratedWhite:alpha:"),
						 1.0, 1.0);
	MSG(id)(stroke_color, sel("retain"));

	id text_color = MSG(id, CGFloat, CGFloat)(cls("NSColor"), sel("colorWithCalibratedWhite:alpha:"),
						  1.0, 1.0);
	id font_name = make_string("Menlo Regular");
	id font = MSG(id, id, CGFloat)(cls("NSFont"), sel("fontWithName:size:"), font_name, 18.0);
	CFRelease((CFTypeRef)font_name);
	if (!font)
		font = MSG(id, CGFloat)(cls("NSFont"), sel("userFixedPitchFontOfSize:"), 18.0);
	id style = MSG(id)(MSG(id)(cls("NSParagraphStyle"), sel("defaultParagraphStyle")),
			   sel("mutableCopy"));
	MSG(void, long)(style, sel("setAlignment:"), NSTextAlignmentNatural);
	MSG(void, unsigned long)(style, sel("setLineBreakMode:"), NSLineBreakByTruncatingTail);

	const void *keys[] = { NSForegroundColorAttributeName, NSFontAttributeName,
			       NSParagraphStyleAttributeName };
	const void *values[] = { text_color, font, style };
	text_attributes = (id)CFDictionaryCreate(NULL, keys, values, 3, &kCFTypeDictionaryKeyCallBacks,
						 &kCFTypeDictionaryValueCallBacks);
	MSG(void)(style, sel("release"));
}

static void close_canvases(void)
{
	for (size_t i = 0; i < ncanvases; i++) {
		MSG(void, id)(canvases[i].window, sel("orderOut:"), nil);
		MSG(void)(canvases[i].window, sel("release"));
		MSG(void)(canvases[i].view, sel("release"));
	}
	free(canvases);
	canvases = NULL;
	ncanvases = 0;
}

/* WindowSigils' canvas for each screen, configured as hs.canvas configures its windows. */
static void make_canvases(void)
{
	close_canvases();

	id screens = MSG(id)(cls("NSScreen"), sel("screens"));
	size_t count = MSG(unsigned long)(screens, sel("count"));
	if (count == 0 || !(canvases = calloc(count, sizeof *canvases)))
		return;
	id primary = MSG(id, unsigned long)(screens, sel("objectAtIndex:"), 0);
	CGFloat primary_height = MSG_STRET(CGRect)(primary, sel("frame")).size.height;

	for (size_t i = 0; i < count; i++) {
		id screen = MSG(id, unsigned long)(screens, sel("objectAtIndex:"), i);
		CGRect frame = MSG_STRET(CGRect)(screen, sel("visibleFrame"));
		id window = MSG(id)(cls("NSWindow"), sel("alloc"));

		window = MSG(id, CGRect, unsigned long, unsigned long, BOOL)(
			window, sel("initWithContentRect:styleMask:backing:defer:"), frame,
			NSWindowStyleMaskBorderless, NSBackingStoreBuffered, YES);
		MSG(void, BOOL)(window, sel("setReleasedWhenClosed:"), NO);
		MSG(void, id)(window, sel("setBackgroundColor:"), MSG(id)(cls("NSColor"), sel("clearColor")));
		MSG(void, BOOL)(window, sel("setOpaque:"), NO);
		MSG(void, BOOL)(window, sel("setHasShadow:"), NO);
		MSG(void, BOOL)(window, sel("setIgnoresMouseEvents:"), YES);
		MSG(void, BOOL)(window, sel("setRestorable:"), NO);
		MSG(void, BOOL)(window, sel("setHidesOnDeactivate:"), NO);
		MSG(void, long)(window, sel("setAnimationBehavior:"), NSWindowAnimationBehaviorNone);
		MSG(void, long)(window, sel("setLevel:"), CGWindowLevelForKey(kCGScreenSaverWindowLevelKey));

		CGRect bounds = MSG_STRET(CGRect)(MSG(id)(window, sel("contentView")), sel("bounds"));
		id view = MSG(id, CGRect)(MSG(id)(view_class(), sel("alloc")), sel("initWithFrame:"), bounds);
		MSG(void, id)(window, sel("setContentView:"), view);
		MSG(void)(window, sel("orderFrontRegardless"));

		canvases[ncanvases++] = (struct canvas){
			.window = window,
			.view = view,
			.x = frame.origin.x,
			.y = primary_height - frame.origin.y - frame.size.height,
		};
	}
}

static void refresh(void)
{
	void *pool = objc_autoreleasePoolPush();

	make_boxes();
	for (size_t i = 0; i < ncanvases; i++)
		MSG(void, BOOL)(canvases[i].view, sel("setNeedsDisplay:"), YES);
	objc_autoreleasePoolPop(pool);
}

/* Windows change in bursts, so all the changes in one turn of the run loop are drawn once. */
static void schedule_refresh(void)
{
	static int pending;

	if (pending)
		return;
	pending = 1;
	dispatch_async(dispatch_get_main_queue(), ^{
		pending = 0;
		refresh();
	});
}

/* What hs.window.filter watches, through hs.uielement.watcher. */
static CFStringRef const app_notifications[] = {
	CFSTR("AXWindowCreated"),
	CFSTR("AXFocusedWindowChanged"),
};
static CFStringRef const window_notifications[] = {
	CFSTR("AXUIElementDestroyed"),
	CFSTR("AXWindowMoved"),
	CFSTR("AXWindowResized"),
	CFSTR("AXWindowMiniaturized"),
	CFSTR("AXWindowDeminiaturized"),
	CFSTR("AXTitleChanged"),
};

struct watched_app {
	pid_t pid;
	AXObserverRef observer;
	AXUIElementRef element;
};

static struct watched_app *apps;
static size_t napps;

static void watch_window(AXObserverRef observer, AXUIElementRef window)
{
	for (size_t i = 0; i < sizeof window_notifications / sizeof *window_notifications; i++)
		AXObserverAddNotification(observer, window, window_notifications[i], NULL);
}

static void observed(AXObserverRef observer, AXUIElementRef element, CFStringRef notification,
		     void *refcon)
{
	(void)refcon;
	if (CFEqual(notification, CFSTR("AXWindowCreated")))
		watch_window(observer, element);
	schedule_refresh();
}

static void watch_app(pid_t pid)
{
	AXObserverRef observer;

	if (pid == getpid())
		return;
	for (size_t i = 0; i < napps; i++)
		if (apps[i].pid == pid)
			return;
	if (AXObserverCreate(pid, observed, &observer) != kAXErrorSuccess)
		return;
	struct watched_app *grown = realloc(apps, (napps + 1) * sizeof *apps);
	if (!grown) {
		CFRelease(observer);
		return;
	}
	apps = grown;
	AXUIElementRef element = AXUIElementCreateApplication(pid);
	apps[napps++] = (struct watched_app){ pid, observer, element };

	for (size_t i = 0; i < sizeof app_notifications / sizeof *app_notifications; i++)
		AXObserverAddNotification(observer, element, app_notifications[i], NULL);
	CFArrayRef windows;
	if (AXUIElementCopyAttributeValues(element, kAXWindowsAttribute, 0, 100, &windows) ==
	    kAXErrorSuccess) {
		for (CFIndex i = 0; i < CFArrayGetCount(windows); i++)
			watch_window(observer, CFArrayGetValueAtIndex(windows, i));
		CFRelease(windows);
	}
	CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(observer),
			   kCFRunLoopDefaultMode);
}

static void unwatch_app(pid_t pid)
{
	for (size_t i = 0; i < napps; i++) {
		if (apps[i].pid != pid)
			continue;
		CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(apps[i].observer),
				      kCFRunLoopDefaultMode);
		CFRelease(apps[i].observer);
		CFRelease(apps[i].element);
		apps[i] = apps[--napps];
		return;
	}
}

static id notification_app(id notification)
{
	id info = MSG(id)(notification, sel("userInfo"));
	return MSG(id, id)(info, sel("objectForKey:"), NSWorkspaceApplicationKey);
}

static pid_t app_pid(id app)
{
	return app ? MSG(pid_t)(app, sel("processIdentifier")) : -1;
}

/* Like hs.window.filter, leave out applications which can't have windows. */
static void watch_running_app(id app)
{
	if (app && MSG(long)(app, sel("activationPolicy")) != NSApplicationActivationPolicyProhibited)
		watch_app(app_pid(app));
}

static void observe(id center, id name, void (^block)(id notification))
{
	MSG(id, id, id, id, id)(center, sel("addObserverForName:object:queue:usingBlock:"), name, nil,
				MSG(id)(cls("NSOperationQueue"), sel("mainQueue")), (id)block);
}

/* What hs.window.filter watches, through hs.application.watcher, hs.spaces.watcher and hs.screen.watcher. */
static void watch_workspace(void)
{
	id workspace = MSG(id)(cls("NSWorkspace"), sel("sharedWorkspace"));
	id center = MSG(id)(workspace, sel("notificationCenter"));

	observe(center, NSWorkspaceDidLaunchApplicationNotification, ^(id note) {
		watch_running_app(notification_app(note));
		schedule_refresh();
	});
	observe(center, NSWorkspaceDidTerminateApplicationNotification, ^(id note) {
		unwatch_app(app_pid(notification_app(note)));
		schedule_refresh();
	});
	observe(center, NSWorkspaceDidActivateApplicationNotification, ^(id note) {
		(void)note;
		schedule_refresh();
	});
	observe(center, NSWorkspaceDidHideApplicationNotification, ^(id note) {
		(void)note;
		schedule_refresh();
	});
	observe(center, NSWorkspaceDidUnhideApplicationNotification, ^(id note) {
		(void)note;
		schedule_refresh();
	});
	observe(center, NSWorkspaceActiveSpaceDidChangeNotification, ^(id note) {
		(void)note;
		schedule_refresh();
	});
	observe(MSG(id)(cls("NSNotificationCenter"), sel("defaultCenter")),
		NSApplicationDidChangeScreenParametersNotification, ^(id note) {
			(void)note;
			make_canvases();
			schedule_refresh();
		});

	id running = MSG(id)(workspace, sel("runningApplications"));
	for (unsigned long i = 0, n = MSG(unsigned long)(running, sel("count")); i < n; i++) {
		watch_running_app(MSG(id, unsigned long)(running, sel("objectAtIndex:"), i));
	}
}

static void client_waiting(CFFileDescriptorRef ref, CFOptionFlags flags, void *info)
{
	int listen_fd = CFFileDescriptorGetNativeDescriptor(ref);
	int fd;

	(void)flags;
	(void)info;
	while ((fd = accept(listen_fd, NULL, NULL)) >= 0) {
		void *pool = objc_autoreleasePoolPush();
		fcntl(fd, F_SETFL, fcntl(fd, F_GETFL) & ~O_NONBLOCK);
		handle_client(fd, defaults);
		close(fd);
		objc_autoreleasePoolPop(pool);
	}
	CFFileDescriptorEnableCallBacks(ref, kCFFileDescriptorReadCallBack);
}

int serve_and_draw(int listen_fd, const struct options *options)
{
	defaults = options;
	fcntl(listen_fd, F_SETFL, fcntl(listen_fd, F_GETFL) | O_NONBLOCK);

	id app = MSG(id)(cls("NSApplication"), sel("sharedApplication"));
	MSG(BOOL, long)(app, sel("setActivationPolicy:"), NSApplicationActivationPolicyAccessory);

	CFFileDescriptorRef ref = CFFileDescriptorCreate(NULL, listen_fd, false, client_waiting, NULL);
	CFRunLoopSourceRef source = CFFileDescriptorCreateRunLoopSource(NULL, ref, 0);
	CFRunLoopAddSource(CFRunLoopGetMain(), source, kCFRunLoopCommonModes);
	CFFileDescriptorEnableCallBacks(ref, kCFFileDescriptorReadCallBack);

	make_drawing_attributes();
	make_canvases();
	watch_workspace();
	refresh();

	MSG(void)(app, sel("run"));
	return 0;
}
