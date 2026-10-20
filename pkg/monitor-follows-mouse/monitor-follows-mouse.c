/*
 * monitor-follows-mouse - have AeroSpace focus the monitor the pointer is on.
 *
 *   monitor-follows-mouse path-to-aerospace
 *
 * So new windows and the workspace commands act there.  Every tenth of a
 * second, it looks at which display the pointer is on, and once it has
 * stayed on one it moved to for two looks, which waits out AeroSpace moving
 * it as focus passes between monitors, it focuses that one, if it isn't
 * already.  It focuses the window under the pointer, if there is one, so
 * AeroSpace has no reason to move the pointer to some other window, and
 * otherwise the monitor, numbered from the left as AeroSpace does.
 */
#include <CoreGraphics/CoreGraphics.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define MAX_DISPLAYS 16

static const char* aerospace;

static bool pointer_display(CGDirectDisplayID* display)
{
    CGEventRef event = CGEventCreate(NULL);
    uint32_t count = 0;

    if (!event)
        return false;
    CGPoint location = CGEventGetLocation(event);
    CFRelease(event);
    return CGGetDisplaysWithPoint(location, 1, display, &count) == kCGErrorSuccess && count == 1;
}

/* Counted from 1 at the left, as AeroSpace numbers them. */
static int monitor_number(CGDirectDisplayID display)
{
    CGDirectDisplayID displays[MAX_DISPLAYS];
    uint32_t count = 0;
    int number = 1;

    if (CGGetActiveDisplayList(MAX_DISPLAYS, displays, &count) != kCGErrorSuccess)
        return 0;
    CGFloat x = CGDisplayBounds(display).origin.x;
    for (uint32_t i = 0; i < count; i++)
        if (CGDisplayBounds(displays[i]).origin.x < x)
            number++;
    return number;
}

static int focused_monitor(void)
{
    char command[PATH_MAX + 64];
    char output[32] = "";

    snprintf(command, sizeof command, "'%s' list-monitors --focused --format '%%{monitor-id}'",
             aerospace);
    FILE* f = popen(command, "r");
    if (!f)
        return 0;
    if (!fgets(output, sizeof output, f))
        output[0] = '\0';
    pclose(f);
    return atoi(output);
}

/* The frontmost ordinary window under the pointer, or 0. */
static long window_under_pointer(void)
{
    CGEventRef event = CGEventCreate(NULL);
    long found = 0;

    if (!event)
        return 0;
    CGPoint location = CGEventGetLocation(event);
    CFRelease(event);

    CFArrayRef info = CGWindowListCopyWindowInfo(
        kCGWindowListOptionOnScreenOnly | kCGWindowListExcludeDesktopElements, kCGNullWindowID);
    for (CFIndex i = 0; info && !found && i < CFArrayGetCount(info); i++)
    {
        CFDictionaryRef window = CFArrayGetValueAtIndex(info, i);
        long layer = -1, number = 0;
        CGRect bounds;
        CFDictionaryRef b = CFDictionaryGetValue(window, kCGWindowBounds);

        CFNumberGetValue(CFDictionaryGetValue(window, kCGWindowLayer), kCFNumberLongType, &layer);
        CFNumberGetValue(CFDictionaryGetValue(window, kCGWindowNumber), kCFNumberLongType, &number);
        if (layer == 0 && b && CGRectMakeWithDictionaryRepresentation(b, &bounds) &&
            CGRectContainsPoint(bounds, location))
            found = number;
    }
    if (info)
        CFRelease(info);
    return found;
}

static void run(const char* format, long argument)
{
    char command[PATH_MAX + 64];

    snprintf(command, sizeof command, format, aerospace, argument);
    if (system(command) != 0)
        fprintf(stderr, "monitor-follows-mouse: %s failed\n", command);
}

static void focus_monitor(int number)
{
    run("'%s' focus-monitor %ld", number);
}

int main(int argc, char* argv[])
{
    CGDirectDisplayID current = kCGNullDirectDisplay;
    int settled = 0;

    if (argc != 2 || strchr(argv[1], '\''))
    {
        fprintf(stderr, "usage: monitor-follows-mouse path-to-aerospace\n");
        return 2;
    }
    aerospace = argv[1];

    for (;;)
    {
        struct timespec tenth = { 0, 100 * 1000 * 1000 };
        CGDirectDisplayID display;

        nanosleep(&tenth, NULL);
        if (!pointer_display(&display))
            continue;
        if (display != current)
        {
            current = display;
            settled = 0;
            continue;
        }
        if (++settled != 2)
            continue;

        int number = monitor_number(display);
        if (!number || number == focused_monitor())
            continue;
        long window = window_under_pointer();
        if (window)
            run("'%s' focus --window-id %ld", window);
        else
            focus_monitor(number);
    }
}
