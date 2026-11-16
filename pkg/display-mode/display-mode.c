/*
 * display-mode - set a monitor's resolution.
 *
 *   display-mode MONITOR WIDTHxHEIGHT
 *
 * MONITOR is counted from 1 at the left, as AeroSpace numbers them.  The size
 * is what the UI looks like, as System Settings shows it.  Of the modes with
 * that size, it picks the sharpest (HiDPI), then the fastest.  The change
 * lasts until logout.
 */
#include <CoreGraphics/CoreGraphics.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_DISPLAYS 16

static bool display_numbered(int number, CGDirectDisplayID* display)
{
    CGDirectDisplayID displays[MAX_DISPLAYS];
    uint32_t count = 0;

    if (CGGetActiveDisplayList(MAX_DISPLAYS, displays, &count) != kCGErrorSuccess)
        return false;
    for (uint32_t i = 0; i < count; i++)
    {
        CGFloat x = CGDisplayBounds(displays[i]).origin.x;
        int left_of = 0;
        for (uint32_t j = 0; j < count; j++)
            if (CGDisplayBounds(displays[j]).origin.x < x)
                left_of++;
        if (left_of + 1 == number)
        {
            *display = displays[i];
            return true;
        }
    }
    return false;
}

static bool better(CGDisplayModeRef mode, CGDisplayModeRef than)
{
    if (!than)
        return true;
    size_t pixels = CGDisplayModeGetPixelWidth(mode), than_pixels = CGDisplayModeGetPixelWidth(than);
    if (pixels != than_pixels)
        return pixels > than_pixels;
    return CGDisplayModeGetRefreshRate(mode) > CGDisplayModeGetRefreshRate(than);
}

static void list_sizes(CFArrayRef modes)
{
    size_t last_width = 0, last_height = 0;

    /* Each size comes in several modes, listed together. */
    fprintf(stderr, "display-mode: sizes available:");
    for (CFIndex i = 0; i < CFArrayGetCount(modes); i++)
    {
        CGDisplayModeRef mode = (CGDisplayModeRef)CFArrayGetValueAtIndex(modes, i);
        size_t width = CGDisplayModeGetWidth(mode), height = CGDisplayModeGetHeight(mode);
        if (!CGDisplayModeIsUsableForDesktopGUI(mode) || (width == last_width && height == last_height))
            continue;
        fprintf(stderr, " %zux%zu", width, height);
        last_width = width;
        last_height = height;
    }
    fprintf(stderr, "\n");
}

int main(int argc, char* argv[])
{
    int number;
    size_t width, height;
    char extra;
    CGDirectDisplayID display;

    if (argc != 3 || sscanf(argv[1], "%d%c", &number, &extra) != 1 ||
        sscanf(argv[2], "%zux%zu%c", &width, &height, &extra) != 2)
    {
        fprintf(stderr, "usage: display-mode MONITOR WIDTHxHEIGHT\n");
        return 2;
    }
    if (!display_numbered(number, &display))
    {
        fprintf(stderr, "display-mode: no monitor %d\n", number);
        return 1;
    }

    /* Without this option, HiDPI modes aren't listed. */
    const void* keys[] = { kCGDisplayShowDuplicateLowResolutionModes };
    const void* values[] = { kCFBooleanTrue };
    CFDictionaryRef options = CFDictionaryCreate(NULL, keys, values, 1, &kCFTypeDictionaryKeyCallBacks,
                                                 &kCFTypeDictionaryValueCallBacks);
    CFArrayRef modes = CGDisplayCopyAllDisplayModes(display, options);
    CFRelease(options);
    if (!modes)
    {
        fprintf(stderr, "display-mode: can't list modes of monitor %d\n", number);
        return 1;
    }

    CGDisplayModeRef best = NULL;
    for (CFIndex i = 0; i < CFArrayGetCount(modes); i++)
    {
        CGDisplayModeRef mode = (CGDisplayModeRef)CFArrayGetValueAtIndex(modes, i);
        if (CGDisplayModeIsUsableForDesktopGUI(mode) && CGDisplayModeGetWidth(mode) == width &&
            CGDisplayModeGetHeight(mode) == height && better(mode, best))
            best = mode;
    }
    if (!best)
    {
        fprintf(stderr, "display-mode: monitor %d has no %zux%zu mode\n", number, width, height);
        list_sizes(modes);
        return 1;
    }

    CGDisplayConfigRef config;
    CGError error = CGBeginDisplayConfiguration(&config);
    if (error == kCGErrorSuccess)
    {
        CGConfigureDisplayWithDisplayMode(config, display, best, NULL);
        error = CGCompleteDisplayConfiguration(config, kCGConfigureForSession);
    }
    CFRelease(modes);
    if (error != kCGErrorSuccess)
    {
        fprintf(stderr, "display-mode: can't set the mode (error %d)\n", error);
        return 1;
    }
    return 0;
}
