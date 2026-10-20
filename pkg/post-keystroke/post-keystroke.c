/*
 * post-keystroke - type a key into the focused application.
 *
 *   post-keystroke [-n] [-p pid] [ctrl] [alt] [shift] [cmd] key
 *
 * The key goes straight to the application's process, so global hotkeys,
 * like AeroSpace's, don't see it, and binding a key to pass it on doesn't
 * loop.  The key is a letter, digit or punctuation key of a US keyboard.
 * Without -p, the process is found through the accessibility API.  With -n,
 * it prints the process and key code rather than typing.  Whatever
 * runs it needs Accessibility.
 */
#include <ApplicationServices/ApplicationServices.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

struct key
{
    const char* name;
    CGKeyCode code;
};

/* From Carbon's kVK_ANSI_* constants. */
static const struct key KEYS[] =
{
    { "a", 0 }, { "s", 1 }, { "d", 2 }, { "f", 3 }, { "h", 4 }, { "g", 5 },
    { "z", 6 }, { "x", 7 }, { "c", 8 }, { "v", 9 }, { "b", 11 }, { "q", 12 },
    { "w", 13 }, { "e", 14 }, { "r", 15 }, { "y", 16 }, { "t", 17 }, { "1", 18 },
    { "2", 19 }, { "3", 20 }, { "4", 21 }, { "6", 22 }, { "5", 23 }, { "=", 24 },
    { "9", 25 }, { "7", 26 }, { "-", 27 }, { "8", 28 }, { "0", 29 }, { "]", 30 },
    { "o", 31 }, { "u", 32 }, { "[", 33 }, { "i", 34 }, { "p", 35 }, { "l", 37 },
    { "j", 38 }, { "'", 39 }, { "k", 40 }, { ";", 41 }, { "\\", 42 }, { ",", 43 },
    { "/", 44 }, { "n", 45 }, { "m", 46 }, { ".", 47 }, { "`", 50 },
};

struct modifier
{
    const char* name;
    CGEventFlags flag;
};

static const struct modifier MODIFIERS[] =
{
    { "ctrl", kCGEventFlagMaskControl },
    { "alt", kCGEventFlagMaskAlternate },
    { "shift", kCGEventFlagMaskShift },
    { "cmd", kCGEventFlagMaskCommand },
};

#define COUNT(a) (sizeof (a) / sizeof (a)[0])

static void usage(void)
{
    fputs("usage: post-keystroke [-n] [-p pid] [ctrl] [alt] [shift] [cmd] key\n", stderr);
}

static bool focused_pid(pid_t* pid)
{
    AXUIElementRef system = AXUIElementCreateSystemWide();
    CFTypeRef app = NULL;
    bool found = false;

    if (AXUIElementCopyAttributeValue(system, kAXFocusedApplicationAttribute, &app) ==
            kAXErrorSuccess)
    {
        found = AXUIElementGetPid((AXUIElementRef)app, pid) == kAXErrorSuccess;
        CFRelease(app);
    }
    CFRelease(system);
    return found;
}

static void post(CGEventSourceRef source, pid_t pid, CGKeyCode code, CGEventFlags flags,
                 bool down)
{
    CGEventRef event = CGEventCreateKeyboardEvent(source, code, down);

    CGEventSetFlags(event, flags);
    CGEventPostToPid(pid, event);
    CFRelease(event);
}

int main(int argc, char* argv[])
{
    bool dry_run = false;
    CGEventFlags flags = 0;
    pid_t pid = 0;
    int arg = 1;

    if (arg < argc && strcmp(argv[arg], "-n") == 0)
    {
        dry_run = true;
        arg++;
    }
    if (arg + 1 < argc && strcmp(argv[arg], "-p") == 0)
    {
        pid = atoi(argv[arg + 1]);
        arg += 2;
    }
    for (; arg < argc - 1; arg++)
    {
        size_t i;
        for (i = 0; i < COUNT(MODIFIERS); i++)
            if (strcmp(argv[arg], MODIFIERS[i].name) == 0)
                break;
        if (i == COUNT(MODIFIERS))
        {
            usage();
            return 2;
        }
        flags |= MODIFIERS[i].flag;
    }
    if (arg != argc - 1)
    {
        usage();
        return 2;
    }

    const struct key* key = NULL;
    for (size_t i = 0; i < COUNT(KEYS); i++)
        if (strcmp(argv[arg], KEYS[i].name) == 0)
            key = &KEYS[i];
    if (!key)
    {
        fprintf(stderr, "post-keystroke: no key %s\n", argv[arg]);
        return 2;
    }

    if (!pid && !focused_pid(&pid))
    {
        fprintf(stderr, "post-keystroke: can't find the focused application\n");
        return 1;
    }
    if (dry_run)
    {
        printf("%d %d\n", (int)pid, (int)key->code);
        return 0;
    }
    /* Without it, macOS drops posted events and says nothing. */
    if (!AXIsProcessTrusted())
    {
        fprintf(stderr, "post-keystroke: not allowed Accessibility (pid %d), so nothing is typed\n",
                (int)getpid());
        return 1;
    }
    CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStateHIDSystemState);
    post(source, pid, key->code, flags, true);
    usleep(20000);
    post(source, pid, key->code, flags, false);
    if (source)
        CFRelease(source);
    return 0;
}
