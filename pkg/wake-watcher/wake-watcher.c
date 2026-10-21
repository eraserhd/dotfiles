/*
 * wake-watcher - run a command when the Mac or its displays wake.
 *
 *   wake-watcher command [argument ...]
 *
 * It uses the notifications Hammerspoon's hs.caffeinate.watcher did,
 * systemDidWake and screensDidWake.  sleepwatcher's display options don't
 * work on Apple silicon, and a Mac which is only dark, as it is here with
 * sleep off, never wakes as a system.  One command runs at a time; a wake
 * while it runs is ignored.
 */
#include <dispatch/dispatch.h>
#include <objc/message.h>
#include <objc/runtime.h>
#include <spawn.h>
#include <stdio.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define MSG(ret, ...) ((ret (*)(id, SEL, ##__VA_ARGS__))objc_msgSend)

extern char** environ;
extern id NSWorkspaceDidWakeNotification;
extern id NSWorkspaceScreensDidWakeNotification;

enum
{
    NSApplicationActivationPolicyAccessory = 1,
};

static char** command;
static volatile int running;

static SEL sel(const char* name)
{
    return sel_registerName(name);
}

static id cls(const char* name)
{
    return (id)objc_getClass(name);
}

static void run_command(const char* why)
{
    time_t now = time(NULL);
    char stamp[32];

    strftime(stamp, sizeof stamp, "%F %T", localtime(&now));
    if (__sync_lock_test_and_set(&running, 1))
    {
        fprintf(stderr, "%s %s, but the command is still running\n", stamp, why);
        return;
    }
    fprintf(stderr, "%s %s\n", stamp, why);
    dispatch_async(dispatch_get_global_queue(QOS_CLASS_DEFAULT, 0), ^{
        pid_t pid;

        if (posix_spawnp(&pid, command[0], NULL, NULL, command, environ) == 0)
            waitpid(pid, NULL, 0);
        else
            perror("wake-watcher");
        __sync_lock_release(&running);
    });
}

static void observe(id center, id name, const char* why)
{
    MSG(id, id, id, id, id)(center, sel("addObserverForName:object:queue:usingBlock:"), name, nil,
                            MSG(id)(cls("NSOperationQueue"), sel("mainQueue")),
                            (id) ^(id notification) {
                                (void)notification;
                                run_command(why);
                            });
}

int main(int argc, char* argv[])
{
    if (argc < 2)
    {
        fprintf(stderr, "usage: wake-watcher command [argument ...]\n");
        return 2;
    }
    command = argv + 1;
    setvbuf(stderr, NULL, _IOLBF, 0);

    id app = MSG(id)(cls("NSApplication"), sel("sharedApplication"));
    MSG(BOOL, long)(app, sel("setActivationPolicy:"), NSApplicationActivationPolicyAccessory);

    id center = MSG(id)(MSG(id)(cls("NSWorkspace"), sel("sharedWorkspace")), sel("notificationCenter"));
    observe(center, NSWorkspaceDidWakeNotification, "the system woke");
    observe(center, NSWorkspaceScreensDidWakeNotification, "the screens woke");
#ifdef WAKE_WATCHER_TEST
    extern id NSWorkspaceDidLaunchApplicationNotification;
    observe(center, NSWorkspaceDidLaunchApplicationNotification, "an application launched");
#endif

    MSG(void)(app, sel("run"));
    return 0;
}
