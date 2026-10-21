PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=gnu99 -fblocks
LDLIBS += -framework AppKit -framework Foundation -lobjc

wake-watcher: wake-watcher.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ wake-watcher.c $(LDLIBS)

install: wake-watcher
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 wake-watcher $(DESTDIR)$(PREFIX)/bin/wake-watcher

clean:
	rm -f wake-watcher

.PHONY: install clean
