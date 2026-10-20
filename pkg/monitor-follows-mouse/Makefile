PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99
LDLIBS += -framework CoreGraphics -framework CoreFoundation

monitor-follows-mouse: monitor-follows-mouse.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ monitor-follows-mouse.c $(LDLIBS)

install: monitor-follows-mouse
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 monitor-follows-mouse $(DESTDIR)$(PREFIX)/bin/monitor-follows-mouse

clean:
	rm -f monitor-follows-mouse

.PHONY: install clean
