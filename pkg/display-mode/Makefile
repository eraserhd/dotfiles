PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99
LDLIBS += -framework CoreGraphics -framework CoreFoundation

display-mode: display-mode.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ display-mode.c $(LDLIBS)

install: display-mode
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 display-mode $(DESTDIR)$(PREFIX)/bin/display-mode

clean:
	rm -f display-mode

.PHONY: install clean
