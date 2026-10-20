PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99
LDLIBS += -framework ApplicationServices

post-keystroke: post-keystroke.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ post-keystroke.c $(LDLIBS)

install: post-keystroke
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 post-keystroke $(DESTDIR)$(PREFIX)/bin/post-keystroke

clean:
	rm -f post-keystroke

.PHONY: install clean
