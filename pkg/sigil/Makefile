PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99

ifeq ($(shell uname),Darwin)
BACKEND = darwin.c
LDLIBS += -framework CoreFoundation -framework CoreGraphics
else
BACKEND = x11.c
LDLIBS += -lX11
endif

sigil: sigil.c $(BACKEND) sigil.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ sigil.c $(BACKEND) $(LDLIBS)

install: sigil
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 sigil $(DESTDIR)$(PREFIX)/bin/sigil

clean:
	rm -f sigil

.PHONY: install clean
