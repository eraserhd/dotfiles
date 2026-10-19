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

sigils: sigils.c $(BACKEND) sigils.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ sigils.c $(BACKEND) $(LDLIBS)

install: sigils
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 sigils $(DESTDIR)$(PREFIX)/bin/sigils

clean:
	rm -f sigils

.PHONY: install clean
