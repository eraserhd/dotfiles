PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99

ifeq ($(shell uname),Darwin)
BACKEND = darwin.c overlay.c
CFLAGS += -fblocks
LDLIBS += -framework AppKit -framework ApplicationServices -framework CoreFoundation -framework CoreGraphics -lobjc
else
BACKEND = x11.c
LDLIBS += -lX11
endif

sigils: sigils.c socket.c spaces.c $(BACKEND) sigils.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ sigils.c socket.c spaces.c $(BACKEND) $(LDLIBS)

sigils-fake: sigils.c socket.c spaces.c fake.c sigils.h
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ sigils.c socket.c spaces.c fake.c

check: sigils-fake
	./tests/run.sh ./sigils-fake

install: sigils
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 sigils $(DESTDIR)$(PREFIX)/bin/sigils

clean:
	rm -f sigils sigils-fake

.PHONY: check install clean
