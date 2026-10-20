PREFIX ?= /usr/local
CFLAGS ?= -O2 -Wall -Wextra
CFLAGS += -std=c99

ifeq ($(shell uname),Darwin)
CFLAGS += -fblocks
LDLIBS += -framework AppKit -framework ApplicationServices -framework CoreFoundation -framework CoreGraphics -lobjc
else
LDLIBS += -lX11
endif

sigils: sigils.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ sigils.c $(LDLIBS)

# For tests: windows and displays from $SIGILS_FAKE_WINDOWS.
sigils-fake: sigils.c
	$(CC) $(CFLAGS) $(LDFLAGS) -DSIGILS_FAKE -o $@ sigils.c

check: sigils-fake
	./tests/run.sh ./sigils-fake

install: sigils
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 sigils $(DESTDIR)$(PREFIX)/bin/sigils

clean:
	rm -f sigils sigils-fake

.PHONY: check install clean
