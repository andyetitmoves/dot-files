PREFIX ?= $(shell pkg-config --variable=prefix gaim)
LIBDIR ?= $(shell pkg-config --variable=libdir gaim)

PLUGINDIR ?= $(LIBDIR)/gaim
DOCDIR ?= $(PREFIX)/share/doc/gaim-copy-status

GAIM_CFLAGS = $(shell pkg-config --cflags gaim gtk+-2.0)
GAIM_LDFLAGS = $(shell pkg-config --libs gaim gtk+-2.0)

CFLAGS += $(GAIM_CFLAGS) -fPIC
LDFLAGS += $(GAIM_LDFLAGS) -fPIC -shared

all: copystatus.so

copystatus.so: plugin.o
	$(LINK.o) -o $@ $^

install: copystatus.so
	mkdir -p $(PLUGINDIR)
	install copystatus.so $(PLUGINDIR)
	mkdir -p $(DOCDIR)
	install -m 644 COPYING README INSTALL ChangeLog NEWS $(DOCDIR)

clean:
	-rm -f *.o *.so
