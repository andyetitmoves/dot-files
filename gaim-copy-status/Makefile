PREFIX ?= $(shell pkg-config --variable=prefix pidgin)
LIBDIR ?= $(shell pkg-config --variable=libdir pidgin)

PLUGINDIR ?= $(LIBDIR)/pidgin
DOCDIR ?= $(PREFIX)/share/doc/pidgin-copy-status

PIDGIN_CFLAGS = $(shell pkg-config --cflags pidgin gtk+-2.0)
PIDGIN_LDFLAGS = $(shell pkg-config --libs pidgin gtk+-2.0)

CFLAGS += $(PIDGIN_CFLAGS) -fPIC
LDFLAGS += $(PIDGIN_LDFLAGS) -fPIC -shared

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
