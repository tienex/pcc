#	$Id$
#
# Makefile for libseh - SEH Runtime Library for PCC
#

CC?=		pcc
CXX?=		pcc++
AR?=		ar
RANLIB?=	ranlib
INSTALL?=	install

PREFIX?=	/usr/local
LIBDIR=		$(PREFIX)/lib
INCDIR=		$(PREFIX)/include

CFLAGS+=	-fPIC -Wall
CXXFLAGS+=	-fPIC -Wall
LDFLAGS+=	-shared

LIB=		libseh.a
SOLIB=		libseh.so
SOVERSION=	0.1

SRCS=		seh.c seh_dwarf.c seh_context.c
CXXSRCS=	seh_cxx.cpp
OBJS=		$(SRCS:.c=.o) $(CXXSRCS:.cpp=.o)

HEADERS=	seh.h

all: $(LIB) $(SOLIB)

$(LIB): $(OBJS)
	$(AR) rcs $@ $(OBJS)
	$(RANLIB) $@

$(SOLIB): $(OBJS)
	$(CC) $(LDFLAGS) -o $@.$(SOVERSION) $(OBJS)
	ln -sf $@.$(SOVERSION) $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

.cpp.o:
	$(CXX) $(CXXFLAGS) -c $< -o $@

install: $(LIB) $(SOLIB)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) -d $(DESTDIR)$(INCDIR)
	$(INSTALL) -m 644 $(LIB) $(DESTDIR)$(LIBDIR)/
	$(INSTALL) -m 755 $(SOLIB).$(SOVERSION) $(DESTDIR)$(LIBDIR)/
	ln -sf $(SOLIB).$(SOVERSION) $(DESTDIR)$(LIBDIR)/$(SOLIB)
	$(INSTALL) -m 644 $(HEADERS) $(DESTDIR)$(INCDIR)/

clean:
	rm -f $(OBJS) $(LIB) $(SOLIB) $(SOLIB).$(SOVERSION)

.PHONY: all install clean
