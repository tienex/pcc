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

SRCS=		seh_new.c seh_context_new.c
CXXSRCS=	seh_cxx.cpp
OBJS=		$(SRCS:.c=.o) $(CXXSRCS:.cpp=.o)

# Platform-specific source files (included by dispatcher)
PLATFORM_SRCS=	\
	unix/seh_unix.c \
	unix/seh_context_unix.c \
	unix/seh_dwarf.c \
	win32/seh_win32.c \
	win32/seh_context_win32.c \
	win16/seh_win16.c \
	win16/seh_context_win16.c \
	dos/seh_dos.c \
	dos/seh_context_dos.c \
	dos32/seh_dos32.c \
	dos32/seh_context_dos32.c \
	os2_16/seh_os2_16.c \
	os2_16/seh_context_os2_16.c \
	os2_32/seh_os2_32.c \
	os2_32/seh_context_os2_32.c \
	openvms/seh_openvms.c \
	openvms/seh_context_openvms.c \
	beos/seh_beos.c \
	beos/seh_context_beos.c \
	amiga/seh_amiga.c \
	amiga/seh_context_amiga.c \
	atari/seh_atari.c \
	atari/seh_context_atari.c \
	macos/seh_macos.c \
	macos/seh_context_macos.c \
	netware/seh_netware.c \
	netware/seh_context_netware.c \
	uefi/seh_uefi.c \
	uefi/seh_context_uefi.c

EXTRA_DIST=	$(PLATFORM_SRCS) seh_helpers.h

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
