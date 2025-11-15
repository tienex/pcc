#	$Id$
#
# Makefile for libdosmem - DOS Memory Manager Library

CC?=		pcc
AR?=		ar
RANLIB?=	ranlib
INSTALL?=	install

PREFIX?=	/usr/local
LIBDIR=		$(PREFIX)/lib
INCDIR=		$(PREFIX)/include

CFLAGS+=	-Wall -O2
LDFLAGS+=

LIB=		libdosmem.a

# Core sources
CORE_SRCS=	dosmem.c vmem_compat.c

# Memory manager sources
DPMI_SRCS=	dpmi/dpmi.c
XMS_SRCS=	xms/xms.c
EMS_SRCS=	ems/ems.c
VCPI_SRCS=	vcpi/vcpi.c
HMA_SRCS=	hma/hma.c
UMB_SRCS=	umb/umb.c
SWAP_SRCS=	swap/swap.c

SRCS=		$(CORE_SRCS) $(DPMI_SRCS) $(XMS_SRCS) $(EMS_SRCS) $(VCPI_SRCS) \
		$(HMA_SRCS) $(UMB_SRCS) $(SWAP_SRCS)
OBJS=		$(SRCS:.c=.o)

HEADERS=	dosmem.h vmem_compat.h

all: $(LIB)

$(LIB): $(OBJS)
	$(AR) rcs $@ $(OBJS)
	$(RANLIB) $@

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@

install: $(LIB)
	$(INSTALL) -d $(DESTDIR)$(LIBDIR)
	$(INSTALL) -d $(DESTDIR)$(INCDIR)
	$(INSTALL) -m 644 $(LIB) $(DESTDIR)$(LIBDIR)/
	$(INSTALL) -m 644 $(HEADERS) $(DESTDIR)$(INCDIR)/

clean:
	rm -f $(OBJS) $(LIB)

distclean: clean
	rm -f *~

test: test_dosmem
	./test_dosmem

test_dosmem: test_dosmem.o $(LIB)
	$(CC) $(LDFLAGS) -o $@ test_dosmem.o $(LIB)

.PHONY: all install clean distclean test
