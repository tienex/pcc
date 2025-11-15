lib_LTLIBRARIES = libunwind.la

libunwind_la_SOURCES = unwind_new.c
libunwind_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = unwind.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	x86_64/unwind_x86_64.c \
	i386/unwind_i386.c \
	arm/unwind_arm.c \
	mips/unwind_mips.c \
	sparc/unwind_sparc.c \
	generic/unwind_generic.c
