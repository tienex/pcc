lib_LTLIBRARIES = libgc.la

libgc_la_SOURCES = gc_new.c
libgc_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = gc.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	unix/gc_unix.c \
	win32/gc_win32.c \
	dos/gc_dos.c
