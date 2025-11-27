lib_LTLIBRARIES = libbgi.la
libbgi_la_SOURCES = bgi_new.c

EXTRA_DIST = \
	dos/bgi_dos.c \
	win32/bgi_win32.c \
	unix/bgi_unix.c
libbgi_la_LDFLAGS = -version-info 0:0:0
include_HEADERS = bgi.h
AM_CPPFLAGS = -I$(top_srcdir)/include
