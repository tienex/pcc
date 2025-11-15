lib_LTLIBRARIES = libcoro.la

libcoro_la_SOURCES = coro.c
libcoro_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = coro.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	generic/coro_generic.c \
	unix/ \
	win32/ \
	dos/
