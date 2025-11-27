lib_LTLIBRARIES = libexcept.la

libexcept_la_SOURCES = except_new.c
libexcept_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = except.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	unix/except_unix.c \
	win32/except_win32.c \
	dos/except_dos.c
