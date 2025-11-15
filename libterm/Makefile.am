lib_LTLIBRARIES = libterm.la

libterm_la_SOURCES = term.c
libterm_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = term.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	core/term_core.c \
	win32/term_win32.c \
	x11_motif/ \
	x11_xaw/ \
	atari/ \
	amiga/ \
	macos/ \
	win16/ \
	os2_16/ \
	os2_32/ \
	unix_curses/
