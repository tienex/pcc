lib_LTLIBRARIES = libnet.la

libnet_la_SOURCES = net_new.c
libnet_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = net.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	win32/net_win32.c \
	unix/net_unix.c
