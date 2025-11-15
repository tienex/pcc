lib_LTLIBRARIES = libgthread.la

libgthread_la_SOURCES = gthread_new.c
libgthread_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = gthread.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	dos/gthread_dos.c \
	win16/gthread_win16.c \
	os2_16/gthread_os2_16.c \
	dos32/gthread_dos32.c \
	win32/gthread_win32.c \
	os2_32/gthread_os2_32.c \
	openvms/gthread_openvms.c \
	beos/gthread_beos.c \
	unix/gthread_unix.c
