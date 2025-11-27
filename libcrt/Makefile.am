lib_LTLIBRARIES = libcrt.la

libcrt_la_SOURCES = crt_new.c
libcrt_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = crt.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	dos/crt_dos.c \
	win16/crt_win16.c \
	os2_16/crt_os2_16.c \
	dos32/crt_dos32.c \
	win32/crt_win32.c \
	os2_32/crt_os2_32.c \
	openvms/crt_openvms.c \
	beos/crt_beos.c \
	unix/crt_unix.c
