lib_LTLIBRARIES = libfs.la

libfs_la_SOURCES = fs.c
libfs_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = fs.h

AM_CPPFLAGS = -I$(top_srcdir)/include

EXTRA_DIST = \
	dos/fs_dos.c \
	win16/fs_win16.c \
	os2_16/fs_os2_16.c \
	dos32/fs_dos32.c \
	win32/fs_win32.c \
	os2_32/fs_os2_32.c \
	openvms/fs_openvms.c \
	beos/fs_beos.c \
	unix/fs_unix.c
