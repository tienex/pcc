lib_LTLIBRARIES = libvm.la

libvm_la_SOURCES = generic/vm.c
libvm_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = vm.h

AM_CPPFLAGS = -I$(top_srcdir)/include

# Build standalone VM assembler tool
bin_PROGRAMS = vmasm vmdis vmrun

vmasm_SOURCES = generic/vmasm.c
vmasm_LDADD = libvm.la

vmdis_SOURCES = generic/vmdis.c
vmdis_LDADD = libvm.la

vmrun_SOURCES = generic/vmrun.c
vmrun_LDADD = libvm.la

# Install bootstrap scripts
dist_bin_SCRIPTS = shell/vmsh.sh batch/vmbat.bat

EXTRA_DIST = \
	generic/vm.c \
	generic/vmasm.c \
	generic/vmdis.c \
	generic/vmrun.c \
	shell/vmsh.sh \
	batch/vmbat.bat \
	examples/
