lib_LTLIBRARIES = libvm.la

libvm_la_SOURCES = vm.c
libvm_la_LDFLAGS = -version-info 0:0:0

include_HEADERS = vm.h

AM_CPPFLAGS = -I$(top_srcdir)/include

# Build standalone VM assembler tool
bin_PROGRAMS = vmasm vmdis vmrun

vmasm_SOURCES = vmasm.c
vmasm_LDADD = libvm.la

vmdis_SOURCES = vmdis.c
vmdis_LDADD = libvm.la

vmrun_SOURCES = vmrun.c
vmrun_LDADD = libvm.la

# Install bootstrap scripts
dist_bin_SCRIPTS = vmsh.sh vmbat.bat
