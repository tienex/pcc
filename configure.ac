                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([Portable C Compiler], [1.2.0.DEVEL], [pcc@lists.ludd.ltu.se], [pcc], [http://pcc.ludd.ltu.se/])
AC_CONFIG_HEADERS([config.h])

AC_CANONICAL_TARGET

abi=unknown
endian=little
targosver=0
tls=no
gcccompat=yes
pccdebug=yes
stripping=yes
native=no
nativefp=yes
useyasm=no
stabs=no
dwarf=no
# allowed: UNSIGNED (4-char u_int), INT (4-char int), SHORT (2-char u_short)
wchar_type=INT
# Default assembler format (gnu-as, nasm, yasm, fasm, apple-as, masm, jwasm, uasm, tasm, wasm)
asm_format=gnu-as

case "$target_os" in

    apple)
	targos=apple
	abi=classic68k
	stabs=yes
	case "$target_cpu" in
	    m68k) targmach=m68k endian=big ;;
	esac
	;;

    bsd)
	targos=bsd
	abi=aout
	case "$target_cpu" in
	    pdp11) targmach=pdp11 ;;
	    nova) targmach=nova ;;
	esac
	wchar_type=USHORT
	;;

    darwin*)
	targos=darwin
	abi=macho
	stabs=yes
	asm_format=apple-as
	case "$target_os" in
	    *10.*) targosver=10 ;;
	    *9.*) targosver=9 ;;
	    *8.*) targosver=8 ;;
	    *7.*) targosver=7 ;;
	esac
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	    powerpc) targmach=powerpc endian=big ;;
	    x86_64) targmach=amd64 ;;
	esac
        ;;

    dragonfly*)
	targos=dragonfly
	abi=elf
	stabs=yes
	tls=yes
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	    x86_64) targmach=amd64 ;;
	esac
	;;

    freebsd*)
	targos=freebsd
	abi=elf
	stabs=yes
	case "$target_os" in
	    *10.*) targosver=10 ;;
	    *9.*) targosver=9 ;;
	    *8.*) targosver=8 ;;
	    *7.*) targosver=7 ;;
	    *6.*) targosver=6 ;;
	    *5.*) targosver=5 ;;
	    *4.*) targosver=4 ;;
	esac
	case "$target_cpu" in
	    i386) targmach=i386 ;;
	    sparc64) targmach=sparc64 endian=big ;;
	    x86_64) targmach=amd64 ;;
	esac
	;;

    linux-android*)
	targos=android
	abi=elf
	stabs=yes
	case "$target_cpu" in
	    arm*) targmach=arm ;;
	    i?86) targmach=i386 ;;
	    x86_64) targmach=amd64 ;;
	    mips64el) targmach=mips64 ;;
	    mips64) targmach=mips64 endian=big ;;
	    mipseb) targmach=mips endian=big ;;
	    mips*) targmach=mips ;;
	esac
	;;

    linux*)
	targos=linux
	abi=elf
	stabs=yes
	case "$target_cpu" in
	    arm*) targmach=arm ;;
	    i?86) targmach=i386 ;;
	    powerpc*) targmach=powerpc endian=big ;;
	    x86_64) targmach=amd64 ;;
	    mips64el) targmach=mips64 ;;
	    mips64) targmach=mips64 endian=big ;;
	    mipseb) targmach=mips endian=big ;;
	    mips*) targmach=mips ;;
	    wasm32) targmach=wasm ;;
	    wasm64) targmach=wasm ;;
	esac
	case "$target_os" in
	    *-musl*) ADD_CPPFLAGS="$ADD_CPPFLAGS -DUSE_MUSL" ;;
	esac
	;;

    litebsd*)
	targos=litebsd
	abi=elf
	case "$target_cpu" in
	    mips*) targmach=mips ;;
	esac
	;;

    midnightbsd*)
	targos=midnightbsd
	abi=elf
	stabs=yes
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	    sparc64) targmach=sparc64 endian=big ;;
	esac
	;;

    mingw*)
	targos=win32
	abi=pecoff
	wchar_type=USHORT
	targmach=i386
	altincdir="c:/mingw/include"
	altlibdir="c:/mingw/lib"
	;;

    clr*|dotnet*|msil*)
	# Microsoft CLR / .NET Framework target
	# Generates ILASM (CIL/MSIL) for the Common Language Runtime
	targos=clr
	abi=ilasm
	wchar_type=USHORT
	# Default to i386 for CLR metadata, can override with --target=x86_64-clr
	case "$target_cpu" in
	    i?86) targmach=clr ;;
	    x86_64) targmach=clr ;;  # CLR64 mode
	    *) targmach=clr ;;  # Default CLR (32-bit)
	esac
	;;

   minix*)
        targos=minix
        targosver=`uname -v` 
        stabs=yes
        case "$targosver" in
            # explicit setting
            1.*)
                # pcc works for 3.1.6
                # not tested for 3.1.7 and 3.1.8
                targosver=3.1.x
                abi=aout ;;  
           *3*)
                # Minix switched to ELF with 3.2
                targosver=3.2+
                abi=elf ;;
            *)
                # default to elf
               targosver=unknown
               abi=elf ;;
        esac     
        case "$target_cpu" in
            i86) targmach=i86 ;;
            i?86) targmach=i386 ;;
            arm*) targmach=arm ;;
            x86_64) targmach=amd64 ;;
        esac
        ;; 

    mirbsd*)
	targos=mirbsd
	abi=elf
	stabs=yes
	wchar_type=USHORT
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	esac
	;;

    netbsd*)
	targos=netbsd
	abi=elf
	stabs=yes
	case "$target_os" in
	    *7.*) targosver=7 ;;
	    *6.*) targosver=6 ;;
	    *5.*) targosver=5 ;;
	    *4.*) targosver=4 ;;
	    *3.*) targosver=3 ;;
	    *2.*) targosver=2 ;;
	    *1.*) targosver=1 ;;
	esac
	case "$target_cpu" in
	    armeb) targmach=arm endian=big ;;
	    arm*) targmach=arm ;;
	    i?86) targmach=i386 ;;
	    m68k*) targmach=m68k endian=big ;;
	    mips64el) targmach=mips64 ;;
	    mips64) targmach=mips64 endian=big ;;
	    mipseb) targmach=mips endian=big ;;
	    mips*) targmach=mips ;;
	    pdp10) targmach=pdp10 ;;
	    powerpc) targmach=powerpc endian=big ;;
	    riscv32) targmach=riscv32 targmachdir=riscv ;;
	    riscv64) targmach=riscv64 targmachdir=riscv ;;
	    sparc64) targmach=sparc64 endian=big ;;
	    vax) targmach=vax ;;
	    x86_64) targmach=amd64 ;;
	esac
	;;

    nextstep*)
	targos=nextstep
	abi=macho
	stabs=yes
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	    sparc) targmach=sparc endian=big ;;
	    hppa) targmach=hppa endian=big ;;
	esac
	;;

    openbsd*)
	targos=openbsd
	abi=elf
	stabs=yes
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	    vax) targmach=vax ;;
	    mips64el) targmach=mips64 ;;
	    mips64) targmach=mips64 endian=big ;;
	    powerpc) targmach=powerpc endian=big ;;
	    sparc64) targmach=sparc64 endian=big ;;
	    m68k) targmach=m68k endian=big ;;
	    x86_64) targmach=amd64 ;;
	esac
	;;

    sysv4*)
	targos=sysv4
	abi=elf
	case "$target_cpu" in
	    i?86) targmach=i386 ;;
	esac
	;;

    sunos*|solaris*)
        targos=sunos
        abi=elf
	stabs=yes
        case "$target_cpu" in
            i?86) targmach=i386 ;;
            sparc*) targmach=sparc64 endian=big ;;
        esac
        ;;

    wasm*)
	targos=wasm
	abi=elf
	case "$target_cpu" in
	    wasm32) targmach=wasm ;;
	    wasm64) targmach=wasm ;;
	esac
	;;

    windows*|pe*)
	target_alias=i386-pe
	targos=win32
	abi=pecoff
	wchar_type=USHORT
	targmach=i386
	;;

    *)
        targos="$target_os"
	case "$target_cpu" in
	    m16c) targmach=m16c ;;
	    nova) targmach=nova ;;
	    i86) targmach=i86 ;;
	    pdp7) targmach=pdp7 ;;
	    wasm32) targmach=wasm ;;
	    wasm64) targmach=wasm ;;
	esac
	;;
esac

if test "X$targos" = X -o "X$targmach" = X ; then
	AC_MSG_ERROR(['$target' is not (yet) supported by pcc.])
fi

if test "X$targmachdir" = X ; then
	targmachdir=$targmach
fi

case "$host_os" in

    apple)
        hostos=apple
	;;
    bsd)
        hostos=bsd
	;;
    darwin*)
	hostos=darwin
	;;
    dragonfly*)
        hostos=dragonfly
	;;
    freebsd*)
        hostos=freebsd
	;;
    linux*)
	ADD_CPPFLAGS="$ADD_CPPFLAGS -D_DEFAULT_SOURCE"
        hostos=linux
	;;
    litebsd*)
	hostos=litebsd
	;;
    midnightbsd*)
        hostos=midnightbsd
	;;
    mingw*)
        hostos=win32
	;;
    minix*)
        hostos=minix
	;;
    mirbsd*)
        hostos=mirbsd
	;;
    netbsd*)
        hostos=netbsd
	;;
    nextstep*)
        hostos=nextstep
	;;
    openbsd*)
        hostos=openbsd
	;;
    sunos*|solaris*)
	ADD_CPPFLAGS="$ADD_CPPFLAGS -D_XOPEN_SOURCE=600"
        hostos=sunos
	;;
    pe*|windows*)
	# quick hack for cross-build to win32 host
	hostos=win32
	if "$prefix" = NONE; then
		prefix="c:/pcc"
		assembler="yasm.exe -p gnu -f win32"
		linker="link.exe /nologo"
		ADD_CPPFLAGS="$ADD_CPPFLAGS -DMSLINKER"
	fi
	;;

esac

if test "X$endian" = "Xbig" ; then
	AC_DEFINE(TARGET_BIG_ENDIAN, 1,
		[Define if target defaults to BIG endian])
else
	AC_DEFINE(TARGET_LITTLE_ENDIAN, 1,
		[Define if target defaults to LITTLE endian])
fi

case "$abi" in
	elf*)		AC_DEFINE(ELFABI, 1, [Using ELF ABI]) ;;
	aout)		AC_DEFINE(AOUTABI, 1, [Using a.out ABI]) ;;
	macho)		AC_DEFINE(MACHOABI, 1, [Using Mach-O ABI]) ;;
	coff)		AC_DEFINE(COFFABI, 1, [Using COFF ABI]) ;;
	ecoff)		AC_DEFINE(ECOFFABI, 1, [Using ECOFF ABI]) ;;
	pecoff)		AC_DEFINE(PECOFFABI, 1, [Using PE/COFF ABI]) ;;
	classic68k)	AC_DEFINE(CLASSIC68K, 1, [Using Classic 68k ABI]) ;;
esac

if test "$stabs" = "yes"; then
	AC_DEFINE(STABS, 1, [Enable STABS debugging output])
fi

if test "$dwarf" = "yes"; then
	AC_DEFINE(DWARF, 1, [Enable DWARF debugging output])
fi

# Specify alternate assembler, linker, include and lib paths
AC_ARG_ENABLE(multiarch,
	AS_HELP_STRING([--enable-multiarch=yes/no/auto/<triplet>],
		[Enable use of Linux Multi-Arch paths (default: auto)]),
	[multiarch=$enableval], [multiarch=auto])
AC_ARG_WITH(incdir,
	AS_HELP_STRING([--with-incdir=<path>],
		[Specify the default include path.]),
	altincdir=$withval,
	[])
AC_ARG_WITH(libdir,
	AS_HELP_STRING([--with-libdir=<path>],
		[Specify the default library path.]),
	altlibdir=$withval,
	[])
AC_ARG_WITH(assembler,
	AS_HELP_STRING([--with-assembler=<path>],
		[Specify alternate assember.]),
	assembler=$withval,
	[])
AC_ARG_WITH(linker,
	AS_HELP_STRING([--with-linker=<path>],
		[Specify alternate linker.]),
	linker=$withval,
	[])
AC_ARG_WITH(libvmf,
	AS_HELP_STRING([--with-libvmf=<path>],
		[Use libvmf.]),
	libvmf=$withval,
	libvmf=no)
AC_ARG_ENABLE(tls,
	AS_HELP_STRING([--enable-tls],
		[Enable Thread-local storage (TLS).]),
	[tls=$enableval], [])
if test "$tls" = "yes"; then
	AC_DEFINE(TLS, 1, [Enable thread-local storage (TLS).])
fi
AC_ARG_ENABLE(Werror,
	AS_HELP_STRING([--enable-Werror],
		[Enable use of compiler -Werror flag]),
	[werror=$enableval], [])
if test "$werror" = "yes"; then
	ADD_CFLAGS="$ADD_CFLAGS -Werror"
fi
AC_ARG_ENABLE(gcc-compat,
	AS_HELP_STRING([--disable-gcc-compat],
		[Disable GCC compatibility]),
	[gcccompat=$enableval], [])
if test "$gcccompat" = "yes"; then
	ADD_CPPFLAGS="$ADD_CPPFLAGS -DGCC_COMPAT";
fi
AC_ARG_ENABLE(pcc-debug,
	AS_HELP_STRING([--disable-pcc-debug],
		[Disable PCC debugging]),
	[pccdebug=$enableval], [])
if test "$pccdebug" = "yes"; then
	ADD_CPPFLAGS="$ADD_CPPFLAGS -DPCC_DEBUG";
fi
AC_ARG_ENABLE(twopass,
	AS_HELP_STRING([--enable-twopass],
		[Link PCC as a two-pass compiler]),
	[twopass=$enableval], [])
if test "$twopass" = "yes"; then
	ADD_CPPFLAGS="$ADD_CPPFLAGS -DTWOPASS";
	CCNAMES='$(BINPREFIX)cc0$(EXEEXT) $(BINPREFIX)cc1$(EXEEXT)'
	CF0='-DPASS1'
	CF1='-DPASS2'
else
	CCNAMES='$(BINPREFIX)ccom$(EXEEXT)'
fi

AC_ARG_ENABLE(stripping,
	AS_HELP_STRING([--disable-stripping],
		[Disable stripping of symbols in installed binaries]),
	[stripping=$enableval], [])
if test "$stripping" = "yes"; then
	if test -z "$INSTALL_PROGRAM"; then
		INSTALL_PROGRAM='${INSTALL} -s'
	else
		AC_MSG_WARN([Installed binaries may be unstripped])
	fi
fi

AC_ARG_ENABLE(nativefp,
	AS_HELP_STRING([--disable-nativefp],
		[Disable use of compiler host floating point.]),
	[nfp=$enableval], [])
if test "$nfp" = "no"; then
	nativefp=no
fi
if test "$nativefp" = "yes"; then
	ADD_CPPFLAGS="$ADD_CPPFLAGS -DNATIVE_FLOATING_POINT"
fi

AC_ARG_WITH(yasm,
	AS_HELP_STRING([--with-yasm], [Use yasm assembler]),
	useyasm=$withval,
	[])
if test "$useyasm" = "yes"; then
	assembler="yasm"
	ADD_CPPFLAGS="$ADD_CPPFLAGS -DUSE_YASM"
fi
AC_ARG_WITH(asm-format,
	AS_HELP_STRING([--with-asm-format=<format>],
		[Specify assembly output format: gnu-as (default), nasm, yasm, fasm, apple-as, masm, jwasm, uasm, tasm, wasm]),
	asm_format=$withval,
	[])
AC_ARG_ENABLE(native,
	AS_HELP_STRING([--enable-native],
		[Build the compiler as a native rather than cross-build compiler]),
	[native=$enableval], [])

# Language selection options
AC_ARG_ENABLE(languages,
	AS_HELP_STRING([--enable-languages=LANGLIST],
		[Comma-separated list of languages to build: c,c++,pascal,f77,bliss (default: c,c++)]),
	[enable_languages=$enableval], [enable_languages=c,c++])

# Architecture selection options
AC_ARG_ENABLE(archs,
	AS_HELP_STRING([--enable-archs=ARCHLIST],
		[Comma-separated list of target architectures (default: host architecture)]),
	[enable_archs=$enableval], [enable_archs=auto])

# OS target selection options
AC_ARG_ENABLE(targets,
	AS_HELP_STRING([--enable-targets=OSLIST],
		[Comma-separated list of target operating systems (default: host OS)]),
	[enable_targets=$enableval], [enable_targets=auto])

# Bootstrap options
bootstrap_stage=0
AC_ARG_ENABLE(bootstrap,
	AS_HELP_STRING([--enable-bootstrap@<:@=ARG@:>@],
		[Enable multi-stage bootstrap build (yes/no/lean, default: no)
		 - yes: 3-stage bootstrap with stage2/stage3 comparison
		 - lean: 2-stage bootstrap without comparison
		 - no: single-stage build (default)]),
	[bootstrap=$enableval], [bootstrap=no])
AC_ARG_WITH(bootstrap-stage,
	AS_HELP_STRING([--with-bootstrap-stage=N],
		[Set bootstrap stage number (default: 0)]),
	[bootstrap_stage=$withval], [])

# Normalize bootstrap values
case "$bootstrap" in
yes|"")
	bootstrap=yes
	bootstrap_stages=3
	bootstrap_compare=yes
	;;
lean)
	bootstrap=yes
	bootstrap_stages=2
	bootstrap_compare=no
	;;
no)
	bootstrap=no
	bootstrap_stages=1
	bootstrap_compare=no
	;;
*)
	AC_MSG_ERROR([Invalid --enable-bootstrap argument '$bootstrap'. Use yes, lean, or no.])
	;;
esac

if test "$bootstrap" = "yes" -o "$bootstrap_stage" != "0"; then
	AC_DEFINE_UNQUOTED(BOOTSTRAP_STAGE, $bootstrap_stage,
		[Bootstrap stage number])
fi
AC_SUBST(bootstrap)
AC_SUBST(bootstrap_stage)
AC_SUBST(bootstrap_stages)
AC_SUBST(bootstrap_compare)
AC_SUBST(asm_format)

# Process language selection
enable_c=no
enable_cxx=no
enable_pascal=no
enable_f77=no
enable_basic=no
enable_bliss=no
enable_pxc=no

# Normalize language list (replace spaces with commas)
enable_languages=`echo "$enable_languages" | tr ' ' ',' | tr -d ' '`

# Parse comma-separated language list using portable shell syntax
old_IFS=$IFS
IFS=','
for lang in $enable_languages; do
	case "$lang" in
	c|C)
		enable_c=yes
		;;
	c++|C++|cxx|CXX)
		enable_cxx=yes
		;;
	pascal|Pascal|PASCAL)
		enable_pascal=yes
		;;
	f77|F77|fortran|FORTRAN)
		enable_f77=yes
		;;
	basic|BASIC|bas|BAS)
		enable_basic=yes
		;;
	bliss|BLISS)
		enable_bliss=yes
		;;
	pxc|PXC|xbase++|xbasepp|XBASEPP)
		enable_pxc=yes
		;;
	all|ALL)
		enable_c=yes
		enable_cxx=yes
		enable_pascal=yes
		enable_f77=yes
		enable_basic=yes
		enable_bliss=yes
		enable_pxc=yes
		;;
	"")
		# Skip empty entries
		;;
	*)
		AC_MSG_ERROR([Unknown language '$lang'. Supported: c, c++, pascal, f77, basic, bliss, pxc, all])
		;;
	esac
done
IFS=$old_IFS

# C is always required for bootstrap
if test "$enable_c" = "no"; then
	AC_MSG_WARN([C language is required and will be enabled])
	enable_c=yes
fi

AC_SUBST(enable_c)
AC_SUBST(enable_cxx)
AC_SUBST(enable_pascal)
AC_SUBST(enable_f77)
AC_SUBST(enable_basic)
AC_SUBST(enable_bliss)
AC_SUBST(enable_pxc)

# Process architecture selection
if test "$enable_archs" = "auto" -o "$enable_archs" = ""; then
	# Default to target machine
	enable_archs="$targmach"
fi

# Process OS target selection
if test "$enable_targets" = "auto" -o "$enable_targets" = ""; then
	# Default to target OS
	enable_targets="$targos"
fi

AC_SUBST(enable_archs)
AC_SUBST(enable_targets)

# Setup for ubuntu multiarch
multiarch_path=
case x$multiarch in
xno)
	;;
xyes)
	multiarch_path=`dpkg-architecture -qDEB_HOST_MULTIARCH 2>/dev/null` || multiarch_path=
	case $multiarch_path in
	*-*-*) ;;
	*)
		AC_MSG_ERROR([Cannot determine Multi-Arch path '$multiarch_path'!])
		;;
	esac
	;;
xauto|x)
	multiarch_path=`dpkg-architecture -qDEB_HOST_MULTIARCH 2>/dev/null` || multiarch_path=
	case x$multiarch_path in
	x*-*-*) ;;
	x) ;;
	*)
		AC_MSG_WARN([Ignoring unrecognised Multi-Arch path '$multiarch_path'!])
		multiarch_path=
		;;
	esac
	;;
x*-*-*)
	multiarch_path=$multiarch
	;;
*)
	AC_MSG_ERROR([Ignoring unrecognised Multi-Arch path '$multiarch_path'!])
	;;
esac
if test -n "$multiarch_path"; then
	AC_DEFINE_UNQUOTED([MULTIARCH_PATH], ["$multiarch_path"],
		[Define target Multi-Arch path])
	multiarch="\"$multiarch_path\""
else
	multiarch="(no)"
fi
# setup for building a cross-compiler
if test "X$native" = "Xyes" -o "X$target_alias" = "X$host_alias" -o "X$target_alias" = "X"; then
	BINPREFIX=""
else
	BINPREFIX="${target_alias}-"
	test "X$prefix" = XNONE && prefix="$ac_default_prefix"
	test "X$exec_prefix" = XNONE && exec_prefix="${prefix}"
	if test -z "$altincdir"; then
		altincdir=${exec_prefix}/${target_alias}/include
	fi
	if test -z "$altlibdir"; then
		altlibdir=${exec_prefix}/${target_alias}/lib
	fi
	if test -z "$assembler"; then
		assembler=${BINPREFIX}as
	fi
	if test -z  "$linker"; then
		linker=${BINPREFIX}ld
	fi
	preprocessor="${BINPREFIX}cpp"
	compiler="${BINPREFIX}ccom"
fi
AC_SUBST(BINPREFIX)

if test -n "$altincdir"; then
	AC_DEFINE_UNQUOTED(STDINC, "$altincdir",
		[Define alternate standard include directory])
fi
if test -n "$altlibdir"; then
	AC_DEFINE_UNQUOTED(LIBDIR, "${altlibdir}/",
		[Define alternate standard lib directory])
fi
if test -n "$assembler"; then
	AC_DEFINE_UNQUOTED(ASSEMBLER, "$assembler",
		[Define path to alternate assembler])
fi
if test -n "$asm_format"; then
	AC_DEFINE_UNQUOTED(ASM_FORMAT, "$asm_format",
		[Define default assembly output format])
fi
if test -n "$linker"; then
	AC_DEFINE_UNQUOTED(LINKER, "$linker",
		[Define path to alternate linker])
fi
if test -n "$preprocessor"; then
	AC_DEFINE_UNQUOTED(PREPROCESSOR, "$preprocessor",
		[Define path to alternate preprocessor])
fi
if test -n "$compiler"; then
	AC_DEFINE_UNQUOTED(COMPILER, "$compiler",
		[Define path to alternate compiler])
fi

case $wchar_type in
USHORT) wchar_size=2 ;;
UNSIGNED|INT) wchar_size=4 ;;
*) AC_MSG_ERROR([Unknown wchar_t '$wchar_type'.]) ;;
esac

AC_DEFINE_UNQUOTED(WCHAR_TYPE, $wchar_type, [Type to use for wide characters])
AC_DEFINE_UNQUOTED(WCHAR_SIZE, $wchar_size, [Size of wide-character type in chars])

# check for additional compiler flags
AC_PROG_CC
DESIRED_FLAGS="-Wall -Wmissing-prototypes -Wstrict-prototypes -Wshadow -Wsign-compare -Wtruncate"
for flag in $DESIRED_FLAGS
do
	AC_MSG_CHECKING([whether $CC accepts $flag])
	cflags="$CFLAGS"
	CFLAGS="$CFLAGS $flag -Werror"
	AC_COMPILE_IFELSE([
	    AC_LANG_PROGRAM([[]], [[]])
	  ], [
	    use_flag=yes
	  ], [
	    use_flag=no
	])
	CFLAGS="$cflags"

	AC_MSG_RESULT([$use_flag])
	if test $use_flag = yes; then
	    ADD_CFLAGS="$ADD_CFLAGS $flag"
	fi
done

# setup for cross-compiling mkext
AC_MSG_CHECKING([for a C compiler for mkext])
if test $cross_compiling = yes; then
        AC_MSG_RESULT([cross compiling])
        AC_CHECK_PROGS(CC_FOR_BUILD, [pcc gcc cc])
else
        AC_MSG_RESULT([not cross compiling])
        CC_FOR_BUILD=${CC-cc}
        AC_SUBST(CC_FOR_BUILD)
fi

AC_CACHE_CHECK([for C99 printf size specifiers], ac_cv_have_c99_format, [
  AC_RUN_IFELSE([
    AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT], [[
      char buf[64];
      if (sprintf(buf, "%lld%hhd%jd%zd%td", (long long int)1, (char)2, (intmax_t)3, (size_t)4, (ptrdiff_t)5) != 5)
        exit(1);
      else if (strcmp(buf, "12345"))
        exit(2);
      ]])],
    [ ac_cv_have_c99_format=yes ],
    [ ac_cv_have_c99_format=no ],
    [ ac_cv_have_c99_format=yes ])
])
if test $ac_cv_have_c99_format = yes; then
  AC_DEFINE([HAVE_C99_FORMAT], 1,
    [Define to 1 if printf supports C99 size specifiers])
fi

AC_CHECK_SIZEOF([int *])

# Byteorder of host
AC_C_BIGENDIAN([AC_DEFINE(HOST_BIG_ENDIAN, 1, [Define if host is BIG endian])],
	[AC_DEFINE(HOST_LITTLE_ENDIAN, 1, [Define if host is LITTLE endian])],
	[])

# Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_INSTALL
AC_PROG_RANLIB
AC_PROG_YACC
AC_PROG_LEX([noyywrap])
AC_PROG_RANLIB

# Checks for libraries.
if test $libvmf != no ; then
	LIBS=-lvmf
	AC_DEFINE(LIBVMF, 1, [Use libvmf.])
if test $libvmf != yes ; then
	LDFLAGS=-L$libvmf
fi
fi

# Check for C11 standard support
AC_MSG_CHECKING([for C11 support])
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -std=c11"
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([[
        #if __STDC_VERSION__ < 201112L
        #error C11 not supported
        #endif
    ]], [[]])
  ], [
    have_c11=yes
    C11_CFLAGS="-std=c11"
  ], [
    have_c11=no
    C11_CFLAGS=""
])
CFLAGS="$save_CFLAGS"
AC_MSG_RESULT([$have_c11])
AC_SUBST(C11_CFLAGS)

# Check for C23 standard support
AC_MSG_CHECKING([for C23 support])
save_CFLAGS="$CFLAGS"
CFLAGS="$CFLAGS -std=c2x"
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([[
        #if __STDC_VERSION__ < 202000L
        #error C23 not supported
        #endif
    ]], [[]])
  ], [
    have_c23=yes
    C23_CFLAGS="-std=c2x"
  ], [
    have_c23=no
    C23_CFLAGS=""
])
CFLAGS="$save_CFLAGS"
AC_MSG_RESULT([$have_c23])
AC_SUBST(C23_CFLAGS)

# Checks for header files.
# AC_CHECK_HEADERS([fcntl.h stdlib.h string.h unistd.h])
AC_CHECK_HEADERS([string.h malloc.h libgen.h])
AC_HEADER_SYS_WAIT

# Check for C11 Unicode support (uchar.h)
AC_CHECK_HEADERS([uchar.h], [have_uchar=yes], [have_uchar=no])

# Check for C11 threads support (threads.h)
AC_CHECK_HEADERS([threads.h], [have_threads=yes], [have_threads=no])

# Check for C23 bit utilities (stdbit.h)
AC_CHECK_HEADERS([stdbit.h], [have_stdbit=yes], [have_stdbit=no])

# Check for C23 checked integer arithmetic (stdckdint.h)
AC_CHECK_HEADERS([stdckdint.h], [have_stdckdint=yes], [have_stdckdint=no])

# Check for IEEE 754-2008 decimal floating point support
AC_MSG_CHECKING([for _Decimal64 support])
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([[
        #ifndef __STDC_DEC_FP__
        /* Try to use _Decimal64 anyway */
        _Decimal64 x;
        #endif
    ]], [[
        _Decimal64 x = 1.0DD;
        x = x + 1.0DD;
    ]])
  ], [have_decimal=yes], [have_decimal=no])
AC_MSG_RESULT([$have_decimal])

# Check for POSIX threads if C11 threads not available
PTHREAD_LIBS=""
if test "$have_threads" = "no"; then
    AC_SEARCH_LIBS([pthread_create], [pthread], [
        if test "$ac_cv_search_pthread_create" != "none required"; then
            PTHREAD_LIBS="$ac_cv_search_pthread_create"
        fi
        have_pthreads=yes
    ], [have_pthreads=no])
fi
AC_SUBST(PTHREAD_LIBS)

# Determine which libraries to build
BUILD_LIBUNICODE=no
BUILD_LIBTHREAD=no
BUILD_LIBSTDBIT=no
BUILD_LIBCKDINT=no
BUILD_LIBDECIMAL=no
BUILD_LIBMETAWARE=yes  # Always build MetaWare extensions
PORTABLE_LIBS=""

if test "$have_uchar" = "no"; then
    BUILD_LIBUNICODE=yes
    PORTABLE_LIBS="$PORTABLE_LIBS libunicode"
    AC_DEFINE(NEED_LIBUNICODE, 1, [Define if portable libunicode should be linked])
    AC_MSG_NOTICE([Will build portable libunicode (C11 Unicode support)])
fi

if test "$have_threads" = "no"; then
    BUILD_LIBTHREAD=yes
    PORTABLE_LIBS="$PORTABLE_LIBS libthread"
    AC_DEFINE(NEED_LIBTHREAD, 1, [Define if portable libthread should be linked])
    AC_MSG_NOTICE([Will build portable libthread (C11 threading support)])
fi

if test "$have_stdbit" = "no"; then
    BUILD_LIBSTDBIT=yes
    PORTABLE_LIBS="$PORTABLE_LIBS libstdbit"
    AC_DEFINE(NEED_LIBSTDBIT, 1, [Define if portable libstdbit should be linked])
    AC_MSG_NOTICE([Will build portable libstdbit (C23 bit utilities)])
fi

if test "$have_stdckdint" = "no"; then
    BUILD_LIBCKDINT=yes
    PORTABLE_LIBS="$PORTABLE_LIBS libckdint"
    AC_DEFINE(NEED_LIBCKDINT, 1, [Define if portable libckdint should be linked])
    AC_MSG_NOTICE([Will build portable libckdint (C23 checked arithmetic)])
fi

if test "$have_decimal" = "no"; then
    BUILD_LIBDECIMAL=yes
    PORTABLE_LIBS="$PORTABLE_LIBS libdecimal"
    AC_DEFINE(NEED_LIBDECIMAL, 1, [Define if portable libdecimal should be linked])
    AC_MSG_NOTICE([Will build portable libdecimal (IEEE 754-2008 decimal FP)])
fi

# Always build MetaWare extensions library (it's opt-in via header)
PORTABLE_LIBS="$PORTABLE_LIBS libmetaware"
AC_MSG_NOTICE([Will build MetaWare High C extensions library (libmetaware)])

AC_SUBST(BUILD_LIBUNICODE)
AC_SUBST(BUILD_LIBTHREAD)
AC_SUBST(BUILD_LIBSTDBIT)
AC_SUBST(BUILD_LIBCKDINT)
AC_SUBST(BUILD_LIBDECIMAL)
AC_SUBST(BUILD_LIBMETAWARE)
AC_SUBST(PORTABLE_LIBS)

# Checks for library functions.
##  AC_FUNC_STRTOD
# AC_FUNC_VPRINTF
# AC_CHECK_FUNCS([memset strchr strdup strrchr strtol])
AC_CHECK_FUNCS([strtold vsnprintf snprintf mkstemp strlcat strlcpy getopt ffs vfork])
AC_FUNC_ALLOCA

AC_EXEEXT

AC_SUBST(targos)
AC_SUBST(targosver)
AC_SUBST(targmach)
AC_SUBST(targmachdir)
AC_SUBST(hostos)
AC_SUBST(prefix)
AC_SUBST(exec_prefix)
AC_SUBST(libexecdir)
AC_SUBST(includedir)
AC_SUBST(PACKAGE_VERSION)
AC_SUBST(ADD_CFLAGS)
AC_SUBST(ADD_CPPFLAGS)
AC_SUBST(CCNAMES)
AC_SUBST(CF0)
AC_SUBST(CF1)
AC_SUBST(LIBS)
AC_SUBST(LDFLAGS)

pcc_major=`echo $PACKAGE_VERSION | awk -F. '{print $1}'`
pcc_minor=`echo $PACKAGE_VERSION | awk -F. '{print $2}'`
pcc_minorminor=`echo $PACKAGE_VERSION | awk -F. '{print $3}'`
test -n "$MPVERSION" && MPVERSION=", $MPVERSION"
versstr="\"$PACKAGE_STRING `cat $srcdir/DATESTAMP` for $target$MPVERSION\""
targstr="\"$target$MPVERSION\""

AC_DEFINE_UNQUOTED(PCC_MAJOR, $pcc_major, [Major version no])
AC_DEFINE_UNQUOTED(PCC_MINOR, $pcc_minor, [Minor version no])
AC_DEFINE_UNQUOTED(PCC_MINORMINOR, $pcc_minorminor, [Minor minor version no])
AC_DEFINE_UNQUOTED(VERSSTR, $versstr, [Version string])
AC_DEFINE_UNQUOTED(TARGSTR, $targstr, [Target string])

# Find compiler runtime library directory for crtbegin.o/crtend.o (or MSVC equivalent)
# Supports GCC, Clang, and MSVC
gcclibdir=""

# Detect compiler type
cc_type=""
if test -n "$CC" ; then
	cc_type=`$CC --version 2>/dev/null | head -1`
	case "$cc_type" in
	*clang*|*Clang*|*CLANG*)
		cc_type="clang"
		;;
	*gcc*|*GCC*|*GNU*)
		cc_type="gcc"
		;;
	*Microsoft*|*MSVC*|*msvc*)
		cc_type="msvc"
		;;
	*)
		# Default: try gcc if CC not set or unrecognized
		cc_type="gcc"
		;;
	esac
else
	# No CC set, try to auto-detect
	if command -v gcc >/dev/null 2>&1; then
		cc_type="gcc"
	elif command -v clang >/dev/null 2>&1; then
		cc_type="clang"
	elif command -v cl.exe >/dev/null 2>&1; then
		cc_type="msvc"
	else
		cc_type="gcc"  # fallback
	fi
fi

# Find library directory based on compiler type
case "$cc_type" in
gcc)
	# GCC: Find version and library directory
	gccver=`$CC -dumpversion 2>/dev/null || gcc -dumpversion 2>/dev/null | cut -d. -f1`
	if test -n "$gccver" ; then
		case "$target_cpu" in
		x86_64|amd64)
			gcclibdir="/usr/lib/gcc/x86_64-linux-gnu/$gccver"
			;;
		i?86)
			gcclibdir="/usr/lib/gcc/i686-linux-gnu/$gccver"
			;;
		aarch64|arm64)
			gcclibdir="/usr/lib/gcc/aarch64-linux-gnu/$gccver"
			;;
		arm*)
			gcclibdir="/usr/lib/gcc/arm-linux-gnueabihf/$gccver"
			;;
		esac
	fi
	# Fallback for GCC
	if test -z "$gcclibdir" ; then
		gcclibdir="/usr/lib/gcc/x86_64-linux-gnu/4.4"
	fi
	;;

clang)
	# Clang: Find version and library directory
	clangver=`$CC --version 2>/dev/null | head -1 | sed 's/.*version \([[0-9]]*\)\..*/\1/'`
	if test -z "$clangver" ; then
		clangver=`clang --version 2>/dev/null | head -1 | sed 's/.*version \([[0-9]]*\)\..*/\1/'`
	fi

	# Try to find Clang's resource directory
	if test -n "$clangver" ; then
		case "$target_os" in
		*darwin*|*macos*)
			# macOS: Clang uses Xcode or CommandLineTools paths
			if test -d "/Library/Developer/CommandLineTools/usr/lib/clang/$clangver" ; then
				gcclibdir="/Library/Developer/CommandLineTools/usr/lib/clang/$clangver/lib/darwin"
			elif test -d "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/$clangver" ; then
				gcclibdir="/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/$clangver/lib/darwin"
			fi
			;;
		*linux*)
			# Linux: Clang often uses GCC's libraries for compatibility
			# First try clang's own runtime
			if test -d "/usr/lib/clang/$clangver/lib/linux" ; then
				gcclibdir="/usr/lib/clang/$clangver/lib/linux"
			elif test -d "/usr/lib/llvm-$clangver/lib/clang/$clangver/lib/linux" ; then
				gcclibdir="/usr/lib/llvm-$clangver/lib/clang/$clangver/lib/linux"
			else
				# Fall back to GCC's crtbegin.o for Clang on Linux
				gccver=`gcc -dumpversion 2>/dev/null | cut -d. -f1`
				if test -n "$gccver" ; then
					case "$target_cpu" in
					x86_64|amd64)
						gcclibdir="/usr/lib/gcc/x86_64-linux-gnu/$gccver"
						;;
					i?86)
						gcclibdir="/usr/lib/gcc/i686-linux-gnu/$gccver"
						;;
					esac
				fi
			fi
			;;
		*bsd*)
			# BSD: Check /usr/lib/clang
			if test -d "/usr/lib/clang/$clangver" ; then
				gcclibdir="/usr/lib/clang/$clangver/lib"
			fi
			;;
		esac
	fi

	# Fallback for Clang (use GCC's on Linux)
	if test -z "$gcclibdir" ; then
		case "$target_os" in
		*linux*)
			gcclibdir="/usr/lib/gcc/x86_64-linux-gnu/4.4"
			;;
		*darwin*)
			gcclibdir="/Library/Developer/CommandLineTools/usr/lib/clang/13/lib/darwin"
			;;
		*)
			gcclibdir="/usr/lib"
			;;
		esac
	fi
	;;

msvc)
	# MSVC: Find Visual Studio installation
	# Check common Visual Studio paths
	if test -d "C:/Program Files (x86)/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC" ; then
		msvcver=`ls "C:/Program Files (x86)/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC" | tail -1`
		case "$target_cpu" in
		x86_64|amd64)
			gcclibdir="C:/Program Files (x86)/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/$msvcver/lib/x64"
			;;
		i?86)
			gcclibdir="C:/Program Files (x86)/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/$msvcver/lib/x86"
			;;
		esac
	elif test -d "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC" ; then
		msvcver=`ls "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC" | tail -1`
		case "$target_cpu" in
		x86_64|amd64)
			gcclibdir="C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/$msvcver/lib/x64"
			;;
		i?86)
			gcclibdir="C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/$msvcver/lib/x86"
			;;
		esac
	elif test -d "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC" ; then
		msvcver=`ls "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC" | tail -1`
		case "$target_cpu" in
		x86_64|amd64)
			gcclibdir="C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/$msvcver/lib/x64"
			;;
		i?86)
			gcclibdir="C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/$msvcver/lib/x86"
			;;
		esac
	fi

	# Fallback for MSVC
	if test -z "$gcclibdir" ; then
		gcclibdir="C:/Program Files (x86)/Microsoft Visual Studio/14.0/VC/lib"
	fi
	;;
esac

AC_SUBST(gcclibdir)

# Build list of Makefiles to configure based on enabled languages
CONFIG_MAKEFILES="Makefile
		common/abi/Makefile
		include/Makefile
		libunicode/Makefile
		libthread/Makefile
		libstdbit/Makefile
		libckdint/Makefile
		libdecimal/Makefile
		libmetaware/Makefile
		libblocks/Makefile
		libpascal/Makefile
		libwirth/Makefile
		libmangle/Makefile
		libgc/Makefile
		libexcept/Makefile
		libustring/Makefile
		libgthread/Makefile
		libnet/Makefile
		libpccir/Makefile
		libunwind/Makefile
		libxmath/Makefile
		libvtable/Makefile
		libcrt/Makefile
		libbgi/Makefile
		libvm/Makefile
		libfs/Makefile"

# C language is always included (required for bootstrap)
CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		cc/Makefile
		cc/cc/Makefile
		cc/cpp/Makefile
		cc/ccom/Makefile
		cc/driver/Makefile"

# Add C++ if enabled
if test "$enable_cxx" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		cc/cxxcom/Makefile"
fi

# Add Fortran 90 if enabled
if test "$enable_f77" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		f90/Makefile
		f90/f90/Makefile
		f90/fcom/Makefile"
fi

# Add Pascal if enabled
if test "$enable_pascal" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		pascal/Makefile
		pascal/pascal/Makefile
		pascal/pcom/Makefile"
fi

# Add BASIC if enabled
if test "$enable_basic" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		basic/Makefile
		basic/basic/Makefile
		basic/pbc/Makefile
		basic/libbasic/Makefile"
fi

# Add BLISS if enabled
if test "$enable_bliss" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		bliss/Makefile
		bliss/bliss/Makefile
		bliss/bcom/Makefile
		libbliss/Makefile"
fi

# Add PXC (Xbase++) if enabled
if test "$enable_pxc" = "yes"; then
	CONFIG_MAKEFILES="$CONFIG_MAKEFILES
		pxc/Makefile
		pxc/pxc/Makefile
		pxc/pxccom/Makefile
		pxc/libxbrt/Makefile"
fi

AC_CONFIG_FILES([$CONFIG_MAKEFILES])
AC_OUTPUT

eval "exec_prefix=$exec_prefix"
eval "bindir=$bindir"
eval "libexecdir=$libexecdir"

# Build language list for display
ENABLED_LANGUAGES=""
test "$enable_c" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES c"
test "$enable_cxx" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES c++"
test "$enable_pascal" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES pascal"
test "$enable_f77" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES f90"
test "$enable_basic" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES basic"
test "$enable_bliss" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES bliss"
test "$enable_pxc" = "yes" && ENABLED_LANGUAGES="$ENABLED_LANGUAGES pxc"
ENABLED_LANGUAGES=`echo $ENABLED_LANGUAGES | sed 's/^ *//'`

echo
echo "Target CPU is .................... ${targmach}"
echo "Target ABI is .................... ${abi}"
echo "Target OS is ..................... ${targos}"
echo "Enabled languages ................ ${ENABLED_LANGUAGES}"
echo "Enabled architectures ............ ${enable_archs}"
echo "Enabled OS targets ............... ${enable_targets}"
echo "Bootstrap build .................. ${bootstrap} (${bootstrap_stages} stage(s))"
echo "Compiler is called ............... ${BINPREFIX}pcc${EXEEXT}"
echo "Installing compiler into ......... ${bindir}"
echo "Installing pre-processor into .... ${libexecdir}"
echo "Using assembler .................. ${assembler-<default>}"
echo "Using linker ..................... ${linker-<default>}"
echo "Using Multi-Arch path ............ ${multiarch}"
echo "Using include path ............... ${altincdir-<default>}"
echo "Using library path ............... ${altlibdir-<default>}"
echo "Use libvmf ....................... $libvmf"
echo "Has TLS support .................. $tls"
echo "Has native floating point ........ $nativefp"
echo "Has GCC compatibility ............ $gcccompat"
echo "Has PCC debugging ................ $pccdebug"
echo "Type of wchar_t is ............... ${wchar_type} (${wchar_size} chars)"
echo "C11 support ...................... ${have_c11:-no}"
echo "C23 support ...................... ${have_c23:-no}"
echo "Build portable Unicode library ... ${BUILD_LIBUNICODE}"
echo "Build portable Threading library . ${BUILD_LIBTHREAD}"
echo "Build portable Bit utils library . ${BUILD_LIBSTDBIT}"
echo "Build portable Checked int library ${BUILD_LIBCKDINT}"
echo "Build portable Decimal FP library  ${BUILD_LIBDECIMAL}"
echo "Build MetaWare extensions library  ${BUILD_LIBMETAWARE}"
echo
echo "Configure finished.  Do 'make && make install' to compile and install.
"
