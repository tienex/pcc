/*	$Id$	*/

/*
 * Copyright (c) 2004 Anders Magnusson (ragge@ludd.luth.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Various settings that controls how the C compiler works.
 */

/* common cpp predefines */
#define CPPADD	{ "-D__linux__", "-D__ELF__", NULL, }

#define CRT0		"crt1.o"
#define CRT0FILE	CRT0
#define GCRT0		"gcrt1.o"
#define CRTBEGIN	"crtbegin.o"
#define CRTEND		"crtend.o"
#define CRTI		"crti.o"
#define CRTN		"crtn.o"

#define STARTLABEL "_start"

#define STARTFILES { CRTI, CRTBEGIN, NULL }
#define	ENDFILES { CRTEND, CRTN, NULL }
#define DYNLINKER { "-dynamic-linker", DYNLINKLIB, NULL }

#if defined(mach_i386)
#define CPPMDADD	{ "-D__i386__", NULL, }
#define	DYNLINKLIB	"/lib/ld-linux.so.2"
#define MUSL_DYLIB	"/lib/ld-musl-i386.so.1"
#elif defined(mach_powerpc)
#define CPPMDADD	{ "-D__ppc__", NULL, }
#define	DYNLINKLIB	"/lib/ld-linux.so.2"
#define MUSL_DYLIB	"/lib/ld-musl-powerpc.so.1"
#elif defined(mach_amd64)
#include "../inc/amd64.h"
#define	DYNLINKLIB	"/lib64/ld-linux-x86-64.so.2"
#define MUSL_DYLIB	"/lib/ld-musl-x86_64.so.1"
#ifndef GCCLIBDIR
#define GCCLIBDIR	"/usr/lib/gcc/x86_64-linux-gnu/4.4/"
#endif
#ifndef MULTIARCH_PATH
#define	DEFLIBDIRS	{ "/usr/lib64/", GCCLIBDIR, 0 }
#else
#define	DEFLIBDIRS	{ "/usr/lib64/", "/usr/lib/" MULTIARCH_PATH "/", GCCLIBDIR, 0 }
#endif
#elif defined(mach_mips)
#define CPPMDADD	{ "-D__mips__", NULL, }
#define	DYNLINKLIB	"/lib/ld.so.1"
#define MUSL_ROOT	"/lib/ld-musl-mips"
#define MUSL_EL		"el"
#define MUSL_SF		"-sf"
#else
#error defines for arch missing
#endif

/*
 * When USE_MUSL is defined, we either provide MUSL_DYLIB, or
 * code to construct the dynamic linker name at runtime
 */
#ifdef USE_MUSL
#ifdef MUSL_DYLIB
#define DYNLINKLIB MUSL_DYLIB
#else
#ifndef MUSL_EB
#define MUSL_EB	NULL
#endif
#ifndef MUSL_EL
#define MUSL_EL	NULL
#endif
#ifndef MUSL_SF
#define MUSL_SF	NULL
#endif
#ifndef MUSL_HF
#define MUSL_HF	NULL
#endif
#ifndef MUSL_EXT
#define MUSL_EXT ".so.1"
#endif

#define PCC_SETUP_LD_ARGS	{				\
		char *t = MUSL_ROOT;				\
		t = cat(t, bigendian ? MUSL_EB : MUSL_EL);	\
		t = cat(t, softfloat ? MUSL_SF : MUSL_HF);	\
		dynlinklib = cat(t, MUSL_EXT);			\
	}
#endif
#endif

/* Fortran runtime library support (F66-F2018) */
#if defined(LANG_F77) || defined(LANG_F90)
/*
 * Modular Fortran runtime library definitions.
 * The driver will intelligently select libraries based on enabled features.
 * F66/F77/F90/F95: Basic runtime only
 * F2003+: Adds OOP support and quad-precision
 * F2008+: Adds coarray support and threading
 * F2018:  Full modern Fortran with teams/events
 */

/* Core Fortran runtime - always included */
#define F90_CORE_LIBS { "-lgfortran", NULL }

/* OOP support library (F2003+) - included when enable_oop=YES */
#define F90_OOP_LIBS { "-lfortran_oop", NULL }

/* Coarray parallel programming (F2008+) - included when enable_coarrays=YES */
#define F90_COARRAY_LIBS { "-lcaf_single", "-lpthread", NULL }

/* Quad-precision math (F2003+) - included for f_std >= 2003 */
#define F90_QUAD_LIBS { "-lquadmath", NULL }

/* Standard system libraries - always included */
#define F90_SYSTEM_LIBS { "-lm", "-lc", NULL }

/* Complete library list for legacy compatibility (links everything) */
#define F90LIBLIST { "-L/usr/local/lib", \
	"-lgfortran",      /* GNU Fortran runtime (or -lfortran) */ \
	"-lfortran_oop",   /* OOP support (F2003+) */ \
	"-lcaf_single",    /* Coarray support (single-image, F2008+) */ \
	"-lquadmath",      /* Quad-precision math */ \
	"-lm",             /* Math library */ \
	"-lpthread",       /* POSIX threads for parallel features */ \
	"-lc",             /* C library */ \
	NULL };

/* Legacy F77 library list for compatibility */
#define F77LIBLIST F90LIBLIST
#endif
