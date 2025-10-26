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
#define	CPPADD	{ "-D__OpenBSD__", "-D__ELF__", NULL, }
#define	DYNLINKLIB	"/usr/libexec/ld.so"
#define	CRTBEGIN_T	CRTBEGIN
#define	CRTEND_T	CRTEND
#define	CRTI 0		/* OpenBSD does not use this */
#define	CRTN 0		/* OpenBSD does not use this */

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

#define PCC_EARLY_SETUP { kflag = 1; }

#if defined(mach_amd64)
#define	CPPMDADD { "-D__amd64__", "-D__amd64", "-D__x86_64__", "-D__x86_64", \
		    "-D__LP64__", "-D_LP64", NULL, }
#elif defined(mach_i386)
#define	CPPMDADD { "-D__i386__", NULL, }
#elif defined(mach_vax)
#define CPPMDADD { "-D__vax__", NULL, } 
#elif defined(mach_powerpc)
#define CPPMDADD { "-D__powerpc__", NULL }
#elif defined(mach_sparc64)
#define CPPMDADD { "-D__sparc64__", "-D__LP64__", "-D_LP64", NULL }
#elif defined(mach_m68k)
#define CPPMDADD { "-D__mc68000__", "-D__mc68020__", "-D__m68k__", NULL }
#define STARTLABEL "_start"
#elif defined(mach_mips64)
#ifdef TARGET_BIG_ENDIAN
#define CPPMDADD { "-D__MIPSEB__", "-D__mips__", "-D__mips64__", "-D__LP64__", \
		    "-D_LP64", NULL }
#else
#define CPPMDADD { "-D__MIPSEL__", "-D__mips__", "-D__mipsel__", \
		    "-D__mips64__", "-D__mips64el__", "-D__LP64__", "-D_LP64", \
		    NULL }
#endif
#else
#error defines for arch missing
#endif
