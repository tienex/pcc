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
#define CPPADD  { "-D__LiteBSD__", "-D__ELF__", "-D__unix__", \
	NULL, }
#define	DYNLINKER { NULL }

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

#define	CPPMDADD { "-D__mips__", "-Dmips", "-D__mips=32", "-D__MIPSEL", \
	"-D__MIPSEL__", "-DMIPSEL", "-D_MIPSEL", NULL, }

#define DEFLIBS { "-lc", "-lgcc", NULL }

/* We only have crt0.o */
#define CRTBEGIN	0
#define CRTEND		0
#define CRTI		0
#define CRTN		0
