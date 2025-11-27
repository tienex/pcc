/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Runtime - Platform Dispatcher
 */

#if defined(__UEFI__) || defined(UEFI) || defined(_UEFI)
  /* UEFI (Unified Extensible Firmware Interface) */
  #include "uefi/seh_uefi.c"
#elif defined(__NETWARE__) || defined(__netware__)
  /* Novell NetWare 3.x+ */
  #include "netware/seh_netware.c"
#elif defined(__amigaos__) || defined(__AMIGA__) || defined(AMIGA)
  /* AmigaOS (68k and PowerPC) */
  #include "amiga/seh_amiga.c"
#elif defined(__ATARI__) || defined(__atarist__) || defined(ATARI)
  /* Atari TOS/GEM */
  #include "atari/seh_atari.c"
#elif defined(macintosh) || (defined(__APPLE__) && !defined(__MACH__))
  /* Mac OS Classic (pre-OSX) */
  #include "macos/seh_macos.c"
#elif defined(__OS2__) && defined(__I16__)
  /* OS/2 16-bit */
  #include "os2_16/seh_os2_16.c"
#elif defined(__OS2__) || defined(__EMX__)
  /* OS/2 32-bit */
  #include "os2_32/seh_os2_32.c"
#elif defined(__VMS) || defined(VMS) || defined(__vms__)
  /* OpenVMS (VAX, Alpha, Itanium) */
  #include "openvms/seh_openvms.c"
#elif defined(__BEOS__) || defined(__HAIKU__)
  /* BeOS / Haiku */
  #include "beos/seh_beos.c"
#elif (defined(_WIN32) || defined(_WIN64)) && !defined(_WIN32_WCE)
  /* Windows 32/64-bit */
  #include "win32/seh_win32.c"
#elif defined(__WINDOWS__) || defined(_WINDOWS) || defined(__WIN16__)
  /* Windows 16-bit */
  #include "win16/seh_win16.c"
#elif defined(__GO32__) || defined(__DJGPP__) || (defined(__WATCOMC__) && defined(__DOS__) && defined(__386__))
  /* DOS 32-bit (DJGPP, Watcom DOS4GW) */
  #include "dos32/seh_dos32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS) || defined(MSDOS)
  /* DOS 16-bit */
  #include "dos/seh_dos.c"
#else
  /* Unix/POSIX - includes:
   * - Linux, *BSD, macOS (OS X), Solaris, AIX, HP-UX, IRIX
   * - UnixWare, SCO OpenServer
   * - OpenStep, NEXTSTEP, OPENSTEP
   * - Plan 9 from Bell Labs
   * - MINIX 3.x
   * - QNX, Tru64 UNIX, and other POSIX-compliant systems
   */
  #include "unix/seh_unix.c"
#endif
