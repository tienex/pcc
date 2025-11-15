/*
 * C11 threads implementation
 * Platform Dispatcher
 */

#if defined(__OS2__) && defined(__I16__)
  /* OS/2 16-bit - no native threading */
  #include "os2_16/threads_os2_16.c"
#elif defined(__OS2__) || defined(__EMX__)
  /* OS/2 32-bit - native threading */
  #include "os2_32/threads_os2_32.c"
#elif defined(__VMS) || defined(VMS) || defined(__vms__)
  /* OpenVMS - POSIX threads on modern VMS */
  #include "openvms/threads_openvms.c"
#elif defined(__BEOS__) || defined(__HAIKU__)
  /* BeOS / Haiku - native threading (POSIX-like) */
  #include "beos/threads_beos.c"
#elif (defined(_WIN32) || defined(_WIN64)) && !defined(_WIN32_WCE)
  /* Windows 32/64-bit */
  #include "win32/threads_win32.c"
#elif defined(__WINDOWS__) || defined(_WINDOWS) || defined(__WIN16__)
  /* Windows 16-bit - no real threading */
  #include "win16/threads_win16.c"
#elif defined(__GO32__) || defined(__DJGPP__) || (defined(__WATCOMC__) && defined(__DOS__) && defined(__386__))
  /* DOS 32-bit - no real threading */
  #include "dos32/threads_dos32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS) || defined(MSDOS)
  /* DOS 16-bit - no threading */
  #include "dos/threads_dos.c"
#else
  /* Unix/POSIX (Linux, *BSD, macOS, Solaris, AIX, etc.) */
  #include "unix/threads_unix.c"
#endif
