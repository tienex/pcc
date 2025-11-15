/*
 * C11 threads implementation
 * Platform Dispatcher
 */

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/threads_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/threads_dos.c"
#else
  /* Unix/POSIX (Linux, BSD, macOS, etc.) */
  #include "unix/threads_unix.c"
#endif
