/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Runtime - Platform Dispatcher
 */

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/seh_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/seh_dos.c"
#else
  /* Unix/POSIX (Linux, BSD, macOS, etc.) */
  #include "unix/seh_unix.c"
#endif
