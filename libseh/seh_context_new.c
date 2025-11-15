/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * SEH Context Handling - Platform Dispatcher
 */

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/seh_context_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/seh_context_dos.c"
#else
  /* Unix/POSIX (Linux, BSD, macOS, etc.) */
  #include "unix/seh_context_unix.c"
#endif
