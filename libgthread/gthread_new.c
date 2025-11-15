/*
 * Copyright (c) 2025 PCC Project
 * Threading Library - Main Dispatcher
 */

#include "gthread.h"

/* Detect OS and include appropriate implementation */
#if defined(_WIN32) || defined(_WIN64)
  #if defined(_WIN64) || defined(__MINGW32__)
    #define GTHREAD_OS_WIN32
  #else
    #define GTHREAD_OS_WIN16
  #endif
#elif defined(__DOS__) || defined(MSDOS)
  #ifdef __DJGPP__
    #define GTHREAD_OS_DOS32
  #else
    #define GTHREAD_OS_DOS
  #endif
#elif defined(__OS2__)
  #ifdef __32BIT__
    #define GTHREAD_OS_OS2_32
  #else
    #define GTHREAD_OS_OS2_16
  #endif
#elif defined(__VMS)
  #define GTHREAD_OS_OPENVMS
#elif defined(__BEOS__) || defined(__HAIKU__)
  #define GTHREAD_OS_BEOS
#else
  #define GTHREAD_OS_UNIX
#endif

#ifdef GTHREAD_OS_WIN32
  #include "win32/gthread_win32.c"
#elif GTHREAD_OS_WIN16
  #include "win16/gthread_win16.c"
#elif GTHREAD_OS_DOS32
  #include "dos32/gthread_dos32.c"
#elif GTHREAD_OS_DOS
  #include "dos/gthread_dos.c"
#elif GTHREAD_OS_OS2_32
  #include "os2_32/gthread_os2_32.c"
#elif GTHREAD_OS_OS2_16
  #include "os2_16/gthread_os2_16.c"
#elif GTHREAD_OS_OPENVMS
  #include "openvms/gthread_openvms.c"
#elif GTHREAD_OS_BEOS
  #include "beos/gthread_beos.c"
#else
  #include "unix/gthread_unix.c"
#endif
