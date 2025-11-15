/*
 * Copyright (c) 2025 PCC Project
 * CRT (Console/Terminal) Library - Main Dispatcher
 */

#include "crt.h"

#if defined(_WIN32) || defined(_WIN64)
  #ifndef WIN16
    #define CRT_OS_WIN32
  #else
    #define CRT_OS_WIN16
  #endif
#elif defined(__DOS__) || defined(MSDOS)
  #ifdef __DJGPP__
    #define CRT_OS_DOS32
  #else
    #define CRT_OS_DOS
  #endif
#elif defined(__OS2__)
  #ifdef __32BIT__
    #define CRT_OS_OS2_32
  #else
    #define CRT_OS_OS2_16
  #endif
#elif defined(__VMS)
  #define CRT_OS_OPENVMS
#elif defined(__BEOS__) || defined(__HAIKU__)
  #define CRT_OS_BEOS
#else
  #define CRT_OS_UNIX
#endif

#ifdef CRT_OS_WIN32
  #include "win32/crt_win32.c"
#elif CRT_OS_WIN16
  #include "win16/crt_win16.c"
#elif CRT_OS_DOS32
  #include "dos32/crt_dos32.c"
#elif CRT_OS_DOS
  #include "dos/crt_dos.c"
#elif CRT_OS_OS2_32
  #include "os2_32/crt_os2_32.c"
#elif CRT_OS_OS2_16
  #include "os2_16/crt_os2_16.c"
#elif CRT_OS_OPENVMS
  #include "openvms/crt_openvms.c"
#elif CRT_OS_BEOS
  #include "beos/crt_beos.c"
#else
  #include "unix/crt_unix.c"
#endif
