/*
 * Copyright (c) 2025 PCC Project
 * Universal File System Abstraction - Main Implementation
 */

#include "fs.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Detect operating system at compile time */
#if defined(_WIN32) || defined(_WIN64)
  #if defined(_WIN64) || defined(__MINGW32__)
    #define FS_OS_WIN32
  #else
    #define FS_OS_WIN16
  #endif
#elif defined(__DOS__) || defined(MSDOS) || defined(__MSDOS__)
  #ifdef __DJGPP__
    #define FS_OS_DOS32
  #else
    #define FS_OS_DOS
  #endif
#elif defined(__OS2__)
  #ifdef __32BIT__
    #define FS_OS_OS2_32
  #else
    #define FS_OS_OS2_16
  #endif
#elif defined(__VMS)
  #define FS_OS_OPENVMS
#elif defined(__BEOS__) || defined(__HAIKU__)
  #define FS_OS_BEOS
#else
  #define FS_OS_UNIX
#endif

/* Include OS-specific implementation */
#ifdef FS_OS_WIN32
  #include "win32/fs_win32.c"
#elif FS_OS_WIN16
  #include "win16/fs_win16.c"
#elif FS_OS_DOS32
  #include "dos32/fs_dos32.c"
#elif FS_OS_DOS
  #include "dos/fs_dos.c"
#elif FS_OS_OS2_32
  #include "os2_32/fs_os2_32.c"
#elif FS_OS_OS2_16
  #include "os2_16/fs_os2_16.c"
#elif FS_OS_OPENVMS
  #include "openvms/fs_openvms.c"
#elif FS_OS_BEOS
  #include "beos/fs_beos.c"
#else
  #include "unix/fs_unix.c"
#endif
