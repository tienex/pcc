/* Garbage Collector Library - OS dispatcher */
#include "gc.h"

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/gc_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/gc_dos.c"
#else
  #include "unix/gc_unix.c"
#endif
