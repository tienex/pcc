/* Exception Handling Library - OS dispatcher */
#include "except.h"

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/except_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/except_dos.c"
#else
  #include "unix/except_unix.c"
#endif
