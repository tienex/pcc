/* BGI Graphics Library - OS dispatcher */
#include "bgi.h"

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/bgi_win32.c"
#elif defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)
  #include "dos/bgi_dos.c"
#else
  #include "unix/bgi_unix.c"
#endif
