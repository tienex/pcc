/* Networking Library - OS dispatcher */
#include "net.h"

#if defined(_WIN32) || defined(_WIN64)
  #include "win32/net_win32.c"
#else
  #include "unix/net_unix.c"
#endif
