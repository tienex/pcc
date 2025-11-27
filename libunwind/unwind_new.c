/* Stack Unwinding - Arch dispatcher */
#include "unwind.h"

#if defined(__x86_64__) || defined(_M_X64)
  #include "x86_64/unwind_x86_64.c"
#elif defined(__i386__) || defined(_M_IX86)
  #include "i386/unwind_i386.c"
#elif defined(__arm__) || defined(__aarch64__)
  #include "arm/unwind_arm.c"
#elif defined(__mips__)
  #include "mips/unwind_mips.c"
#elif defined(__sparc__)
  #include "sparc/unwind_sparc.c"
#else
  #include "generic/unwind_generic.c"
#endif
