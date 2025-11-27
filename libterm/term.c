/* Terminal Emulation Library - Main dispatcher */

/* Include core implementation (always used) */
#include "core/term_core.c"

/* Include platform-specific implementation */
#if defined(_WIN32) || defined(_WIN64)
  #include "win32/term_win32.c"
#elif defined(__APPLE__) && defined(__MACH__)
  /* MacOS Classic would go here */
  /* For now, use curses */
#elif defined(__unix__) || defined(__linux__)
  /* X11/Motif or curses would go here */
#else
  /* Fallback: no GUI, core only */
#endif

/* Default platform init/shutdown if not provided */
#ifndef TERM_PLATFORM_INIT_DEFINED
int term_platform_init(void) {
	return 1;
}

void term_platform_shutdown(void) {
}
#endif
