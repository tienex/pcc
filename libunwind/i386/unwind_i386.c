/* Generic unwinding stub */
#include <stdlib.h>
struct unw_context { int dummy; };
struct unw_cursor { int dummy; };

int unw_getcontext(unw_context_t *ctx) { (void)ctx; return -1; }
int unw_init_local(unw_cursor_t *cursor, unw_context_t *ctx) { (void)cursor; (void)ctx; return -1; }
int unw_step(unw_cursor_t *cursor) { (void)cursor; return 0; }
int unw_get_reg(unw_cursor_t *cursor, unw_regnum_t reg, uintptr_t *val) {
	(void)cursor; (void)reg; (void)val; return -1;
}
int unw_backtrace(void **buffer, int size) { (void)buffer; (void)size; return 0; }
