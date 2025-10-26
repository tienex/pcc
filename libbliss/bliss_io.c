/*
 * Copyright (c) 2025 PCC BLISS Runtime Library
 *
 * BLISS-callable I/O Wrappers
 * These functions have BLISS-compatible calling conventions
 */

#include "blissrt.h"
#include <stdint.h>

/*
 * BLISS uses fullword (intptr_t) for all values
 * These wrappers convert between BLISS words and C types
 */

/*
 * Print a character - BLISS callable
 */
void BLISS$PUTCHAR(intptr_t ch)
{
	bliss_putchar((int)ch);
}

/*
 * Print a C string - BLISS callable
 */
void BLISS$PUTS(intptr_t str_ptr)
{
	bliss_puts((const char *)str_ptr);
}

/*
 * Print a newline - BLISS callable
 */
void BLISS$PUTCRLF(void)
{
	bliss_putcrlf();
}

/*
 * Print a decimal number - BLISS callable
 */
void BLISS$PUT_DECIMAL(intptr_t value)
{
	bliss_put_decimal((bliss_word_t)value);
}

/*
 * Print a hexadecimal number - BLISS callable
 */
void BLISS$PUT_HEX(intptr_t value)
{
	bliss_put_hex((bliss_word_t)value);
}

/*
 * Print an octal number - BLISS callable
 */
void BLISS$PUT_OCTAL(intptr_t value)
{
	bliss_put_octal((bliss_word_t)value);
}

/*
 * Get a character - BLISS callable
 */
intptr_t BLISS$GETCHAR(void)
{
	return (intptr_t)bliss_getchar();
}

/*
 * Simplified names (without BLISS$ prefix) for convenience
 */
void putchar_bliss(intptr_t ch) { BLISS$PUTCHAR(ch); }
void puts_bliss(intptr_t str) { BLISS$PUTS(str); }
void putcrlf_bliss(void) { BLISS$PUTCRLF(); }
void put_decimal_bliss(intptr_t val) { BLISS$PUT_DECIMAL(val); }
void put_hex_bliss(intptr_t val) { BLISS$PUT_HEX(val); }
void put_octal_bliss(intptr_t val) { BLISS$PUT_OCTAL(val); }
intptr_t getchar_bliss(void) { return BLISS$GETCHAR(); }
