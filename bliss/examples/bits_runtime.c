/*
 * Bit Field Operations using BLISS Runtime Library
 * Demonstrates bit manipulation functions
 */

#include <blissrt.h>

/* BLISS main routine */
long bliss_main(void)
{
	bliss_word_t value, field, new_value;
	bliss_uword_t test;

	bliss_puts("BLISS Bit Field Operations");
	bliss_putcrlf();
	bliss_putcrlf();

	/* Field extraction */
	value = 0x12345678;
	bliss_puts("Original value: 0x");
	bliss_put_hex(value);
	bliss_putcrlf();

	field = bliss_field_extract(value, 8, 8);
	bliss_puts("Extract bits [8:15]: 0x");
	bliss_put_hex(field);
	bliss_putcrlf();

	/* Field insertion */
	new_value = bliss_field_insert(value, 0xFF, 8, 8);
	bliss_puts("Insert 0xFF at [8:15]: 0x");
	bliss_put_hex(new_value);
	bliss_putcrlf();

	/* Bit counting */
	test = 0x00FF0000;
	bliss_putcrlf();
	bliss_puts("Bit counting for 0x");
	bliss_put_hex(test);
	bliss_putcrlf();

	bliss_puts("  Leading zeros: ");
	bliss_put_decimal(bliss_count_leading_zeros(test));
	bliss_putcrlf();

	bliss_puts("  Trailing zeros: ");
	bliss_put_decimal(bliss_count_trailing_zeros(test));
	bliss_putcrlf();

	bliss_puts("  Population count: ");
	bliss_put_decimal(bliss_popcount(test));
	bliss_putcrlf();

	/* More examples */
	test = 0xAAAAAAAA;  /* Alternating bits */
	bliss_putcrlf();
	bliss_puts("Alternating bits (0x");
	bliss_put_hex(test);
	bliss_puts(") has ");
	bliss_put_decimal(bliss_popcount(test));
	bliss_puts(" set bits");
	bliss_putcrlf();

	return 0;
}
