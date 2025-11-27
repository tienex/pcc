/*
 * String Operations using BLISS Runtime Library
 * Demonstrates string manipulation functions
 */

#include <blissrt.h>

/* BLISS main routine */
long bliss_main(void)
{
	bliss_string_t s1, s2, s3, sub;
	char *cstr;

	bliss_puts("BLISS String Operations");
	bliss_putcrlf();
	bliss_putcrlf();

	/* Create strings */
	s1 = bliss_string_from_cstr("Hello");
	s2 = bliss_string_from_cstr(" World");

	/* Concatenate strings */
	s3 = bliss_string_concat(s1, s2);

	/* Display result */
	bliss_puts("Concatenation: ");
	bliss_put_string(s3);
	bliss_putcrlf();

	/* Compare strings */
	bliss_puts("Compare 'Hello' vs ' World': ");
	bliss_put_decimal(bliss_string_compare(s1, s2));
	bliss_putcrlf();

	/* Extract substring */
	sub = bliss_string_substr(s3, 0, 5);
	bliss_puts("Substring [0:5]: ");
	bliss_put_string(sub);
	bliss_putcrlf();

	/* Convert to C string */
	cstr = bliss_string_to_cstr(s3);
	bliss_puts("As C string: ");
	bliss_puts(cstr);
	bliss_putcrlf();

	/* Free resources */
	bliss_free(cstr);
	bliss_string_free(s1);
	bliss_string_free(s2);
	bliss_string_free(s3);
	bliss_string_free(sub);

	return 0;
}
