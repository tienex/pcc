/*
 * Hello World using BLISS Runtime Library
 * This demonstrates using the runtime library from C
 * (BLISS programs would use similar calls)
 */

#include <blissrt.h>

/*
 * BLISS main routine
 * This is called by the startup code
 */
long bliss_main(void)
{
	/* Print hello message */
	bliss_puts("Hello from BLISS Runtime!");
	bliss_putcrlf();

	/* Print some numbers */
	bliss_puts("Decimal: ");
	bliss_put_decimal(42);
	bliss_putcrlf();

	bliss_puts("Hexadecimal: ");
	bliss_put_hex(255);
	bliss_putcrlf();

	bliss_puts("Octal: ");
	bliss_put_octal(64);
	bliss_putcrlf();

	return 0;  /* Success */
}
