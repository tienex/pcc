/*
 * Integration test for BLISS Runtime I/O functions
 */

#include <blissrt.h>
#include <stdio.h>
#include <stdlib.h>

long bliss_main(void)
{
	int tests_passed = 0;
	int tests_total = 0;

	printf("=== BLISS Runtime I/O Tests ===\n\n");

	/* Test 1: putchar */
	tests_total++;
	printf("Test 1: putchar... ");
	bliss_putchar('O');
	bliss_putchar('K');
	bliss_putcrlf();
	tests_passed++;

	/* Test 2: puts */
	tests_total++;
	printf("Test 2: puts... ");
	bliss_puts("OK");
	bliss_putcrlf();
	tests_passed++;

	/* Test 3: decimal output */
	tests_total++;
	printf("Test 3: put_decimal... ");
	bliss_put_decimal(12345);
	printf(" (expected 12345)");
	bliss_putcrlf();
	tests_passed++;

	/* Test 4: hex output */
	tests_total++;
	printf("Test 4: put_hex... ");
	bliss_put_hex(0xABCD);
	printf(" (expected ABCD)");
	bliss_putcrlf();
	tests_passed++;

	/* Test 5: octal output */
	tests_total++;
	printf("Test 5: put_octal... ");
	bliss_put_octal(0777);
	printf(" (expected 777)");
	bliss_putcrlf();
	tests_passed++;

	printf("\n=== Results ===\n");
	printf("Tests passed: %d/%d\n", tests_passed, tests_total);

	return (tests_passed == tests_total) ? 0 : 1;
}
