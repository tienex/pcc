/*
 * Test program for C23 bit manipulation functions (stdbit.h)
 */

#include <stdio.h>
#include <stdbit.h>

int main(void) {
    unsigned int value;

    printf("Testing C23 Bit Manipulation Functions (stdbit.h)\n");
    printf("==================================================\n\n");

    /* Test leading zeros */
    value = 0x00FF0000;
    printf("Value: 0x%08X\n", value);
    printf("  Leading zeros:  %u\n", stdc_leading_zeros_ui(value));
    printf("  Leading ones:   %u\n", stdc_leading_ones_ui(value));
    printf("  Trailing zeros: %u\n", stdc_trailing_zeros_ui(value));
    printf("  Trailing ones:  %u\n", stdc_trailing_ones_ui(value));
    printf("\n");

    /* Test count operations */
    value = 0x0F0F0F0F;
    printf("Value: 0x%08X\n", value);
    printf("  Count ones:     %u\n", stdc_count_ones_ui(value));
    printf("  Count zeros:    %u\n", stdc_count_zeros_ui(value));
    printf("\n");

    /* Test power of 2 detection */
    printf("Power of 2 tests:\n");
    value = 64;
    printf("  %u is power of 2: %s\n", value,
           stdc_has_single_bit_ui(value) ? "YES" : "NO");
    value = 63;
    printf("  %u is power of 2: %s\n", value,
           stdc_has_single_bit_ui(value) ? "YES" : "NO");
    printf("\n");

    /* Test bit width */
    printf("Bit width tests:\n");
    value = 255;
    printf("  Bit width of %u: %u bits\n", value, stdc_bit_width_ui(value));
    value = 256;
    printf("  Bit width of %u: %u bits\n", value, stdc_bit_width_ui(value));
    value = 1000;
    printf("  Bit width of %u: %u bits\n", value, stdc_bit_width_ui(value));
    printf("\n");

    /* Test bit floor and ceil */
    printf("Bit floor/ceil tests:\n");
    value = 100;
    printf("  Bit floor of %u: %u\n", value, stdc_bit_floor_ui(value));
    printf("  Bit ceil of %u:  %u\n", value, stdc_bit_ceil_ui(value));
    value = 128;
    printf("  Bit floor of %u: %u\n", value, stdc_bit_floor_ui(value));
    printf("  Bit ceil of %u:  %u\n", value, stdc_bit_ceil_ui(value));
    printf("\n");

    /* Test first leading/trailing operations */
    value = 0xFF00FF00;
    printf("Value: 0x%08X\n", value);
    printf("  First leading one:   bit %u\n", stdc_first_leading_one_ui(value));
    printf("  First leading zero:  bit %u\n", stdc_first_leading_zero_ui(value));
    printf("  First trailing one:  bit %u\n", stdc_first_trailing_one_ui(value));
    printf("  First trailing zero: bit %u\n", stdc_first_trailing_zero_ui(value));
    printf("\n");

    /* Type-generic tests with different sizes */
    printf("Type-generic tests:\n");
    unsigned char uc = 0xF0;
    unsigned short us = 0xFF00;
    unsigned long ul = 0xFFFFFF00UL;

    printf("  unsigned char 0x%02X:  %u leading zeros\n",
           uc, stdc_leading_zeros(uc));
    printf("  unsigned short 0x%04X: %u leading zeros\n",
           us, stdc_leading_zeros(us));
    printf("  unsigned long 0x%08lX: %u leading zeros\n",
           ul, stdc_leading_zeros(ul));
    printf("\n");

    printf("C23 Bit Manipulation Tests PASSED!\n");
    return 0;
}
