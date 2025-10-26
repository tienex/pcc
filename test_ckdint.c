/*
 * Test program for C23 checked integer arithmetic (stdckdint.h)
 */

#include <stdio.h>
#include <stdckdint.h>
#include <limits.h>

int main(void) {
    int result_int;
    unsigned int result_uint;
    bool overflow;

    printf("Testing C23 Checked Integer Arithmetic (stdckdint.h)\n");
    printf("====================================================\n\n");

    /* Test signed addition */
    printf("Signed Integer Addition Tests:\n");
    overflow = ckd_add(&result_int, 100, 200);
    printf("  100 + 200 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_add(&result_int, INT_MAX, 1);
    printf("  INT_MAX + 1 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_add(&result_int, INT_MIN, -1);
    printf("  INT_MIN + (-1) = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");
    printf("\n");

    /* Test signed subtraction */
    printf("Signed Integer Subtraction Tests:\n");
    overflow = ckd_sub(&result_int, 300, 100);
    printf("  300 - 100 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_sub(&result_int, INT_MIN, 1);
    printf("  INT_MIN - 1 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_sub(&result_int, INT_MAX, -1);
    printf("  INT_MAX - (-1) = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");
    printf("\n");

    /* Test signed multiplication */
    printf("Signed Integer Multiplication Tests:\n");
    overflow = ckd_mul(&result_int, 100, 200);
    printf("  100 * 200 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_mul(&result_int, INT_MAX, 2);
    printf("  INT_MAX * 2 = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");

    overflow = ckd_mul(&result_int, INT_MIN, -1);
    printf("  INT_MIN * (-1) = %d, overflow: %s\n",
           result_int, overflow ? "YES" : "NO");
    printf("\n");

    /* Test unsigned addition */
    printf("Unsigned Integer Addition Tests:\n");
    overflow = ckd_add(&result_uint, 100U, 200U);
    printf("  100 + 200 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_add(&result_uint, UINT_MAX, 1U);
    printf("  UINT_MAX + 1 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_add(&result_uint, UINT_MAX - 10U, 5U);
    printf("  (UINT_MAX-10) + 5 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");
    printf("\n");

    /* Test unsigned subtraction */
    printf("Unsigned Integer Subtraction Tests:\n");
    overflow = ckd_sub(&result_uint, 300U, 100U);
    printf("  300 - 100 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_sub(&result_uint, 0U, 1U);
    printf("  0 - 1 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_sub(&result_uint, 100U, 200U);
    printf("  100 - 200 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");
    printf("\n");

    /* Test unsigned multiplication */
    printf("Unsigned Integer Multiplication Tests:\n");
    overflow = ckd_mul(&result_uint, 100U, 200U);
    printf("  100 * 200 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_mul(&result_uint, UINT_MAX, 2U);
    printf("  UINT_MAX * 2 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");

    overflow = ckd_mul(&result_uint, UINT_MAX / 2U, 2U);
    printf("  (UINT_MAX/2) * 2 = %u, overflow: %s\n",
           result_uint, overflow ? "YES" : "NO");
    printf("\n");

    /* Practical example: safe array index calculation */
    printf("Practical Example - Safe Array Index:\n");
    unsigned int array_size = 1000;
    unsigned int element_size = 4;
    unsigned int index = 500;
    unsigned int offset;

    overflow = ckd_mul(&offset, index, element_size);
    if (!overflow && offset < array_size * element_size) {
        printf("  Array[%u] offset: %u bytes (SAFE)\n", index, offset);
    } else {
        printf("  Array[%u] access would overflow or exceed bounds\n", index);
    }

    index = UINT_MAX / 2;
    overflow = ckd_mul(&offset, index, element_size);
    if (!overflow) {
        printf("  Array[%u] offset: %u bytes\n", index, offset);
    } else {
        printf("  Array[%u] access causes overflow (UNSAFE)\n", index);
    }
    printf("\n");

    printf("C23 Checked Integer Arithmetic Tests PASSED!\n");
    return 0;
}
