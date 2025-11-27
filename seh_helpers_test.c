/*
 * Test for SEH Helper Macros
 *
 * This demonstrates using SEH manually with helper macros
 * until full compiler code generation is complete.
 */

#include <stdio.h>
#include <stdlib.h>
#include "libseh/seh_helpers.h"

void test_manual_try_except(void) {
    printf("Test 1: Manual __try/__except using macros\n");

    SEH_TRY {
        printf("  Inside try block\n");
        printf("  Raising access violation...\n");
        RaiseException(EXCEPTION_ACCESS_VIOLATION, 0);
        printf("  This won't execute\n");
    } SEH_EXCEPT(EXCEPTION_FILTER_ALL) {
        printf("  Caught exception!\n");
        printf("  Exception code: 0x%08lX\n", GetExceptionCode());
    } SEH_END;

    printf("  Test 1 completed\n\n");
}

void test_manual_try_finally(void) {
    printf("Test 2: Manual __try/__finally using macros\n");

    SEH_TRY_FINALLY {
        printf("  Inside try block\n");
        printf("  Normal execution\n");
    } SEH_FINALLY {
        printf("  Finally block executing\n");
        printf("  Abnormal termination: %d\n", AbnormalTermination());
    } SEH_END_FINALLY;

    printf("  Test 2 completed\n\n");
}

void test_specific_exception_filter(void) {
    printf("Test 3: Specific exception filter\n");

    SEH_TRY {
        printf("  Inside try block\n");
        RaiseException(EXCEPTION_INT_DIVIDE_BY_ZERO, 0);
    } SEH_EXCEPT(EXCEPTION_FILTER_DIVIDE_BY_ZERO) {
        printf("  Caught divide by zero!\n");
        printf("  Exception code: 0x%08lX\n", GetExceptionCode());
    } SEH_END;

    printf("  Test 3 completed\n\n");
}

void test_nested_seh(void) {
    printf("Test 4: Nested SEH blocks\n");

    SEH_TRY {
        printf("  Outer try block\n");

        SEH_TRY {
            printf("    Inner try block\n");
            RaiseException(EXCEPTION_ACCESS_VIOLATION, 0);
        } SEH_EXCEPT(EXCEPTION_FILTER_ACCESS_VIOLATION) {
            printf("    Inner except handler\n");
        } SEH_END;

        printf("  Back in outer try\n");
    } SEH_EXCEPT(EXCEPTION_FILTER_ALL) {
        printf("  Outer except handler (shouldn't reach)\n");
    } SEH_END;

    printf("  Test 4 completed\n\n");
}

int main(void) {
    printf("=== SEH Helper Macros Test Suite ===\n\n");

    test_manual_try_except();
    test_manual_try_finally();
    test_specific_exception_filter();
    test_nested_seh();

    printf("=== All tests completed ===\n");
    return 0;
}
