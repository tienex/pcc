/*
 * Test file for Structured Exception Handling (SEH) support in PCC
 * This file demonstrates the SEH syntax added to the compiler.
 */

#include <stdio.h>

/* SEH filter expression return values */
#define EXCEPTION_EXECUTE_HANDLER      1
#define EXCEPTION_CONTINUE_SEARCH      0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)

/* Test 1: Basic try-except */
int test_try_except(void)
{
    int result = 0;

    __try {
        printf("Inside try block\n");
        result = 42;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        printf("Exception caught\n");
        result = -1;
    }

    return result;
}

/* Test 2: Try-finally */
void test_try_finally(void)
{
    __try {
        printf("Try block executing\n");
    }
    __finally {
        printf("Finally block always executes\n");
    }
}

/* Test 3: __leave statement */
int test_leave(int should_leave)
{
    int result = 0;

    __try {
        printf("Before potential leave\n");

        if (should_leave) {
            result = 1;
            __leave;
        }

        printf("After leave check\n");
        result = 2;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        printf("Exception handler\n");
        result = -1;
    }

    return result;
}

/* Test 4: Nested try blocks */
int test_nested_try(void)
{
    int result = 0;

    __try {
        printf("Outer try block\n");

        __try {
            printf("Inner try block\n");
            result = 10;
        }
        __except (EXCEPTION_EXECUTE_HANDLER) {
            printf("Inner exception handler\n");
            result = 20;
        }

        result += 5;
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        printf("Outer exception handler\n");
        result = -1;
    }

    return result;
}

/* Test 5: Try-finally with return */
int test_finally_with_return(int value)
{
    __try {
        if (value > 0) {
            return value;
        }
        return -1;
    }
    __finally {
        printf("Finally executes even with return\n");
    }
}

/* Test 6: Complex filter expression */
int complex_filter(int code)
{
    if (code == 1) {
        return EXCEPTION_EXECUTE_HANDLER;
    } else if (code == 2) {
        return EXCEPTION_CONTINUE_SEARCH;
    } else {
        return EXCEPTION_CONTINUE_EXECUTION;
    }
}

int test_complex_filter(void)
{
    int filter_code = 1;
    int result = 0;

    __try {
        printf("Try block with complex filter\n");
        result = 100;
    }
    __except (complex_filter(filter_code)) {
        printf("Exception handled by complex filter\n");
        result = 200;
    }

    return result;
}

/* Main function to run all tests */
int main(void)
{
    printf("=== SEH Test Suite ===\n\n");

    printf("Test 1: Basic try-except\n");
    printf("Result: %d\n\n", test_try_except());

    printf("Test 2: Try-finally\n");
    test_try_finally();
    printf("\n");

    printf("Test 3: __leave statement (with leave)\n");
    printf("Result: %d\n\n", test_leave(1));

    printf("Test 4: __leave statement (without leave)\n");
    printf("Result: %d\n\n", test_leave(0));

    printf("Test 5: Nested try blocks\n");
    printf("Result: %d\n\n", test_nested_try());

    printf("Test 6: Finally with return\n");
    printf("Result: %d\n\n", test_finally_with_return(42));

    printf("Test 7: Complex filter expression\n");
    printf("Result: %d\n\n", test_complex_filter());

    printf("=== All tests completed ===\n");
    return 0;
}
