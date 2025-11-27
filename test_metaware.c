/*
 * Comprehensive test for MetaWare High C Extensions (libmetaware)
 * Tests: Nested functions, Generators, Function values, Non-local goto
 */

#include <stdio.h>
#include <stdlib.h>
#include "libmetaware/metaware.h"

#define TEST(name) printf("\n=== Testing %s ===\n", name)
#define ASSERT(cond, msg) if (!(cond)) { \
    printf("FAILED: %s\n", msg); \
    return 1; \
}

/* ================================================================
 * TEST 1: Nested Functions with Trampolines
 * ================================================================ */

/* Simulated nested function */
void nested_add_impl(void *chain, void *args, void *result) {
    int *outer_var = (int *)chain;
    int arg = *(int *)args;
    *(int *)result = *outer_var + arg;
}

int test_nested_functions(void) {
    TEST("Nested Functions");

    int outer_value = 100;

    /* Create trampoline for nested function */
    mw_trampoline_t *nested_add = mw_create_trampoline(
        (void (*)(void))nested_add_impl, &outer_value);

    ASSERT(nested_add != NULL, "Failed to create trampoline");

    /* Call nested function */
    int arg = 42;
    int result = 0;
    mw_call_trampoline(nested_add, &arg, &result);

    ASSERT(result == 142, "Nested function should return 142");
    printf("  Nested function returned: %d (expected 142)\n", result);

    /* Test with different outer value */
    outer_value = 200;
    mw_call_trampoline(nested_add, &arg, &result);

    ASSERT(result == 242, "Nested function should capture updated value");
    printf("  After update: %d (expected 242)\n", result);

    mw_free_trampoline(nested_add);

    printf("  Nested functions: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 2: Generator Coroutines
 * ================================================================ */

/* Simple range generator using macro */
MW_GENERATOR(test_range, int, int) {
    int count = *(int *)__args;

    for (int i = 0; i < count; i++) {
        MW_YIELD(__gen, i);
    }
}

int test_generators(void) {
    TEST("Generator Coroutines");

    mw_generator_t *gen = test_range(5);
    ASSERT(gen != NULL, "Failed to create generator");

    printf("  Generated values: ");
    int expected_sum = 0;  /* 0+1+2+3+4 = 10 */
    int actual_sum = 0;

    int value;
    MW_FOR_EACH(value, gen) {
        printf("%d ", value);
        actual_sum += value;
    }
    printf("\n");

    expected_sum = 0 + 1 + 2 + 3 + 4;
    ASSERT(actual_sum == expected_sum, "Generator sum should be 10");
    printf("  Sum: %d (expected %d)\n", actual_sum, expected_sum);

    ASSERT(mw_generator_done(gen), "Generator should be done");

    mw_free_generator(gen);

    printf("  Generator coroutines: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 3: Function Values (Closures)
 * ================================================================ */

typedef struct {
    int multiplier;
} multiplier_ctx_t;

void multiply_impl(void *ctx, void *args, void *result) {
    multiplier_ctx_t *context = (multiplier_ctx_t *)ctx;
    int x = *(int *)args;
    *(int *)result = x * context->multiplier;
}

mw_function_t *make_multiplier(int factor) {
    multiplier_ctx_t ctx = { factor };
    return mw_create_function(multiply_impl, &ctx, sizeof(ctx));
}

int test_function_values(void) {
    TEST("Function Values (Closures)");

    /* Create multiplier functions */
    mw_function_t *times_5 = make_multiplier(5);
    mw_function_t *times_10 = make_multiplier(10);

    ASSERT(times_5 != NULL, "Failed to create times_5 function");
    ASSERT(times_10 != NULL, "Failed to create times_10 function");

    /* Test times_5 */
    int arg = 7;
    int result;

    mw_call_function(times_5, &arg, &result);
    ASSERT(result == 35, "times_5(7) should be 35");
    printf("  times_5(7) = %d\n", result);

    /* Test times_10 */
    mw_call_function(times_10, &arg, &result);
    ASSERT(result == 70, "times_10(7) should be 70");
    printf("  times_10(7) = %d\n", result);

    /* Test with different argument */
    arg = 3;
    mw_call_function(times_5, &arg, &result);
    ASSERT(result == 15, "times_5(3) should be 15");
    printf("  times_5(3) = %d\n", result);

    mw_free_function(times_5);
    mw_free_function(times_10);

    printf("  Function values: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 4: Non-Local Goto
 * ================================================================ */

static int cleanup_called = 0;

void test_cleanup(void *data) {
    cleanup_called = 1;
    printf("  Cleanup handler called\n");
}

int test_nonlocal_goto(void) {
    TEST("Non-Local Goto");

    cleanup_called = 0;

    MW_LABEL(error_label);

    /* Set cleanup handler */
    mw_set_label_cleanup(&error_label, test_cleanup, NULL);

    /* Simulate error condition */
    int error = 1;

    if (error) {
        printf("  Triggering non-local goto...\n");
        mw_goto_label(&error_label, 42);
    }

    printf("  This should not execute\n");
    ASSERT(0, "Code after goto should not execute");

__mw_label_error_label:
    printf("  Arrived at error label\n");

    ASSERT(cleanup_called == 1, "Cleanup should have been called");
    printf("  Cleanup verified\n");

    printf("  Non-local goto: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 5: Nesting Level Management
 * ================================================================ */

int test_nesting_levels(void) {
    TEST("Nesting Level Management");

    int level = mw_get_nesting_level();
    ASSERT(level == 0, "Initial nesting level should be 0");
    printf("  Initial level: %d\n", level);

    mw_push_nesting_level();
    level = mw_get_nesting_level();
    ASSERT(level == 1, "After push, level should be 1");
    printf("  After push: %d\n", level);

    mw_push_nesting_level();
    mw_push_nesting_level();
    level = mw_get_nesting_level();
    ASSERT(level == 3, "After 3 pushes, level should be 3");
    printf("  After 3 pushes: %d\n", level);

    mw_pop_nesting_level();
    level = mw_get_nesting_level();
    ASSERT(level == 2, "After pop, level should be 2");
    printf("  After pop: %d\n", level);

    /* Reset to 0 */
    mw_pop_nesting_level();
    mw_pop_nesting_level();
    level = mw_get_nesting_level();
    ASSERT(level == 0, "After all pops, level should be 0");

    printf("  Nesting levels: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 6: Complex Generator Example (Fibonacci)
 * ================================================================ */

MW_GENERATOR(fibonacci, int, int) {
    int count = *(int *)__args;
    int a = 0, b = 1;

    for (int i = 0; i < count; i++) {
        MW_YIELD(__gen, a);

        int temp = a;
        a = b;
        b = temp + b;
    }
}

int test_fibonacci_generator(void) {
    TEST("Fibonacci Generator");

    int expected[] = {0, 1, 1, 2, 3, 5, 8, 13, 21, 34};

    mw_generator_t *gen = fibonacci(10);
    ASSERT(gen != NULL, "Failed to create fibonacci generator");

    printf("  Fibonacci sequence: ");

    int i = 0;
    int num;
    MW_FOR_EACH(num, gen) {
        printf("%d ", num);
        ASSERT(num == expected[i], "Fibonacci value mismatch");
        i++;
    }
    printf("\n");

    ASSERT(i == 10, "Should have generated 10 values");

    mw_free_generator(gen);

    printf("  Fibonacci generator: PASS\n");
    return 0;
}

/* ================================================================
 * TEST 7: Multiple Captures in Nested Function
 * ================================================================ */

void multi_capture_impl(void *chain, void *args, void *result) {
    /* In a real implementation, chain would point to a structure
       containing all captured variables */
    int *x = (int *)chain;
    int y = x[0];
    int z = x[1];
    int arg = *(int *)args;

    *(int *)result = y + z + arg;
}

int test_multiple_captures(void) {
    TEST("Multiple Variable Captures");

    int vars[2] = {10, 20};

    mw_trampoline_t *func = mw_create_trampoline(
        (void (*)(void))multi_capture_impl, vars);

    ASSERT(func != NULL, "Failed to create multi-capture trampoline");

    int arg = 5;
    int result;
    mw_call_trampoline(func, &arg, &result);

    ASSERT(result == 35, "Multi-capture should return 10+20+5=35");
    printf("  Captured(10,20) + arg(5) = %d\n", result);

    mw_free_trampoline(func);

    printf("  Multiple captures: PASS\n");
    return 0;
}

/* ================================================================
 * MAIN TEST RUNNER
 * ================================================================ */

int main(void) {
    int failures = 0;

    printf("==================================================\n");
    printf("  MetaWare High C Extensions Test Suite\n");
    printf("==================================================\n");

    failures += test_nested_functions();
    failures += test_generators();
    failures += test_function_values();
    failures += test_nonlocal_goto();
    failures += test_nesting_levels();
    failures += test_fibonacci_generator();
    failures += test_multiple_captures();

    printf("\n==================================================\n");
    if (failures == 0) {
        printf("  ALL TESTS PASSED!\n");
        printf("==================================================\n");
        return 0;
    } else {
        printf("  %d TEST(S) FAILED\n", failures);
        printf("==================================================\n");
        return 1;
    }
}
