/*
 * Test Suite for libblocks (Apple Blocks Runtime)
 *
 * Tests the core functionality of the Blocks runtime library.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libblocks/Block.h"

#define TEST(name) printf("\n=== Testing %s ===\n", name)
#define ASSERT(cond, msg) if (!(cond)) { \
    printf("FAILED: %s\n", msg); \
    return 1; \
}
#define PASS(msg) printf("  %s: PASS\n", msg)

/* ================================================================
 * Helper Functions and Structures
 * ================================================================ */

/* Simple block structure for testing */
typedef struct {
    struct Block_layout layout;
    int captured_value;
} TestBlock;

/* Global block descriptor */
static struct Block_descriptor_1 test_descriptor = {
    .reserved = 0,
    .size = sizeof(TestBlock)
};

/* Test function that will be invoked by blocks */
static int test_function_impl(void *block_ptr, int arg) {
    TestBlock *block = (TestBlock *)block_ptr;
    return arg + block->captured_value;
}

/* ================================================================
 * TEST 1: Block Copy and Release
 * ================================================================ */

int test_copy_release(void) {
    TEST("Block Copy and Release");

    /* Create a stack block */
    TestBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))test_function_impl,
            .descriptor = &test_descriptor
        },
        .captured_value = 10
    };

    /* Verify it's not on heap */
    ASSERT(!_Block_is_heap(&stack_block), "Stack block should not be on heap");
    ASSERT(!_Block_is_global(&stack_block), "Stack block should not be global");

    /* Copy to heap */
    TestBlock *heap_block = (TestBlock *)_Block_copy(&stack_block);
    ASSERT(heap_block != NULL, "Block_copy should return non-NULL");
    ASSERT(_Block_is_heap(heap_block), "Copied block should be on heap");
    ASSERT(heap_block->captured_value == 10, "Captured value should be preserved");

    /* Test reference counting */
    int refcount = _Block_refcount(heap_block);
    ASSERT(refcount == 1, "Initial refcount should be 1");

    /* Copy again (should increment refcount) */
    TestBlock *heap_block2 = (TestBlock *)_Block_copy(heap_block);
    ASSERT(heap_block2 == heap_block, "Second copy should return same pointer");
    refcount = _Block_refcount(heap_block);
    ASSERT(refcount == 2, "Refcount should be 2 after second copy");

    /* Release once */
    _Block_release(heap_block2);
    refcount = _Block_refcount(heap_block);
    ASSERT(refcount == 1, "Refcount should be 1 after one release");

    /* Release final reference */
    _Block_release(heap_block);

    PASS("Block copy and release");
    return 0;
}

/* ================================================================
 * TEST 2: Global Blocks
 * ================================================================ */

/* Global block (never freed) */
static TestBlock global_block = {
    .layout = {
        .isa = _NSConcreteGlobalBlock,
        .flags = BLOCK_IS_GLOBAL,
        .reserved = 0,
        .invoke = (void (*)(void *, ...))test_function_impl,
        .descriptor = &test_descriptor
    },
    .captured_value = 42
};

int test_global_blocks(void) {
    TEST("Global Blocks");

    /* Verify it's global */
    ASSERT(_Block_is_global(&global_block), "Should be global block");
    ASSERT(!_Block_is_heap(&global_block), "Global block should not be heap");

    /* Refcount should be 0 for global blocks */
    ASSERT(_Block_refcount(&global_block) == 0, "Global block refcount should be 0");

    /* Copy should return same pointer */
    TestBlock *copy = (TestBlock *)_Block_copy(&global_block);
    ASSERT(copy == &global_block, "Copying global block should return same pointer");

    /* Release should be no-op */
    _Block_release(&global_block);  /* Should not crash */

    PASS("Global blocks");
    return 0;
}

/* ================================================================
 * TEST 3: Block Size
 * ================================================================ */

int test_block_size(void) {
    TEST("Block Size");

    size_t size = Block_size(&global_block);
    ASSERT(size == sizeof(TestBlock), "Block_size should return correct size");

    PASS("Block size");
    return 0;
}

/* ================================================================
 * TEST 4: NULL Block Handling
 * ================================================================ */

int test_null_blocks(void) {
    TEST("NULL Block Handling");

    /* All functions should handle NULL gracefully */
    void *copy = _Block_copy(NULL);
    ASSERT(copy == NULL, "_Block_copy(NULL) should return NULL");

    _Block_release(NULL);  /* Should not crash */

    ASSERT(_Block_refcount(NULL) == 0, "_Block_refcount(NULL) should return 0");
    ASSERT(!_Block_is_heap(NULL), "_Block_is_heap(NULL) should return 0");
    ASSERT(!_Block_is_global(NULL), "_Block_is_global(NULL) should return 0");
    ASSERT(Block_size(NULL) == 0, "Block_size(NULL) should return 0");

    PASS("NULL block handling");
    return 0;
}

/* ================================================================
 * TEST 5: Block Invocation
 * ================================================================ */

int test_block_invocation(void) {
    TEST("Block Invocation");

    /* Create a block */
    TestBlock block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))test_function_impl,
            .descriptor = &test_descriptor
        },
        .captured_value = 100
    };

    /* Invoke the block directly */
    int (*invoke)(void *, int) = (int (*)(void *, int))block.layout.invoke;
    int result = invoke(&block, 50);

    ASSERT(result == 150, "Block invocation should add captured value");

    PASS("Block invocation");
    return 0;
}

/* ================================================================
 * TEST 6: Multiple Captures
 * ================================================================ */

typedef struct {
    struct Block_layout layout;
    int value1;
    int value2;
    const char *str;
} MultiCaptureBlock;

static int multi_capture_func(void *block_ptr, int arg) {
    MultiCaptureBlock *block = (MultiCaptureBlock *)block_ptr;
    printf("  Captured: %d, %d, \"%s\"\n", block->value1, block->value2, block->str);
    return arg + block->value1 + block->value2;
}

static struct Block_descriptor_1 multi_descriptor = {
    .reserved = 0,
    .size = sizeof(MultiCaptureBlock)
};

int test_multiple_captures(void) {
    TEST("Multiple Captures");

    MultiCaptureBlock block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))multi_capture_func,
            .descriptor = &multi_descriptor
        },
        .value1 = 10,
        .value2 = 20,
        .str = "test string"
    };

    /* Invoke */
    int (*invoke)(void *, int) = (int (*)(void *, int))block.layout.invoke;
    int result = invoke(&block, 5);

    ASSERT(result == 35, "Should add all captured values");
    ASSERT(strcmp(block.str, "test string") == 0, "String should be captured");

    PASS("Multiple captures");
    return 0;
}

/* ================================================================
 * TEST 7: Copy Preserves Captured Values
 * ================================================================ */

int test_copy_preserves_values(void) {
    TEST("Copy Preserves Captured Values");

    MultiCaptureBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))multi_capture_func,
            .descriptor = &multi_descriptor
        },
        .value1 = 123,
        .value2 = 456,
        .str = "preserved string"
    };

    /* Copy to heap */
    MultiCaptureBlock *heap_block = (MultiCaptureBlock *)_Block_copy(&stack_block);

    /* Verify values are preserved */
    ASSERT(heap_block->value1 == 123, "value1 should be preserved");
    ASSERT(heap_block->value2 == 456, "value2 should be preserved");
    ASSERT(strcmp(heap_block->str, "preserved string") == 0,
           "String should be preserved");

    /* Invoke and verify */
    int (*invoke)(void *, int) = (int (*)(void *, int))heap_block->layout.invoke;
    int result = invoke(heap_block, 1);
    ASSERT(result == 580, "Copied block should function correctly");

    _Block_release(heap_block);

    PASS("Copy preserves captured values");
    return 0;
}

/* ================================================================
 * TEST 8: Reference Counting Edge Cases
 * ================================================================ */

int test_refcount_edge_cases(void) {
    TEST("Reference Counting Edge Cases");

    TestBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))test_function_impl,
            .descriptor = &test_descriptor
        },
        .captured_value = 1
    };

    /* Multiple copies */
    void *copy1 = _Block_copy(&stack_block);
    void *copy2 = _Block_copy(copy1);
    void *copy3 = _Block_copy(copy2);

    /* All should point to same heap block */
    ASSERT(copy1 == copy2, "Copies should return same pointer");
    ASSERT(copy2 == copy3, "Copies should return same pointer");

    /* Refcount should be 3 */
    ASSERT(_Block_refcount(copy1) == 3, "Refcount should be 3");

    /* Release all */
    _Block_release(copy3);
    ASSERT(_Block_refcount(copy1) == 2, "Refcount should be 2");

    _Block_release(copy2);
    ASSERT(_Block_refcount(copy1) == 1, "Refcount should be 1");

    _Block_release(copy1);
    /* Block is now freed */

    PASS("Reference counting edge cases");
    return 0;
}

/* ================================================================
 * TEST 9: Stack Block Remains Valid
 * ================================================================ */

int test_stack_block_validity(void) {
    TEST("Stack Block Validity");

    TestBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))test_function_impl,
            .descriptor = &test_descriptor
        },
        .captured_value = 99
    };

    /* Stack block should be usable directly */
    int (*invoke)(void *, int) = (int (*)(void *, int))stack_block.layout.invoke;
    int result = invoke(&stack_block, 1);
    ASSERT(result == 100, "Stack block should work");

    /* Release on stack block should be no-op */
    _Block_release(&stack_block);  /* Should not crash */

    PASS("Stack block validity");
    return 0;
}

/* ================================================================
 * MAIN TEST RUNNER
 * ================================================================ */

int main(void) {
    int failures = 0;

    printf("==================================================\n");
    printf("  Apple Blocks Runtime Test Suite\n");
    printf("==================================================\n");

    failures += test_copy_release();
    failures += test_global_blocks();
    failures += test_block_size();
    failures += test_null_blocks();
    failures += test_block_invocation();
    failures += test_multiple_captures();
    failures += test_copy_preserves_values();
    failures += test_refcount_edge_cases();
    failures += test_stack_block_validity();

    printf("\n==================================================\n");
    if (failures == 0) {
        printf("  ALL TESTS PASSED!\n");
        printf("==================================================\n");
        printf("\nApple Blocks runtime is working correctly.\n");
        printf("For true block syntax support (^ operator),\n");
        printf("use Clang with -fblocks flag.\n");
        return 0;
    } else {
        printf("  %d TEST(S) FAILED\n", failures);
        printf("==================================================\n");
        return 1;
    }
}
