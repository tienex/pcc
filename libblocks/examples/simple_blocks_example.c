/*
 * Simple Blocks Example (Without Compiler Support)
 *
 * Demonstrates using blocks with the runtime library and macro helpers.
 * This does NOT require Clang's -fblocks support.
 */

#include <stdio.h>
#include "../Block.h"

/* ================================================================
 * Example 1: Simple Global Block
 * ================================================================ */

/* Define a simple function to use as a block */
static void hello_impl(void *block_ptr) {
    (void)block_ptr;  /* Unused for simple blocks */
    printf("Hello from a block!\n");
}

/* Define the block descriptor */
static struct Block_descriptor_1 hello_descriptor = {
    .reserved = 0,
    .size = sizeof(struct Block_layout)
};

/* Define the global block */
static struct Block_layout hello_block = {
    .isa = _NSConcreteGlobalBlock,
    .flags = BLOCK_IS_GLOBAL,
    .reserved = 0,
    .invoke = (void (*)(void *, ...))hello_impl,
    .descriptor = &hello_descriptor
};

void example_simple_block(void) {
    printf("=== Example 1: Simple Global Block ===\n");

    /* Invoke the block */
    void (*invoke)(void *) = (void (*)(void *))hello_block.invoke;
    invoke(&hello_block);

    printf("\n");
}

/* ================================================================
 * Example 2: Block with Captured Value
 * ================================================================ */

typedef struct {
    struct Block_layout layout;
    int multiplier;
} MultiplyBlock;

static int multiply_impl(void *block_ptr, int x) {
    MultiplyBlock *block = (MultiplyBlock *)block_ptr;
    return x * block->multiplier;
}

static struct Block_descriptor_1 multiply_descriptor = {
    .reserved = 0,
    .size = sizeof(MultiplyBlock)
};

void example_captured_value(void) {
    printf("=== Example 2: Block with Captured Value ===\n");

    /* Create a stack block that captures a value */
    MultiplyBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))multiply_impl,
            .descriptor = &multiply_descriptor
        },
        .multiplier = 10
    };

    /* Invoke the stack block */
    int (*invoke)(void *, int) = (int (*)(void *, int))stack_block.layout.invoke;
    int result = invoke(&stack_block, 5);

    printf("  5 * 10 = %d\n", result);

    /* Copy to heap (if we need to return it from a function) */
    MultiplyBlock *heap_block = (MultiplyBlock *)Block_copy(&stack_block);

    result = invoke(heap_block, 7);
    printf("  7 * 10 = %d\n", result);

    /* Release when done */
    Block_release(heap_block);

    printf("\n");
}

/* ================================================================
 * Example 3: Multiple Captured Values
 * ================================================================ */

typedef struct {
    struct Block_layout layout;
    int a;
    int b;
    const char *operation;
} CalculatorBlock;

static int calculator_impl(void *block_ptr, int x) {
    CalculatorBlock *block = (CalculatorBlock *)block_ptr;
    printf("  Performing %s with a=%d, b=%d, x=%d\n",
           block->operation, block->a, block->b, x);
    return (x + block->a) * block->b;
}

static struct Block_descriptor_1 calculator_descriptor = {
    .reserved = 0,
    .size = sizeof(CalculatorBlock)
};

void example_multiple_captures(void) {
    printf("=== Example 3: Multiple Captured Values ===\n");

    CalculatorBlock calc_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))calculator_impl,
            .descriptor = &calculator_descriptor
        },
        .a = 5,
        .b = 2,
        .operation = "addition then multiplication"
    };

    int (*invoke)(void *, int) = (int (*)(void *, int))calc_block.layout.invoke;
    int result = invoke(&calc_block, 10);

    printf("  Result: (10 + 5) * 2 = %d\n", result);

    printf("\n");
}

/* ================================================================
 * Example 4: Array of Blocks
 * ================================================================ */

typedef struct {
    struct Block_layout layout;
    int offset;
} OffsetBlock;

static int offset_impl(void *block_ptr, int x) {
    OffsetBlock *block = (OffsetBlock *)block_ptr;
    return x + block->offset;
}

static struct Block_descriptor_1 offset_descriptor = {
    .reserved = 0,
    .size = sizeof(OffsetBlock)
};

void example_array_of_blocks(void) {
    printf("=== Example 4: Array of Blocks ===\n");

    /* Create multiple blocks with different captured values */
    OffsetBlock blocks[3];

    for (int i = 0; i < 3; i++) {
        blocks[i] = (OffsetBlock){
            .layout = {
                .isa = _NSConcreteStackBlock,
                .flags = 0,
                .reserved = 0,
                .invoke = (void (*)(void *, ...))offset_impl,
                .descriptor = &offset_descriptor
            },
            .offset = (i + 1) * 10
        };
    }

    /* Invoke each block */
    int (*invoke)(void *, int) = (int (*)(void *, int))offset_impl;

    for (int i = 0; i < 3; i++) {
        int result = invoke(&blocks[i], 5);
        printf("  Block %d: 5 + %d = %d\n", i, (i + 1) * 10, result);
    }

    printf("\n");
}

/* ================================================================
 * Example 5: Passing Blocks as Arguments
 * ================================================================ */

typedef struct {
    struct Block_layout layout;
    /* No captured values for this example */
} PrintBlock;

static void print_impl(void *block_ptr, int value) {
    (void)block_ptr;
    printf("    Value: %d\n", value);
}

static struct Block_descriptor_1 print_descriptor = {
    .reserved = 0,
    .size = sizeof(PrintBlock)
};

/* Function that takes a block as a callback */
void process_array(int *array, int count, struct Block_layout *block) {
    void (*invoke)(void *, int) = (void (*)(void *, int))block->invoke;

    for (int i = 0; i < count; i++) {
        invoke(block, array[i]);
    }
}

void example_block_callback(void) {
    printf("=== Example 5: Passing Blocks as Callbacks ===\n");

    PrintBlock print_block = {
        .layout = {
            .isa = _NSConcreteGlobalBlock,
            .flags = BLOCK_IS_GLOBAL,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))print_impl,
            .descriptor = &print_descriptor
        }
    };

    int numbers[] = {10, 20, 30, 40, 50};

    printf("  Processing array with block callback:\n");
    process_array(numbers, 5, &print_block.layout);

    printf("\n");
}

/* ================================================================
 * Example 6: Reference Counting
 * ================================================================ */

void example_reference_counting(void) {
    printf("=== Example 6: Reference Counting ===\n");

    /* Create a stack block */
    MultiplyBlock stack_block = {
        .layout = {
            .isa = _NSConcreteStackBlock,
            .flags = 0,
            .reserved = 0,
            .invoke = (void (*)(void *, ...))multiply_impl,
            .descriptor = &multiply_descriptor
        },
        .multiplier = 3
    };

    printf("  Stack block refcount: %d\n", _Block_refcount(&stack_block));
    printf("  Is heap? %s\n", _Block_is_heap(&stack_block) ? "yes" : "no");

    /* Copy to heap */
    MultiplyBlock *heap1 = (MultiplyBlock *)Block_copy(&stack_block);
    printf("  After first copy, refcount: %d\n", _Block_refcount(heap1));
    printf("  Is heap? %s\n", _Block_is_heap(heap1) ? "yes" : "no");

    /* Copy again (increments refcount) */
    MultiplyBlock *heap2 = (MultiplyBlock *)Block_copy(heap1);
    printf("  After second copy, refcount: %d\n", _Block_refcount(heap2));
    printf("  Both pointers equal? %s\n", heap1 == heap2 ? "yes" : "no");

    /* Release first reference */
    Block_release(heap1);
    printf("  After one release, refcount: %d\n", _Block_refcount(heap2));

    /* Release final reference (frees the block) */
    Block_release(heap2);
    printf("  Block has been freed\n");

    printf("\n");
}

/* ================================================================
 * Main
 * ================================================================ */

int main(void) {
    printf("==================================================\n");
    printf("  Apple Blocks Runtime Examples\n");
    printf("  (Without Compiler Support)\n");
    printf("==================================================\n\n");

    example_simple_block();
    example_captured_value();
    example_multiple_captures();
    example_array_of_blocks();
    example_block_callback();
    example_reference_counting();

    printf("==================================================\n");
    printf("  Examples Complete!\n");
    printf("==================================================\n\n");

    printf("NOTE: These examples use manual block creation.\n");
    printf("With Clang and -fblocks, the syntax would be:\n\n");

    printf("  int multiplier = 10;\n");
    printf("  int (^myBlock)(int) = ^(int x) {\n");
    printf("      return x * multiplier;\n");
    printf("  };\n");
    printf("  int result = myBlock(5);  // Returns 50\n\n");

    printf("The runtime library provides the foundation,\n");
    printf("but true block syntax requires compiler support.\n");

    return 0;
}
