/*
 * Block_macros.h - Macro-based Apple Blocks Simulation
 *
 * Provides macro-based approximations of Apple Blocks syntax for compilers
 * that don't support the -fblocks extension (like PCC).
 *
 * LIMITATIONS:
 * - Not true compiler syntax (uses macros and structs)
 * - Variable capture must be explicit
 * - Less type-safe than real blocks
 * - More verbose syntax
 *
 * For true Blocks support, use Clang with -fblocks and link against libBlocksRuntime.
 *
 * USAGE:
 *   #include <Block_macros.h>
 *
 *   // Declare a block type
 *   DECLARE_BLOCK_TYPE(IntBlock, int, int x);
 *
 *   // Create a block instance
 *   DEFINE_BLOCK(IntBlock, double, myBlock) {
 *       // Capture variables
 *       CAPTURE(multiplier, double);
 *   } BLOCK_IMPL {
 *       return ARGS.x * CAPTURED.multiplier;
 *   }
 *
 *   // Invoke the block
 *   double multiplier = 2.5;
 *   int result = CALL_BLOCK(myBlock, &multiplier, 10);
 */

#ifndef _BLOCK_MACROS_H_
#define _BLOCK_MACROS_H_

#include "Block.h"

/* ================================================================
 * Block Type Declaration
 * ================================================================ */

/**
 * Declare a block type with its signature
 *
 * Example:
 *   DECLARE_BLOCK_TYPE(IntToInt, int, int x);
 *   Creates a type IntToInt_Block for blocks that take int and return int
 */
#define DECLARE_BLOCK_TYPE(name, return_type, ...) \
    typedef struct name##_args_t { \
        __VA_ARGS__; \
    } name##_args_t; \
    \
    typedef return_type (*name##_func_t)(name##_args_t args, void *captured); \
    \
    typedef struct name##_Block { \
        struct Block_layout layout; \
        name##_func_t impl; \
        void *captured; \
        size_t captured_size; \
    } name##_Block

/**
 * Forward declare a block variable
 */
#define BLOCK_VAR(block_type, name) \
    block_type##_Block name##_storage; \
    block_type##_Block *name = &name##_storage

/* ================================================================
 * Block Definition
 * ================================================================ */

/**
 * Define a block with captured variables
 *
 * Example:
 *   DEFINE_BLOCK(IntToInt, multiplier_capture, myBlock) {
 *       CAPTURE(multiplier, double);
 *   } BLOCK_IMPL {
 *       return ARGS.x * CAPTURED.multiplier;
 *   }
 */
#define DEFINE_BLOCK(block_type, capture_type, name) \
    /* Define captured variable structure */ \
    typedef struct name##_captured_t capture_type##_t; \
    struct name##_captured_t; \
    \
    /* Forward declare implementation function */ \
    static block_type##_func_t##return_type name##_impl( \
        block_type##_args_t args, void *captured_ptr); \
    \
    /* Define block initialization */ \
    static void name##_init(block_type##_Block *blk, void *captured) { \
        static struct Block_descriptor_1 desc = { 0, sizeof(block_type##_Block) }; \
        blk->layout.isa = _NSConcreteStackBlock; \
        blk->layout.flags = BLOCK_IS_GLOBAL; \
        blk->layout.reserved = 0; \
        blk->layout.invoke = (void (*)(void *, ...))name##_impl; \
        blk->layout.descriptor = &desc; \
        blk->impl = name##_impl; \
        blk->captured = captured; \
        blk->captured_size = sizeof(name##_captured_t); \
    } \
    \
    /* Captured variables structure definition follows */  \
    struct name##_captured_t

/**
 * Macro to access captured variables in block implementation
 */
#define CAPTURED (*(name##_captured_t *)captured_ptr)

/**
 * Macro to access block arguments
 */
#define ARGS args

/**
 * Begin block implementation
 */
#define BLOCK_IMPL \
    ; static block_type##_func_t##return_type name##_impl( \
        block_type##_args_t args, void *captured_ptr)

/* ================================================================
 * Block Invocation
 * ================================================================ */

/**
 * Call a block
 *
 * Example:
 *   int result = CALL_BLOCK(myBlock, &captured_vars, 42);
 */
#define CALL_BLOCK(block, captured, ...) \
    ((block)->impl((block_type##_args_t){__VA_ARGS__}, captured))

/**
 * Invoke a block (alternative syntax)
 */
#define INVOKE_BLOCK(block, ...) \
    CALL_BLOCK(block, (block)->captured, __VA_ARGS__)

/* ================================================================
 * Simplified Block Macros (for simple cases)
 * ================================================================ */

/**
 * Create a simple block without captured variables
 *
 * Example:
 *   SIMPLE_BLOCK(int, add, (int a, int b), {
 *       return a + b;
 *   });
 */
#define SIMPLE_BLOCK(return_type, name, params, body) \
    static return_type name##_impl params body \
    \
    static struct Block_layout name##_layout = { \
        .isa = _NSConcreteGlobalBlock, \
        .flags = BLOCK_IS_GLOBAL, \
        .reserved = 0, \
        .invoke = (void (*)(void *, ...))name##_impl, \
        .descriptor = &(struct Block_descriptor_1){ \
            .reserved = 0, \
            .size = sizeof(struct Block_layout) \
        } \
    }; \
    \
    static struct Block_layout *name = &name##_layout

/* ================================================================
 * Stack Block Helpers
 * ================================================================ */

/**
 * Declare a stack block (will be copied to heap when needed)
 */
#define STACK_BLOCK_BEGIN(block_type, name, captured_vars) \
    block_type##_Block name##_storage; \
    block_type##_Block *name = &name##_storage; \
    name##_init(name, &captured_vars)

/**
 * Copy a stack block to the heap
 */
#define COPY_BLOCK(block) \
    ((typeof(block))Block_copy(block))

/**
 * Release a heap block
 */
#define RELEASE_BLOCK(block) \
    Block_release(block)

/* ================================================================
 * Block Literals (Limited Support)
 * ================================================================ */

/**
 * Inline block literal (very limited - for demonstrations only)
 *
 * Example:
 *   void process(int (^block)(int));
 *
 *   // With macro:
 *   BLOCK_LITERAL(int_block, int, (int x), { return x * 2; });
 *   process((void *)&int_block_layout);
 */
#define BLOCK_LITERAL(name, return_type, params, body) \
    static return_type name##_func params body \
    static struct Block_layout name##_layout = { \
        _NSConcreteGlobalBlock, \
        BLOCK_IS_GLOBAL, \
        0, \
        (void (*)(void *, ...))name##_func, \
        &(struct Block_descriptor_1){ 0, sizeof(struct Block_layout) } \
    }

/* ================================================================
 * Foreach with Blocks (Common Pattern)
 * ================================================================ */

/**
 * Iterate with a block
 *
 * Example:
 *   int array[] = {1, 2, 3, 4, 5};
 *   FOREACH_BLOCK(i, 0, 5, {
 *       printf("%d\n", array[i]);
 *   });
 */
#define FOREACH_BLOCK(var, start, end, block_body) \
    do { \
        for (int var = (start); var < (end); var++) { \
            block_body \
        } \
    } while (0)

/* ================================================================
 * Completion Handler Pattern
 * ================================================================ */

/**
 * Declare a completion handler type
 *
 * Example:
 *   COMPLETION_HANDLER(NetworkCompletion, void,
 *                      int status_code; const char *data);
 */
#define COMPLETION_HANDLER(name, return_type, ...) \
    DECLARE_BLOCK_TYPE(name, return_type, __VA_ARGS__)

/**
 * Call a completion handler
 */
#define CALL_COMPLETION(handler, ...) \
    INVOKE_BLOCK(handler, __VA_ARGS__)

/* ================================================================
 * GCD-Style Dispatch Blocks
 * ================================================================ */

/**
 * Dispatch block type (no arguments, no return)
 */
typedef void (*dispatch_block_t)(void);

/**
 * Create a dispatch block
 */
#define DISPATCH_BLOCK(name, body) \
    static void name##_impl(void) body \
    static struct Block_layout name##_layout = { \
        _NSConcreteGlobalBlock, \
        BLOCK_IS_GLOBAL, \
        0, \
        (void (*)(void *, ...))name##_impl, \
        &(struct Block_descriptor_1){ 0, sizeof(struct Block_layout) } \
    }; \
    dispatch_block_t name = (dispatch_block_t)&name##_layout

/* ================================================================
 * Debugging Helpers
 * ================================================================ */

#ifdef BLOCK_DEBUG

/**
 * Print block information
 */
#define BLOCK_INFO(block) \
    _Block_dump((void *)block)

/* Declare _Block_dump */
extern void _Block_dump(void *block);

#endif /* BLOCK_DEBUG */

/* ================================================================
 * Notes on True Block Syntax
 * ================================================================ */

/*
 * With compiler support (Clang -fblocks), you would write:
 *
 *   int multiplier = 10;
 *   int (^myBlock)(int) = ^(int x) {
 *       return x * multiplier;  // Automatically captures multiplier
 *   };
 *   int result = myBlock(5);  // Returns 50
 *
 * With these macros, you must write:
 *
 *   DECLARE_BLOCK_TYPE(IntBlock, int, int x);
 *
 *   struct { int multiplier; } captured = { 10 };
 *
 *   DEFINE_BLOCK(IntBlock, captured, myBlock) {
 *       int multiplier;
 *   } BLOCK_IMPL {
 *       return ARGS.x * CAPTURED.multiplier;
 *   }
 *
 *   myBlock_init(&myBlock_storage, &captured);
 *   int result = CALL_BLOCK(&myBlock_storage, &captured, 5);
 *
 * The macro version is more verbose but provides similar functionality
 * without requiring compiler modifications.
 */

#endif /* _BLOCK_MACROS_H_ */
