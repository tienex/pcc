# libblocks - Apple Blocks Runtime for PCC

## Overview

libblocks provides runtime support for Apple's Blocks C language extension. Blocks are closures (anonymous functions with state capture) introduced by Apple for C, C++, and Objective-C.

**Standard Proposal:** WG14 N1451 (submitted 2010, not adopted into C standard)
**Reference Implementation:** LLVM compiler-rt BlocksRuntime
**License:** Dual licensed under UIUC or MIT (your choice)

## Features

### Runtime Library

- **Block memory management** - `_Block_copy()`, `_Block_release()`
- **Reference counting** - Automatic memory management for heap blocks
- **Stack to heap promotion** - Automatically copy blocks when needed
- **Variable capture support** - Capture and manage variables from enclosing scope
- **Objective-C compatibility** - Compatible with Cocoa and GCD APIs

### Macro-Based Workarounds

Since PCC doesn't yet support the `^` block syntax at the compiler level, we provide macro-based approximations in `Block_macros.h`:

- Declare block types
- Define blocks with captured variables
- Invoke blocks
- Simple blocks without captures
- Completion handler patterns
- GCD-style dispatch blocks

## Installation

Included with PCC:

```bash
cd libblocks
make
sudo make install
```

## Quick Start

### With Compiler Support (Clang -fblocks)

```c
#include <Block.h>
#include <stdio.h>

int main(void) {
    int multiplier = 10;

    /* Define a block */
    int (^myBlock)(int) = ^(int x) {
        return x * multiplier;  /* Captures multiplier automatically */
    };

    /* Invoke the block */
    int result = myBlock(5);  /* Returns 50 */
    printf("Result: %d\n", result);

    /* Copy to heap if needed */
    int (^heapBlock)(int) = Block_copy(myBlock);
    result = heapBlock(3);    /* Returns 30 */

    /* Release when done */
    Block_release(heapBlock);

    return 0;
}
```

Compile with Clang:
```bash
clang -fblocks program.c -lBlocksRuntime -o program
```

### With Macro Workarounds (PCC without -fblocks)

```c
#include <Block_macros.h>
#include <stdio.h>

/* Declare a block type: int block(int x) */
DECLARE_BLOCK_TYPE(IntBlock, int, int x);

int main(void) {
    /* Define captured variables */
    struct {
        int multiplier;
    } captured = { .multiplier = 10 };

    /* Define the block */
    DEFINE_BLOCK(IntBlock, captured, myBlock) {
        int multiplier;
    } BLOCK_IMPL {
        return ARGS.x * CAPTURED.multiplier;
    }

    /* Initialize the block */
    IntBlock_Block myBlock_storage;
    IntBlock_Block *myBlock = &myBlock_storage;
    myBlock_init(myBlock, &captured);

    /* Invoke the block */
    int result = CALL_BLOCK(myBlock, &captured, 5);  /* Returns 50 */
    printf("Result: %d\n", result);

    return 0;
}
```

Compile with PCC:
```bash
pcc program.c -lblocks -o program
```

## API Documentation

### Core Functions

```c
/* Copy a block to the heap */
void *_Block_copy(const void *aBlock);

/* Type-safe wrapper */
#define Block_copy(...) \
    ((__typeof__(__VA_ARGS__))_Block_copy(__VA_ARGS__))

/* Release a block */
void _Block_release(const void *aBlock);

/* Type-safe wrapper */
#define Block_release(...) \
    _Block_release(__VA_ARGS__)

/* Query functions */
int _Block_refcount(const void *aBlock);
int _Block_is_heap(const void *aBlock);
int _Block_is_global(const void *aBlock);
size_t Block_size(void *aBlock);
const char *Block_signature(void *aBlock);
```

### Block Types

- **Stack blocks** - Allocated on the stack, captured variables are on stack
- **Heap blocks** - Copied to heap with `Block_copy()`, persist beyond stack frame
- **Global blocks** - No captured variables, never deallocated

### Memory Management Rules

1. **Stack blocks** - Created automatically, deallocated when function returns
2. **Copying** - Use `Block_copy()` to move stack block to heap
3. **Releasing** - Always pair `Block_copy()` with `Block_release()`
4. **Global blocks** - Never need to copy or release
5. **Reference counting** - Each `Block_copy()` increments refcount, `Block_release()` decrements

## Examples

### Example 1: Simple Block

```c
/* With compiler support */
void (^hello)(void) = ^{
    printf("Hello from block!\n");
};

hello();  /* Invoke the block */
```

### Example 2: Block with Arguments and Return Value

```c
int (^add)(int, int) = ^(int a, int b) {
    return a + b;
};

int sum = add(5, 3);  /* Returns 8 */
```

### Example 3: Capturing Variables

```c
int multiplier = 10;

int (^multiply)(int) = ^(int x) {
    return x * multiplier;  /* Captures 'multiplier' by value */
};

int result = multiply(5);  /* Returns 50 */

multiplier = 20;  /* Doesn't affect the block */
result = multiply(5);  /* Still returns 50 */
```

### Example 4: Mutable Variables (__block)

```c
__block int counter = 0;

void (^increment)(void) = ^{
    counter++;  /* Can modify __block variable */
};

increment();
increment();
printf("Counter: %d\n", counter);  /* Prints: Counter: 2 */
```

### Example 5: Completion Handlers

```c
typedef void (^CompletionHandler)(int status, const char *data);

void fetchData(const char *url, CompletionHandler completion) {
    /* Simulate async operation */
    int status = 200;
    const char *data = "Hello, World!";

    /* Call completion handler */
    completion(status, data);
}

/* Usage */
fetchData("http://example.com", ^(int status, const char *data) {
    printf("Status: %d, Data: %s\n", status, data);
});
```

### Example 6: Iteration with Blocks

```c
typedef void (^iterator_t)(int index, int value);

void forEach(int *array, int count, iterator_t block) {
    for (int i = 0; i < count; i++) {
        block(i, array[i]);
    }
}

/* Usage */
int numbers[] = {1, 2, 3, 4, 5};
forEach(numbers, 5, ^(int index, int value) {
    printf("[%d] = %d\n", index, value);
});
```

## Performance

- **Global blocks** - Zero overhead (inline function call)
- **Stack blocks** - Minimal overhead (stack allocation)
- **Heap blocks** - Malloc overhead + reference counting
- **Copying** - `O(n)` where n is size of captured variables
- **Reference counting** - Atomic operations (thread-safe)

## Limitations

### Without Compiler Support (Macro Workarounds)

- More verbose syntax
- Manual capture specification
- Less type-safe
- No automatic variable capture
- No `__block` storage class

### With Compiler Support (Future PCC Enhancement)

For full blocks support in PCC, compiler modifications needed:
1. **Lexer** - Recognize `^` operator for block literals
2. **Parser** - Parse block syntax and captured variables
3. **Semantic analysis** - Track captured variables and their lifetimes
4. **Code generation** - Generate block structure and helper functions
5. **ABI** - Follow Apple Blocks ABI specification

See `BLOCKS_COMPILER_SPEC.md` for detailed implementation guide.

## Compatibility

### Language Standards

- **C99** - Minimum requirement (designated initializers, compound literals)
- **C11** - Full support
- **C++** - Compatible (but consider C++11 lambdas instead)
- **Objective-C** - Full compatibility with Cocoa frameworks

### Compilers

- **Clang** - Full support with `-fblocks` flag
- **GCC** - Limited support (Apple branch only)
- **PCC** - Runtime support via macros (no syntax yet)

### Platforms

- **macOS** - Native support (OS X 10.6+)
- **iOS** - Native support (iOS 4.0+)
- **Linux** - Supported with Clang + libBlocksRuntime
- **FreeBSD** - Supported with Clang + libBlocksRuntime
- **Windows** - Supported with Clang + mingw

## Real-World Usage

### Grand Central Dispatch (GCD)

```c
#include <dispatch/dispatch.h>

dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
    /* Background work */
    printf("Running in background\n");

    dispatch_async(dispatch_get_main_queue(), ^{
        /* Update UI on main thread */
        printf("Back on main thread\n");
    });
});
```

### Cocoa Frameworks

```objc
/* NSArray enumeration */
[array enumerateObjectsUsingBlock:^(id obj, NSUInteger idx, BOOL *stop) {
    NSLog(@"Object %lu: %@", idx, obj);
}];

/* Animation */
[UIView animateWithDuration:0.3 animations:^{
    view.alpha = 0.0;
} completion:^(BOOL finished) {
    [view removeFromSuperview];
}];
```

### Sorting with Blocks

```c
#include <stdlib.h>

int (^comparator)(const void *, const void *) = ^(const void *a, const void *b) {
    int x = *(const int *)a;
    int y = *(const int *)b;
    return (x > y) - (x < y);
};

int numbers[] = {5, 2, 8, 1, 9};
qsort_b(numbers, 5, sizeof(int), comparator);
```

## Testing

Run the test suite:

```bash
make test
```

Tests cover:
- Block copy and release
- Reference counting
- Stack to heap promotion
- Variable capture
- Global blocks
- Memory management

## References

1. **Apple Blocks Programming Guide**
   - https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Blocks/

2. **Clang Block Implementation Specification**
   - https://clang.llvm.org/docs/Block-ABI-Apple.html

3. **WG14 N1451 Proposal**
   - Block Objects for C (submitted to ISO C committee)

4. **LLVM compiler-rt BlocksRuntime**
   - https://github.com/llvm/llvm-project/tree/main/compiler-rt/lib/BlocksRuntime

5. **mackyle/blocksruntime**
   - Standalone BlocksRuntime implementation
   - https://github.com/mackyle/blocksruntime

## Contributing

When contributing to libblocks:

1. Maintain compatibility with Apple's BlocksRuntime ABI
2. Follow the reference implementation behavior
3. Add tests for new features
4. Update documentation

## License

This library is dual-licensed under:
- **University of Illinois/NCSA Open Source License (UIUC)**
- **MIT License**

Choose the license that best suits your needs. Both are GPL-compatible and permissive.

---

**Apple, Mac OS X, iOS, Objective-C, and Cocoa are trademarks of Apple Inc.**

This is an independent open-source implementation compatible with Apple's Blocks specification.
