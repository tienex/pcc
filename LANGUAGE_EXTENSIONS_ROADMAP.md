# Language Extensions Roadmap for PCC

## Overview

This document outlines potential language extensions for the Portable C Compiler (PCC), inspired by historical and modern compiler innovations. These features would require compiler frontend modifications beyond simple library support.

## Status Summary

| Feature | Status | Complexity | Priority |
|---------|--------|------------|----------|
| Apple Blocks | Documented | High | Medium |
| MetaWare Generators | Documented | Very High | Low |
| IEEE 754-2008 Decimal | ✅ Implemented | Medium | High |

---

## 1. Apple Blocks Extension

### What Are Blocks?

Apple Blocks are a C language extension that adds closures (anonymous functions with state capture) to C, C++, and Objective-C. First introduced in Mac OS X 10.6 (2009) and iOS 4.0 (2010).

**Standard Proposal:** WG14 N1451 (submitted to ISO C committee in 2010, not adopted)

### Syntax Examples

#### Basic Block Literal

```c
// Traditional function pointer
int (*func_ptr)(int, int) = NULL;

// Block with same signature
int (^block_ptr)(int, int) = ^(int a, int b) {
    return a + b;
};

// Call it
int result = block_ptr(5, 3);  // Returns 8
```

#### Variable Capture

```c
int multiplier = 10;

// Block captures 'multiplier' from enclosing scope
int (^multiply)(int) = ^(int value) {
    return value * multiplier;  // Captures multiplier
};

printf("%d\n", multiply(5));  // Prints 50
```

#### Mutable Capture with __block

```c
__block int counter = 0;

void (^increment)(void) = ^{
    counter++;  // Can modify __block variables
};

increment();
increment();
printf("%d\n", counter);  // Prints 2
```

#### Blocks as Callbacks

```c
void process_array(int *arr, size_t len, void (^callback)(int)) {
    for (size_t i = 0; i < len; i++) {
        callback(arr[i]);
    }
}

// Usage
int numbers[] = {1, 2, 3, 4, 5};
process_array(numbers, 5, ^(int n) {
    printf("Number: %d\n", n);
});
```

### Runtime Requirements

Blocks require runtime support for:

1. **Block descriptor structure**
```c
struct Block_descriptor {
    unsigned long int reserved;
    unsigned long int size;
    void (*copy)(void *dst, void *src);
    void (*dispose)(void *);
};
```

2. **Block literal structure**
```c
struct Block_literal {
    void *isa;                        // Pointer to class (runtime)
    int flags;                        // Flags
    int reserved;
    void (*invoke)(void *, ...);      // Function pointer
    struct Block_descriptor *descriptor;
    // Captured variables follow...
};
```

3. **Runtime helper functions**
```c
void *_Block_copy(void *block);
void _Block_release(void *block);
void _Block_object_assign(void *dest, void *src, int flags);
void _Block_object_dispose(void *obj, int flags);
```

### Compiler Implementation Requirements

#### Parser Changes (scan.l, cgram.y)

1. Add `^` as block operator token
2. Parse block literal syntax: `^return_type (params) { body }`
3. Parse `__block` storage class specifier
4. Handle block type declarations: `return_type (^block_name)(params)`

#### AST Modifications (tree.h, tree.c)

1. New node type `BLOCK_EXPR` for block literals
2. Track captured variables and their capture modes (by-value vs `__block`)
3. Build closure environment structure

#### Code Generation (ccom/)

1. **Transform block to struct + function:**
   ```c
   // Original:
   int (^add)(int, int) = ^(int a, int b) { return a + b; };

   // Transformed:
   struct __block_literal_1 {
       void *isa;
       int flags;
       void (*invoke)(struct __block_literal_1 *, int, int);
   };

   int __block_func_1(struct __block_literal_1 *self, int a, int b) {
       return a + b;
   }

   struct __block_literal_1 add = {
       .isa = &_NSConcreteStackBlock,
       .flags = 0,
       .invoke = __block_func_1
   };
   ```

2. **Generate copy/dispose helpers** for captured objects
3. **Manage block storage:**
   - Stack blocks (default)
   - Heap blocks (via `Block_copy`)
   - Global blocks (for literals with no captures)

#### Type System (mip/common.c)

1. Add block pointer types distinct from function pointers
2. Type checking for block assignments
3. Conversion rules between function pointers and blocks

### Implementation Roadmap

**Phase 1: Foundation (4-6 weeks)**
- [ ] Lexer support for `^` operator
- [ ] Parser grammar for block literals
- [ ] Basic AST representation
- [ ] Minimal runtime library (libblocks-runtime)

**Phase 2: Code Generation (6-8 weeks)**
- [ ] Transform blocks to struct + function
- [ ] Generate invoke functions
- [ ] Stack block allocation

**Phase 3: Capture Semantics (4-6 weeks)**
- [ ] Identify captured variables
- [ ] By-value capture implementation
- [ ] `__block` storage class

**Phase 4: Memory Management (3-4 weeks)**
- [ ] Heap block allocation (Block_copy)
- [ ] Reference counting
- [ ] Copy/dispose helpers

**Phase 5: Advanced Features (4-6 weeks)**
- [ ] Nested blocks
- [ ] Block imports in other blocks
- [ ] Objective-C integration (if needed)

**Estimated Total:** 21-30 weeks (5-7 months)

### Testing Strategy

1. Unit tests for each syntax feature
2. Capture tests (by-value, `__block`, nested)
3. Memory management tests (stack, heap, global)
4. Integration with existing C code
5. Performance benchmarks vs function pointers

### References

- **Specification:** https://clang.llvm.org/docs/BlockLanguageSpec.html
- **ABI:** https://clang.llvm.org/docs/Block-ABI-Apple.html
- **Runtime:** https://github.com/apple/swift-corelibs-libdispatch (includes BlocksRuntime)
- **WG14 N1451:** Blocks proposal to ISO C committee

---

## 2. MetaWare Generator Coroutines

### What Are Generator Coroutines?

MetaWare's High C compiler (1989) featured Python-style generator coroutines decades before they became popular. This allowed functions to yield multiple values over time rather than returning once.

**Historical Context:** Documented in FM Towns High C compiler manual (Fujitsu, late 1980s)

### Syntax Examples

#### Basic Generator

```c
// Generator function declaration
void traverse_tree(TreeNode *root) -> (int value) {
    if (root == NULL) return;

    // Yield left subtree
    traverse_tree(root->left);

    // Yield current value
    yield(root->value);

    // Yield right subtree
    traverse_tree(root->right);
}

// Iterate over yielded values
for value <- traverse_tree(tree_root) do {
    printf("%d\n", value);
}
```

#### Generator with Multiple Yields

```c
void fibonacci(int count) -> (int number) {
    int a = 0, b = 1;

    for (int i = 0; i < count; i++) {
        yield(a);
        int temp = a;
        a = b;
        b = temp + b;
    }
}

// Usage
for num <- fibonacci(10) do {
    printf("%d ", num);  // Prints: 0 1 1 2 3 5 8 13 21 34
}
```

#### Recursive Generator

```c
void permutations(int *arr, int start, int end) -> (int *perm) {
    if (start == end) {
        yield(arr);
        return;
    }

    for (int i = start; i <= end; i++) {
        // Swap
        int temp = arr[start];
        arr[start] = arr[i];
        arr[i] = temp;

        // Recurse and yield from nested call
        permutations(arr, start + 1, end);

        // Swap back
        temp = arr[start];
        arr[start] = arr[i];
        arr[i] = temp;
    }
}

// Generate all permutations
int data[] = {1, 2, 3};
for perm <- permutations(data, 0, 2) do {
    printf("%d %d %d\n", perm[0], perm[1], perm[2]);
}
```

### Implementation Approaches

#### Option 1: Continuation-Based (Simplest)

Use setjmp/longjmp for coroutine switching:

```c
typedef struct {
    jmp_buf env;           // Saved execution state
    void *stack;           // Dedicated stack space
    int state;             // Current state
    void *yielded_value;   // Last yielded value
} generator_t;

void *yield(void *value) {
    current_generator->yielded_value = value;
    // Switch back to caller
    longjmp(caller_env, 1);
}
```

**Pros:**
- Portable (standard C)
- Relatively simple

**Cons:**
- Stack management complex
- Performance overhead
- Not fully safe (stack corruption risks)

#### Option 2: State Machine Transform (Robust)

Transform generator into state machine:

```c
// Original:
void numbers() -> (int n) {
    yield(1);
    yield(2);
    yield(3);
}

// Transformed:
struct numbers_state {
    int pc;  // Program counter (state)
};

int numbers_next(struct numbers_state *state) {
    switch (state->pc) {
        case 0: state->pc = 1; return 1;
        case 1: state->pc = 2; return 2;
        case 2: state->pc = 3; return 3;
        case 3: return -1;  // Done
    }
}
```

**Pros:**
- No stack management needed
- Efficient
- Safe

**Cons:**
- Complex transformation
- Cannot preserve all local variables across yields
- Nested calls challenging

#### Option 3: Fiber/Coroutine Library (Best)

Use dedicated coroutine library (libucontext, libcoro):

```c
typedef struct {
    ucontext_t context;
    void *stack;
    size_t stack_size;
    void *yielded_value;
} fiber_t;

void generator_wrapper(fiber_t *gen, void (*func)(void)) {
    func();  // Run generator
    gen->done = true;
}
```

**Pros:**
- Full coroutine support
- Handles nested calls
- Preserves all local state

**Cons:**
- Platform-specific (requires ucontext or similar)
- Larger runtime dependency

### Compiler Implementation Requirements

#### Parser Changes

1. Add `->` operator for generator return type
2. Parse `yield(expr)` statements
3. Parse `for var <- generator() do { }` loops
4. Recognize generator function declarations

#### AST Modifications

1. `GENERATOR_FUNC` node type
2. `YIELD_STMT` node type
3. `FOR_GENERATOR` node type
4. Track generator state variables

#### Code Generation

1. **Transform generator to state structure:**
```c
struct generator_state {
    int pc;               // Program counter
    int locals[N];        // Saved local variables
    void *return_value;   // Yielded value
    bool done;            // Completion flag
};
```

2. **Transform yields to state saves:**
```c
// Before yield:
save_locals_to_state();
state->pc = next_state_label;
return yielded_value;

// After yield (on resume):
restore_locals_from_state();
goto *state->pc;
```

3. **Transform for-generator loops:**
```c
// Original:
for x <- generator() do {
    process(x);
}

// Transformed:
struct generator_state *gen = generator_init();
while (!gen->done) {
    x = generator_next(gen);
    if (!gen->done) {
        process(x);
    }
}
generator_free(gen);
```

### Implementation Roadmap

**Phase 1: Research & Design (2-3 weeks)**
- [ ] Study original MetaWare implementation
- [ ] Choose implementation strategy (state machine vs fibers)
- [ ] Design generator runtime API

**Phase 2: Parser (4-6 weeks)**
- [ ] Lexer tokens for `->` and `yield`
- [ ] Grammar for generator declarations
- [ ] Grammar for `for-<-` loops

**Phase 3: Transformation (8-12 weeks)**
- [ ] State machine generation
- [ ] Local variable capture analysis
- [ ] Yield point transformation

**Phase 4: Runtime (3-4 weeks)**
- [ ] Generator state management
- [ ] Iterator protocol
- [ ] Memory management

**Phase 5: Advanced Features (6-8 weeks)**
- [ ] Nested generator calls
- [ ] Recursive generators
- [ ] Exception handling in generators

**Phase 6: Optimization (4-6 weeks)**
- [ ] Minimize state size
- [ ] Eliminate unnecessary saves/restores
- [ ] Inline simple generators

**Estimated Total:** 27-39 weeks (6-9 months)

### Challenges

1. **Local Variable Preservation**
   - Must save/restore all local variables across yields
   - Pointers to locals become invalid after yield

2. **Recursive Generators**
   - Each recursive call needs own state
   - Stack of generator states required

3. **Exception Handling**
   - What happens if generator throws exception?
   - Cleanup of partial generator state

4. **Performance**
   - State saves/restores have overhead
   - May be slower than manual iteration

### References

- **Historical:** "The lost language extensions of MetaWare's High C Compiler" (duriansoftware.com)
- **Modern Equivalent:** Python generators, C# yield, JavaScript generators
- **Implementation:** "Coroutines in C" (multiple implementations available)
- **Library:** libcoro, libtask, libmill

---

## 3. Implementation Priority Recommendation

### Immediate: IEEE 754-2008 Decimal Floating Point
**Status:** ✅ **COMPLETED**

Decimal floating-point is now fully integrated into PCC with automatic detection and portable library support.

### Short Term: Apple Blocks (6-12 months)

**Rationale:**
- Well-defined specification (Clang BlockLanguageSpec)
- Existing runtime library (libBlocksRuntime)
- Real-world usage in Apple ecosystem
- Moderate complexity
- Useful for callback-heavy code

**Priority:** Medium-High

### Long Term: MetaWare Generators (12-18 months)

**Rationale:**
- No modern specification
- Complex implementation
- Limited current demand
- Historical interest mainly
- Python/JavaScript already popular for this use case

**Priority:** Low

**Alternative:** Consider C++20 coroutines or proposals for C2x generators instead

---

## 4. General Implementation Guidelines

### For All Language Extensions

1. **Maintain Compatibility**
   - Extensions should not break existing code
   - Provide feature detection macros

2. **Documentation**
   - Comprehensive syntax guide
   - Usage examples
   - Migration guide from alternatives

3. **Testing**
   - Parser tests for all syntax
   - Semantic analysis tests
   - Code generation tests
   - Runtime behavior tests
   - Cross-platform tests

4. **Performance**
   - Benchmark vs alternatives
   - Optimize common cases
   - Document performance characteristics

5. **Interoperability**
   - Work with existing C libraries
   - FFI considerations
   - ABI stability

---

## 5. Contributing

Interested in implementing these features? Here's how to get started:

### Prerequisites

- Deep understanding of C language semantics
- Familiarity with compiler design
- Experience with PCC codebase
- Knowledge of assembler and ABI for target platforms

### Resources

- **PCC Architecture:** See `common/README` and `cc/ccom/README`
- **Parser:** `cc/ccom/cgram.y` (Yacc grammar)
- **Lexer:** `cc/ccom/scan.l` (Lex scanner)
- **Code Gen:** `mip/` and `arch/*/` directories
- **Mailing List:** pcc@lists.ludd.ltu.se

### Getting Help

1. Join PCC mailing list
2. Read relevant standards/specifications
3. Study existing implementations (Clang for blocks)
4. Start with proof-of-concept
5. Submit patches incrementally

---

## Conclusion

While IEEE 754-2008 decimal floating-point is now fully integrated into PCC, Apple Blocks and MetaWare Generator Coroutines remain potential future enhancements requiring significant compiler frontend development.

This roadmap provides a foundation for anyone interested in implementing these features, with realistic time estimates and clear technical requirements.

**Current Focus:** Decimal floating-point support is production-ready. Blocks and generators are documented for future development.

---

**Document Version:** 1.0
**Last Updated:** 2025
**Maintainer:** PCC Development Team
**License:** BSD-style (see COPYING file)
