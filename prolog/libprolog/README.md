# PCC Prolog Runtime Library

A comprehensive runtime library for executing Prolog programs compiled by the PCC Prolog compiler.

## Overview

The runtime library implements the core execution engine for Prolog, including:

- **Warren Abstract Machine (WAM) architecture** - Industry-standard execution model
- **Unification algorithm** - With optional occurs check
- **Backtracking** - Choice point management and trail
- **Memory management** - Heap, stack, and trail with automatic reclamation
- **Built-in predicates** - 50+ standard predicates
- **Arithmetic evaluation** - Integer and floating-point expressions
- **I/O system** - Stream-based input/output
- **Exception handling** - ISO-compliant error handling

## Architecture

### Memory Areas

The runtime uses four main memory areas:

1. **Heap** (1M words) - Stores terms, structures, and permanent data
2. **Stack** (256K words) - Environment frames and choice points
3. **Trail** (256K words) - Tracks variable bindings for backtracking
4. **PDL** (64K words) - Push-down list for unification

### Data Structures

#### Terms
Terms are represented as tagged pointers (3 low bits for type):
- `TAG_REF` (0) - Unbound variable reference
- `TAG_STRUCT` (1) - Compound term with functor and arguments
- `TAG_LIST` (2) - List cons cell [H|T]
- `TAG_INT` (3) - Tagged integer (immediate or boxed)
- `TAG_ATOM` (4) - Atom (pointer to atom table entry)
- `TAG_FLOAT` (5) - Boxed floating-point value
- `TAG_VAR` (6) - Named variable
- `TAG_STR` (7) - String object

#### Choice Points
Choice points store state for backtracking:
- Continuation pointer
- Environment pointer
- Heap and trail tops
- Next clause to try
- Saved arguments

#### Environment Frames
Environment frames store local variables for deterministic predicates:
- Continuation (return address)
- Previous environment
- Local variable slots

## API Reference

### Initialization

```c
prolog_engine_t *prolog_init(void);
void prolog_cleanup(prolog_engine_t *eng);
void prolog_reset(prolog_engine_t *eng);
```

### Term Construction

```c
word_t make_var(prolog_engine_t *eng);
word_t make_atom(prolog_engine_t *eng, const char *name);
word_t make_integer(prolog_engine_t *eng, intptr_t value);
word_t make_float(prolog_engine_t *eng, double value);
word_t make_string(prolog_engine_t *eng, const char *str, size_t len);
word_t make_struct(prolog_engine_t *eng, functor_t *f, word_t *args);
word_t make_list(prolog_engine_t *eng, word_t head, word_t tail);
word_t make_nil(prolog_engine_t *eng);
```

### Unification

```c
int unify(prolog_engine_t *eng, word_t t1, word_t t2);
word_t deref(prolog_engine_t *eng, word_t term);
```

### Execution

```c
int prolog_call(prolog_engine_t *eng, word_t goal);
int prolog_query(prolog_engine_t *eng, const char *query);
```

### Memory Management

```c
word_t *heap_alloc(prolog_engine_t *eng, size_t words);
word_t *stack_alloc(prolog_engine_t *eng, size_t words);
void trail(prolog_engine_t *eng, word_t *addr);
void unwind_trail(prolog_engine_t *eng, word_t *trail_mark);
```

### Backtracking

```c
choice_point_t *create_choice_point(prolog_engine_t *eng, int arity);
void restore_choice_point(prolog_engine_t *eng, choice_point_t *cp);
void discard_choice_point(prolog_engine_t *eng);
```

### Built-in Predicates

The runtime provides implementations of core built-ins:

#### Control
- `true/0` - Always succeeds
- `fail/0` - Always fails
- `!/0` - Cut (prune choice points)

#### Unification
- `=/2` - Unification
- `\=/2` - Not unifiable

#### Comparison
- `==/2`, `\==/2` - Structural equality
- `@</2`, `@=</2`, `@>/2`, `@>=/2` - Standard order

#### Type Testing
- `var/1`, `nonvar/1` - Variable testing
- `atom/1`, `number/1`, `integer/1`, `float/1`
- `atomic/1`, `compound/1`, `ground/1`

#### Term Manipulation
- `functor/3` - Extract functor and arity
- `arg/3` - Extract nth argument
- `=../2` - Univ (term ↔ list conversion)

#### Arithmetic
- `is/2` - Arithmetic evaluation
- `=:=/2`, `=\=/2` - Arithmetic equality
- `</2`, `=</2`, `>/2`, `>=/2` - Arithmetic comparison

Arithmetic expressions support:
- Operators: `+`, `-`, `*`, `/`, `//`, `mod`, `rem`, `**`, `^`
- Bitwise: `/\`, `\/`, `\`, `<<`, `>>`, `xor`
- Functions: `abs`, `sign`, `sqrt`, `sin`, `cos`, `tan`, `exp`, `log`
- Rounding: `floor`, `ceiling`, `round`, `truncate`

#### I/O
- `write/1`, `writeln/1` - Write terms
- `nl/0` - Newline

#### System
- `halt/0`, `halt/1` - Exit program

## Usage Example

```c
#include "runtime.h"

int main(void) {
    /* Initialize engine */
    prolog_engine_t *eng = prolog_init();

    /* Create terms */
    word_t x = make_var(eng);
    word_t five = make_integer(eng, 5);

    /* Unify X with 5 */
    if (unify(eng, x, five)) {
        printf("X = ");
        print_term(eng, stdout, x);
        printf("\n");
    }

    /* Create compound term foo(1, 2) */
    word_t args[2];
    args[0] = make_integer(eng, 1);
    args[1] = make_integer(eng, 2);
    functor_t *f = functor_create(eng, "foo", 2);
    word_t term = make_struct(eng, f, args);

    /* Print term */
    print_term(eng, stdout, term);  /* Prints: foo(1, 2) */

    /* Cleanup */
    prolog_cleanup(eng);

    return 0;
}
```

## Building

```bash
make
```

This produces `libprolog.a` which can be linked with compiled Prolog programs.

### Building Test Program

```bash
gcc -o test_runtime test_runtime.c -L. -lprolog -lm
./test_runtime
```

## Features

### Unification Algorithm
- Robinson's unification algorithm with path compression
- Optional occurs check (ISO-compliant)
- Trail-based binding for efficient backtracking
- Push-down list for iterative implementation

### Backtracking
- Choice point stack for non-deterministic predicates
- Automatic trail unwinding
- Deterministic optimization (no choice point for single clause)

### Memory Management
- Mark-and-release for heap management
- Stack-based allocation for choice points and environments
- Trail for backtrackable bindings
- No garbage collection (Prolog programs typically don't need it)

### Performance Optimizations
- Tagged pointers for immediate integers
- Inline small integers (no heap allocation)
- Atom interning for fast comparison
- Direct functor comparison (pointer equality)

## Thread Safety

The current implementation is **not thread-safe**. Each thread should have its own `prolog_engine_t` instance.

## Limitations

- No garbage collection (manual memory management only)
- No tail call optimization (TCO) yet
- Limited I/O capabilities (basic stream operations)
- Exception handling is basic (no full ISO error terms)

## Future Enhancements

- [ ] Garbage collection for long-running programs
- [ ] Tail call optimization
- [ ] JIT compilation for hot predicates
- [ ] Full ISO I/O predicates
- [ ] Constraint solver integration
- [ ] Tabling/memoization support
- [ ] Thread-safe multi-engine support

## License

Copyright (c) 2025 PCC Prolog Runtime Library

See top-level PCC license for details.

## References

- Warren, D.H.D. "An Abstract Prolog Instruction Set" (1983) - WAM design
- Aït-Kaci, H. "Warren's Abstract Machine: A Tutorial Reconstruction" (1991)
- ISO/IEC 13211-1:1995 - Prolog standard
