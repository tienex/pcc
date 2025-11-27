# libocaml - OCaml Runtime Library

OCaml runtime library for PCC (Portable C Compiler).

## Overview

This library provides runtime support for OCaml programs compiled with PCC's OCaml compiler (ocom). It includes:

- **Memory Management**: Simple mark-and-sweep garbage collector
- **Data Structures**: Lists, arrays, strings, and references
- **I/O Operations**: Print and read functions for basic types
- **Exception Handling**: Try-catch mechanism using setjmp/longjmp
- **Type Conversions**: Between integers, floats, characters, and strings
- **Math Functions**: sqrt, sin, cos, exp, log

## Value Representation

OCaml values use tagged representation:
- **Integers**: Lowest bit = 1, value shifted left by 1
- **Pointers**: Lowest bit = 0, points to heap-allocated blocks

This allows distinguishing between immediate integers and heap-allocated objects without dereferencing.

## Memory Layout

Heap blocks have a header followed by data:
```
+----------------+
| Header         | (size, tag, color)
+----------------+
| Field 0        |
+----------------+
| Field 1        |
+----------------+
| ...            |
+----------------+
```

## Garbage Collection

The GC uses a simple mark-and-sweep algorithm:
1. **Mark Phase**: Starting from registered roots, mark all reachable values
2. **Sweep Phase**: Free all unmarked blocks

### Registering Roots

Global variables and local variables that should survive GC must be registered:
```c
ocaml_value_t my_value;
ocaml_gc_register_root(&my_value);
```

## Data Structures

### Lists

Lists are implemented as cons cells:
- Empty list: `VAL_NIL` (integer 0)
- Cons cell: 2-field block with head and tail

```c
ocaml_value_t list = ocaml_cons(VAL_INT(1),
                     ocaml_cons(VAL_INT(2), VAL_NIL));
```

### Arrays

Arrays are variable-length blocks tagged with `TAG_ARRAY`:
```c
ocaml_value_t arr = ocaml_array_make(VAL_INT(10), VAL_INT(0));
ocaml_array_set(arr, VAL_INT(0), VAL_INT(42));
```

### Strings

Strings are byte arrays tagged with `TAG_STRING`:
```c
ocaml_value_t str = ocaml_string_make("Hello, World!");
ocaml_print_string(str);
```

### References

Mutable references are single-field blocks:
```c
ocaml_value_t ref = ocaml_ref(VAL_INT(42));
ocaml_assign(ref, VAL_INT(100));
ocaml_value_t val = ocaml_deref(ref);
```

## I/O Functions

```c
ocaml_print_int(VAL_INT(42));
ocaml_print_string(ocaml_string_make("Hello"));
ocaml_print_newline();

ocaml_value_t n = ocaml_read_int();
ocaml_value_t line = ocaml_read_line();
```

## Exception Handling

```c
ocaml_value_t compute(void *arg) {
    // May raise exception
    if (error_condition)
        ocaml_raise(exception_value);
    return result;
}

ocaml_value_t handle_exception(ocaml_exception_t *exn) {
    // Handle the exception
    return default_value;
}

ocaml_value_t result = ocaml_try(compute, arg, handle_exception);
```

## Building

The library is built as part of the PCC build system:
```bash
./configure
make
make install
```

This will install:
- `libocaml.a` to `$(libdir)`
- `ocaml_runtime.h` to `$(includedir)`

## Usage

Link OCaml programs with `-locaml`:
```bash
ocaml -o myprogram myprogram.ml -locaml
```

## Initialization

Programs should initialize the GC at startup:
```c
int main(int argc, char **argv) {
    ocaml_gc_init(16 * 1024 * 1024);  /* 16MB heap */

    // Your OCaml program

    return 0;
}
```

## Implementation Notes

- The GC is conservative and may retain some garbage
- String operations allocate new strings (immutable)
- Array bounds are checked at runtime
- Exception handlers use a global stack (not thread-safe)

## Future Improvements

- Generational GC for better performance
- Thread-safe runtime for concurrent OCaml
- Better type inference integration
- Native code generation optimizations
- Profiling and debugging support
