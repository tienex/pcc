# Common LISP Runtime Library

This is the runtime support library for the PCC Common LISP compiler.

## Overview

The `liblisp` library provides essential runtime functions for compiled LISP programs, including:

- **Memory management**: Object allocation and garbage collection
- **List operations**: `car`, `cdr`, `cons`, `append`, `reverse`, etc.
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`, `abs`, `max`, `min`
- **Comparison**: `=`, `/=`, `<`, `>`, `<=`, `>=`, `eq`, `eql`
- **Logical operations**: `and`, `or`, `not`
- **Type predicates**: `null`, `atom`, `consp`, `listp`, `numberp`, etc.
- **I/O functions**: `print`, `princ`, `read`, `write`
- **String operations**: Case conversion, comparison, concatenation

## Data Structures

### LISP Object Representation

All LISP values are represented by the `lisp_object_t` structure:

```c
typedef struct lisp_object {
    lisp_type_t type;
    union {
        long integer;
        double floating;
        char *string;
        char *symbol;
        struct {
            struct lisp_object *car;
            struct lisp_object *cdr;
        } cons;
        void *function;
    } value;
} lisp_object_t;
```

### Object Types

- `LISP_TYPE_NIL`: Empty list / false value
- `LISP_TYPE_T`: True value
- `LISP_TYPE_INTEGER`: Long integer
- `LISP_TYPE_FLOAT`: Double-precision float
- `LISP_TYPE_STRING`: C string
- `LISP_TYPE_SYMBOL`: Symbol/identifier
- `LISP_TYPE_CONS`: Cons cell (pair)
- `LISP_TYPE_FUNCTION`: Function pointer

## API Reference

### Memory Management

```c
lisp_object_t *lisp_alloc(void);
void lisp_free(lisp_object_t *obj);
void lisp_gc_init(void);
void lisp_gc_collect(void);
```

### Constructors

```c
lisp_object_t *lisp_make_nil(void);
lisp_object_t *lisp_make_t(void);
lisp_object_t *lisp_make_integer(long value);
lisp_object_t *lisp_make_float(double value);
lisp_object_t *lisp_make_string(const char *str);
lisp_object_t *lisp_make_symbol(const char *name);
lisp_object_t *lisp_make_cons(lisp_object_t *car, lisp_object_t *cdr);
```

### List Operations

```c
lisp_object_t *lisp_car(lisp_object_t *obj);
lisp_object_t *lisp_cdr(lisp_object_t *obj);
lisp_object_t *lisp_cons(lisp_object_t *car, lisp_object_t *cdr);
lisp_object_t *lisp_list(int n, ...);
lisp_object_t *lisp_append(lisp_object_t *list1, lisp_object_t *list2);
lisp_object_t *lisp_reverse(lisp_object_t *list);
lisp_object_t *lisp_length(lisp_object_t *list);
lisp_object_t *lisp_nth(lisp_object_t *n, lisp_object_t *list);
```

### Arithmetic Operations

```c
lisp_object_t *lisp_add(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_subtract(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_multiply(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_divide(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_mod(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_abs(lisp_object_t *a);
lisp_object_t *lisp_max(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_min(lisp_object_t *a, lisp_object_t *b);
```

### Comparison Operations

```c
lisp_object_t *lisp_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_not_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_less_than(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_greater_than(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_less_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_greater_equal(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_eq(lisp_object_t *a, lisp_object_t *b);
lisp_object_t *lisp_eql(lisp_object_t *a, lisp_object_t *b);
```

### Type Predicates

```c
lisp_object_t *lisp_null(lisp_object_t *obj);
lisp_object_t *lisp_atom(lisp_object_t *obj);
lisp_object_t *lisp_consp(lisp_object_t *obj);
lisp_object_t *lisp_listp(lisp_object_t *obj);
lisp_object_t *lisp_numberp(lisp_object_t *obj);
lisp_object_t *lisp_integerp(lisp_object_t *obj);
lisp_object_t *lisp_floatp(lisp_object_t *obj);
lisp_object_t *lisp_symbolp(lisp_object_t *obj);
lisp_object_t *lisp_stringp(lisp_object_t *obj);
```

### I/O Functions

```c
void lisp_print(lisp_object_t *obj);
void lisp_princ(lisp_object_t *obj);
void lisp_prin1(lisp_object_t *obj);
void lisp_write(lisp_object_t *obj);
lisp_object_t *lisp_read(void);
lisp_object_t *lisp_read_line(void);
```

### Utility Functions

```c
void lisp_error(const char *fmt, ...);
int lisp_is_true(lisp_object_t *obj);
int lisp_is_nil(lisp_object_t *obj);
void lisp_runtime_init(void);
void lisp_runtime_cleanup(void);
```

## Memory Model

The current implementation uses a simple bump allocator with a fixed-size object pool:

- **Pool Size**: 10,000 objects
- **No Garbage Collection**: Objects are never freed (simple implementation)
- **Singleton Objects**: `NIL` and `T` are pre-allocated singletons

### Future Improvements

- Mark-and-sweep garbage collection
- Generational GC
- Compacting GC
- Dynamic pool expansion
- Reference counting

## Building

```bash
make
make install
```

This will build `liblisp.a` and install it to the library directory along with the header file.

## Linking

To link a LISP program with the runtime library:

```bash
lisp myprogram.lisp -o myprogram
# The driver automatically links with -llisp
```

Or manually:
```bash
lcom myprogram.lisp -o myprogram.s
as myprogram.s -o myprogram.o
ld myprogram.o -llisp -lc -o myprogram
```

## Example Usage

```c
#include <liblisp.h>

int main(void)
{
    /* Initialize runtime */
    lisp_runtime_init();

    /* Create some objects */
    lisp_object_t *num1 = lisp_make_integer(42);
    lisp_object_t *num2 = lisp_make_integer(10);

    /* Perform arithmetic */
    lisp_object_t *sum = lisp_add(num1, num2);

    /* Print result */
    lisp_print(sum);  // Outputs: 52

    /* Create a list */
    lisp_object_t *list = lisp_list(3, num1, num2, sum);
    lisp_print(list);  // Outputs: (42 10 52)

    /* Cleanup */
    lisp_runtime_cleanup();

    return 0;
}
```

## Implementation Notes

### Object Lifetime

- Objects allocated with `lisp_make_*` functions live until the program exits
- No explicit deallocation is required (or supported) in this version
- Future versions will implement proper garbage collection

### Type Safety

- Functions perform runtime type checking
- Type errors are reported via `lisp_error()`
- Invalid operations return `NIL` after reporting an error

### Performance

- Simple bump allocator is very fast for allocation
- No GC overhead during program execution
- Memory usage grows monotonically (no reclamation)

## Testing

Test programs can be found in `../lisp/tests/`:

```bash
cd ../lisp/tests
../lisp/lisp hello.lisp
../lisp/lisp factorial.lisp
../lisp/lisp list-ops.lisp
```

## License

Copyright (c) 2025 PCC Common LISP Runtime Library

Part of the Portable C Compiler project.
