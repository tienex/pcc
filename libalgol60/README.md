# ALGOL 60+ Runtime Library (libalgol60)

This is the runtime library for ALGOL 60+ programs compiled with the PCC ALGOL 60+ compiler.

## Overview

The ALGOL 60+ runtime library provides essential functionality for ALGOL programs:

- **I/O Operations**: Read and write integers, reals, booleans, and strings
- **Mathematical Functions**: Standard ALGOL 60 transfer functions (sin, cos, sqrt, ln, exp, etc.)
- **String Operations**: String creation, concatenation, substring, comparison
- **Array Management**: Multi-dimensional array allocation and element access
- **Memory Management**: Allocation tracking and error checking
- **Runtime Support**: Initialization, cleanup, and error handling

## Building

The library is built automatically as part of the PCC build process:

```bash
./configure
make
make install
```

This will create `libalgol60.a` and install it to the system library directory.

## Usage

When compiling ALGOL 60+ programs, link against this library:

```bash
algol -o program program.alg
# or manually:
acom -o program.s program.alg
as -o program.o program.s
ld -o program program.o -lalgol60 -lm
```

The ALGOL 60+ driver (`algol`) automatically links with `libalgol60.a` and the math library (`-lm`).

## API Reference

### I/O Functions

#### Output Functions
```c
void algol_write_integer(algol_integer value);
void algol_write_real(algol_real value);
void algol_write_boolean(algol_boolean value);
void algol_write_string(const char *str);
void algol_write_newline(void);
```

#### Input Functions
```c
algol_integer algol_read_integer(void);
algol_real algol_read_real(void);
algol_boolean algol_read_boolean(void);
algol_string algol_read_string(void);
```

#### Formatted I/O (Extensions)
```c
void algol_writef(const char *format, ...);  /* printf-like */
int algol_readf(const char *format, ...);    /* scanf-like */
```

### Mathematical Functions

All standard ALGOL 60 transfer functions are supported:

```c
/* Absolute value */
algol_integer algol_abs_integer(algol_integer x);
algol_real algol_abs_real(algol_real x);

/* Sign function: -1, 0, or 1 */
algol_integer algol_sign_integer(algol_integer x);
algol_integer algol_sign_real(algol_real x);

/* Square root */
algol_real algol_sqrt(algol_real x);

/* Trigonometric */
algol_real algol_sin(algol_real x);
algol_real algol_cos(algol_real x);
algol_real algol_arctan(algol_real x);
algol_real algol_tan(algol_real x);

/* Exponential and logarithm */
algol_real algol_exp(algol_real x);
algol_real algol_ln(algol_real x);
algol_real algol_log10(algol_real x);

/* Entier (floor function) */
algol_integer algol_entier(algol_real x);

/* Power */
algol_real algol_power(algol_real base, algol_real exponent);
algol_integer algol_power_int(algol_integer base, algol_integer exponent);
```

### String Operations

```c
/* String creation and destruction */
algol_string algol_string_create(const char *str);
void algol_string_free(algol_string str);

/* String operations */
size_t algol_string_length(algol_string str);
algol_string algol_string_concat(algol_string s1, algol_string s2);
algol_string algol_string_substring(algol_string str, int start, int length);
int algol_string_compare(algol_string s1, algol_string s2);
```

### Array Operations

```c
/* Array creation and destruction */
algol_array_t *algol_array_create(int ndims, int *lower_bounds,
                                   int *upper_bounds, size_t elem_size);
void algol_array_free(algol_array_t *array);

/* Array element access */
void *algol_array_element(algol_array_t *array, int *indices);

/* Bounds checking */
int algol_array_bounds_check(algol_array_t *array, int dim, int index);
```

### Memory Management

```c
void *algol_malloc(size_t size);
void *algol_calloc(size_t nmemb, size_t size);
void *algol_realloc(void *ptr, size_t size);
void algol_free(void *ptr);

/* Memory statistics */
size_t algol_memory_allocated(void);
size_t algol_memory_peak(void);
```

### Runtime Initialization

```c
void algol_init(int argc, char **argv);
void algol_fini(void);
```

**Note**: The compiler automatically generates calls to `algol_init()` and `algol_fini()` in the program's main function.

### Error Handling

```c
/* Error codes */
#define ALGOL_ERR_NONE           0
#define ALGOL_ERR_BOUNDS         1  /* Array bounds error */
#define ALGOL_ERR_DOMAIN         2  /* Math domain error */
#define ALGOL_ERR_RANGE          3  /* Math range error */
#define ALGOL_ERR_DIV_ZERO       4  /* Division by zero */
#define ALGOL_ERR_NULL_PTR       5  /* Null pointer */
#define ALGOL_ERR_MEMORY         6  /* Memory allocation failure */
#define ALGOL_ERR_IO             7  /* I/O error */
#define ALGOL_ERR_TYPE           8  /* Type error */

/* Error reporting */
void algol_error(int code, const char *message, ...);
void algol_fatal_error(int code, const char *message, ...);

/* Custom exception handlers */
typedef void (*algol_exception_handler_t)(int code, const char *message);
void algol_set_exception_handler(algol_exception_handler_t handler);
```

### Type Conversions

```c
algol_real algol_int_to_real(algol_integer i);
algol_integer algol_real_to_int(algol_real r);
algol_integer algol_bool_to_int(algol_boolean b);
algol_boolean algol_int_to_bool(algol_integer i);
```

### Utility Functions (Extensions)

```c
/* Random numbers */
algol_real algol_random(void);  /* 0.0 to 1.0 */
algol_integer algol_random_int(algol_integer min, algol_integer max);

/* Time */
algol_real algol_time(void);  /* Seconds since epoch */
```

## Data Types

```c
typedef int32_t algol_integer;      /* ALGOL integer */
typedef double algol_real;          /* ALGOL real */
typedef int32_t algol_boolean;      /* ALGOL boolean */
typedef char *algol_string;         /* ALGOL string */

#define ALGOL_TRUE  1
#define ALGOL_FALSE 0
```

## Example Usage

### Simple I/O
```c
#include <algol60.h>

int main(int argc, char **argv)
{
    algol_integer n;

    algol_init(argc, argv);

    algol_write_string("Enter a number: ");
    n = algol_read_integer();

    algol_write_string("You entered: ");
    algol_write_integer(n);
    algol_write_newline();

    algol_fini();
    return 0;
}
```

### Using Arrays
```c
#include <algol60.h>

int main(int argc, char **argv)
{
    algol_array_t *arr;
    int lower[1] = {1};
    int upper[1] = {10};
    int indices[1];
    algol_integer *elem;

    algol_init(argc, argv);

    /* Create array[1:10] of integers */
    arr = algol_array_create(1, lower, upper, sizeof(algol_integer));

    /* Fill array */
    for (int i = 1; i <= 10; i++) {
        indices[0] = i;
        elem = (algol_integer *)algol_array_element(arr, indices);
        *elem = i * i;
    }

    /* Print array */
    for (int i = 1; i <= 10; i++) {
        indices[0] = i;
        elem = (algol_integer *)algol_array_element(arr, indices);
        algol_write_integer(*elem);
        algol_write_newline();
    }

    algol_array_free(arr);
    algol_fini();
    return 0;
}
```

### Using Math Functions
```c
#include <algol60.h>

int main(int argc, char **argv)
{
    algol_real x = 2.0;
    algol_real result;

    algol_init(argc, argv);

    result = algol_sqrt(x);
    algol_write_string("sqrt(2.0) = ");
    algol_write_real(result);
    algol_write_newline();

    result = algol_sin(3.14159 / 2.0);
    algol_write_string("sin(π/2) = ");
    algol_write_real(result);
    algol_write_newline();

    algol_fini();
    return 0;
}
```

## Implementation Notes

### Array Storage
- Arrays are stored in row-major order
- Multi-dimensional arrays support arbitrary lower and upper bounds
- Automatic bounds checking on element access

### Error Handling
- By default, errors print to stderr and the program continues
- Fatal errors call the exception handler (which exits by default)
- Custom exception handlers can be installed for specialized error handling

### Memory Management
- All allocations are tracked for debugging
- Memory statistics available via `algol_memory_allocated()` and `algol_memory_peak()`

### Floating Point
- ALGOL `real` type maps to C `double` (64-bit IEEE 754)
- Standard math library functions are used for transcendental functions
- Domain and range errors are detected and reported

## Files

- **algol60.h** - Main header file (public API)
- **runtime.c** - Runtime initialization and error handling
- **io.c** - I/O operations
- **math.c** - Mathematical functions
- **string.c** - String operations
- **array.c** - Array and memory management
- **Makefile.in** - Build configuration

## Dependencies

- Standard C library (`libc`)
- Math library (`libm`) for transcendental functions

## License

Part of the Portable C Compiler (PCC) project.
Copyright (c) 2025

## See Also

- ALGOL 60+ Compiler documentation: `../algol60/README.md`
- ALGOL 60 Revised Report (1963)
- PCC documentation
