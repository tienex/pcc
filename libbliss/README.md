# BLISS Runtime Library (libbliss)

The BLISS Runtime Library provides essential runtime support for BLISS programs compiled with the PCC BLISS compiler.

## Overview

BLISS (Basic Language for Implementation of System Software) was designed as a systems programming language without many built-in operations. This runtime library fills that gap by providing:

- **I/O operations** - Character and string input/output
- **String manipulation** - String creation, comparison, concatenation
- **Memory management** - Vector allocation, dynamic memory
- **Bit operations** - Field extraction/insertion, bit counting
- **Signal handling** - BLISS-style exception/condition handling
- **Debugging support** - Runtime tracing and data dumping

## Building

The library is built as part of the PCC build process:

```bash
# From the PCC root directory
./configure
make

# Or build just libbliss
cd libbliss
make
```

This creates `libbliss.a` which can be linked with BLISS programs.

## Usage

### Compiling and Linking

```bash
# Compile BLISS program with runtime library
bliss myprogram.bli -lbliss

# Or manually link
bliss -c myprogram.bli -o myprogram.o
ld myprogram.o -lbliss -lc -o myprogram
```

### Including the Header

```c
#include <blissrt.h>
```

## API Reference

### Data Types

#### `bliss_word_t`
Default BLISS word type (signed integer, pointer-sized).

#### `bliss_uword_t`
Unsigned word type (pointer-sized).

#### `bliss_vector_t`
Vector (array) descriptor:
```c
typedef struct {
    bliss_word_t *data;     // Pointer to data
    size_t size;            // Number of elements
    size_t element_size;    // Size of each element
} bliss_vector_t;
```

#### `bliss_string_t`
Counted string descriptor:
```c
typedef struct {
    char *data;      // Pointer to string data
    size_t length;   // Length of string
} bliss_string_t;
```

#### `bliss_signal_t`
Signal condition codes:
- `BLISS_SIG_SUCCESS` - Success
- `BLISS_SIG_ERROR` - Error
- `BLISS_SIG_WARNING` - Warning
- `BLISS_SIG_INFO` - Information

### Memory Management

#### Vector Allocation
```c
bliss_vector_t *bliss_alloc_vector(size_t size, size_t element_size);
void bliss_free_vector(bliss_vector_t *vec);
```

Example:
```c
// Allocate vector of 100 words
bliss_vector_t *vec = bliss_alloc_vector(100, sizeof(bliss_word_t));
// Use vector...
bliss_free_vector(vec);
```

#### Dynamic Memory
```c
void *bliss_malloc(size_t size);
void bliss_free(void *ptr);
void *bliss_realloc(void *ptr, size_t size);
```

### String Operations

#### String Creation and Conversion
```c
bliss_string_t bliss_string_from_cstr(const char *str);
char *bliss_string_to_cstr(bliss_string_t str);
```

Example:
```c
bliss_string_t s = bliss_string_from_cstr("Hello, BLISS!");
char *cstr = bliss_string_to_cstr(s);
printf("%s\n", cstr);
bliss_free(cstr);
bliss_string_free(s);
```

#### String Manipulation
```c
int bliss_string_compare(bliss_string_t s1, bliss_string_t s2);
bliss_string_t bliss_string_concat(bliss_string_t s1, bliss_string_t s2);
bliss_string_t bliss_string_substr(bliss_string_t str, size_t start, size_t length);
void bliss_string_free(bliss_string_t str);
```

Example:
```c
bliss_string_t s1 = bliss_string_from_cstr("Hello");
bliss_string_t s2 = bliss_string_from_cstr(" World");
bliss_string_t s3 = bliss_string_concat(s1, s2);
// s3 now contains "Hello World"
bliss_string_free(s1);
bliss_string_free(s2);
bliss_string_free(s3);
```

### I/O Operations

#### Character I/O
```c
void bliss_putchar(int ch);
int bliss_getchar(void);
```

#### String Output
```c
void bliss_puts(const char *str);
void bliss_put_string(bliss_string_t str);
void bliss_putcrlf(void);
```

#### Numeric Output
```c
void bliss_put_decimal(bliss_word_t value);  // Decimal
void bliss_put_hex(bliss_word_t value);      // Hexadecimal
void bliss_put_octal(bliss_word_t value);    // Octal
```

Example:
```c
bliss_puts("The answer is: ");
bliss_put_decimal(42);
bliss_putcrlf();
// Output: "The answer is: 42\n"
```

#### Line Input
```c
int bliss_getline(char *buffer, size_t size);
```

### Bit and Field Operations

#### Field Extraction and Insertion
```c
bliss_word_t bliss_field_extract(bliss_word_t value, int position, int size);
bliss_word_t bliss_field_insert(bliss_word_t dest, bliss_word_t field,
                                int position, int size);
```

Example:
```c
// Extract bits 4-7 (4 bits starting at position 4)
bliss_word_t value = 0x12345678;
bliss_word_t field = bliss_field_extract(value, 4, 4);  // 0x7

// Insert field at position 8
value = bliss_field_insert(value, 0xA, 8, 4);  // 0x12340A78
```

#### Bit Counting
```c
int bliss_count_leading_zeros(bliss_uword_t value);
int bliss_count_trailing_zeros(bliss_uword_t value);
int bliss_popcount(bliss_uword_t value);
```

Example:
```c
bliss_uword_t val = 0x00FF0000;
int lz = bliss_count_leading_zeros(val);   // 8
int tz = bliss_count_trailing_zeros(val);  // 16
int pc = bliss_popcount(val);              // 8
```

### Signal Handling

#### Signal Handler Type
```c
typedef int (*bliss_signal_handler_t)(bliss_signal_t condition, void *arg);
```

#### Signal Functions
```c
void bliss_signal(bliss_signal_t condition, void *arg);
void bliss_set_signal_handler(bliss_signal_handler_t handler);
bliss_signal_handler_t bliss_get_signal_handler(void);
```

Example:
```c
int my_handler(bliss_signal_t condition, void *arg) {
    if (condition == BLISS_SIG_ERROR) {
        printf("Error occurred!\n");
        return 0;  // Abort
    }
    return 1;  // Continue
}

// Install handler
bliss_set_signal_handler(my_handler);

// Signal a condition
bliss_signal(BLISS_SIG_WARNING, NULL);
```

### Runtime Initialization

```c
void bliss_runtime_init(void);
void bliss_runtime_cleanup(void);
const char *bliss_runtime_version(void);
```

Example:
```c
int main(void) {
    bliss_runtime_init();

    // Program code...

    bliss_runtime_cleanup();
    return 0;
}
```

### Debugging Support

```c
void bliss_dump_vector(bliss_vector_t *vec);
void bliss_dump_string(bliss_string_t str);
void bliss_set_debug(int enable);
```

Example:
```c
bliss_set_debug(1);  // Enable debug tracing

bliss_vector_t *vec = bliss_alloc_vector(10, sizeof(bliss_word_t));
bliss_dump_vector(vec);  // Prints vector contents

bliss_string_t s = bliss_string_from_cstr("Debug test");
bliss_dump_string(s);  // Prints string info
```

### Error Handling

```c
void bliss_abort(const char *msg) __attribute__((noreturn));
```

Example:
```c
if (error_condition) {
    bliss_abort("Fatal error: invalid operation");
}
```

## Complete Example Program

```c
#include <blissrt.h>

int main(void) {
    bliss_runtime_init();

    // String operations
    bliss_string_t greeting = bliss_string_from_cstr("Hello, BLISS!");
    bliss_put_string(greeting);
    bliss_putcrlf();

    // Numeric output
    bliss_puts("Answer: ");
    bliss_put_decimal(42);
    bliss_putcrlf();

    // Vector allocation
    bliss_vector_t *vec = bliss_alloc_vector(10, sizeof(bliss_word_t));
    vec->data[0] = 100;
    vec->data[1] = 200;

    bliss_puts("Vector[0] = ");
    bliss_put_decimal(vec->data[0]);
    bliss_putcrlf();

    // Cleanup
    bliss_free_vector(vec);
    bliss_string_free(greeting);

    bliss_runtime_cleanup();
    return 0;
}
```

Compile and run:
```bash
bliss example.c -lbliss -o example
./example
```

Output:
```
Hello, BLISS!
Answer: 42
Vector[0] = 100
```

## Implementation Details

### Memory Management
- All allocations use standard C `malloc`/`free`
- Vectors are heap-allocated with separate descriptor
- Strings are heap-allocated with null terminator

### Thread Safety
- Current implementation is **not thread-safe**
- Global signal handler is shared across all threads
- For multi-threaded programs, external synchronization is required

### Platform Support
- Portable C99 implementation
- Works on all platforms supported by PCC
- Uses compiler intrinsics for bit operations when available (GCC/Clang)

### Performance
- Field operations use efficient bitwise operations
- String operations minimize copying where possible
- Vector allocation uses `calloc` for zero-initialization

## Version History

### Version 1.0.0 (2025)
- Initial release
- Core I/O operations
- String manipulation
- Memory management
- Bit/field operations
- Signal handling
- Debugging support

## License

Copyright (c) 2025 PCC BLISS Runtime Library
Part of the Portable C Compiler project.

## See Also

- [BLISS Compiler Documentation](../bliss/README.md)
- [PCC Documentation](../README.md)
- Original BLISS language manuals (DEC)
