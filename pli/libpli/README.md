# libpli - PL/I Runtime Library

Runtime support library for PL/I programs compiled with the PCC PL/I compiler.

## Overview

`libpli.a` provides runtime support for:
- I/O operations (GET, PUT, READ, WRITE, OPEN, CLOSE)
- Mathematical functions (trigonometric, exponential, logarithmic)
- String manipulation (INDEX, SUBSTR, LENGTH, TRIM, etc.)
- Memory management (ALLOCATE, FREE, ADDR, NULL)
- Array operations (DIM, HBOUND, LBOUND, SUM, PROD)
- Type conversions (BINARY, CHAR, DECIMAL, FIXED, FLOAT)
- Condition handling (ON, SIGNAL, REVERT)
- PL/M-specific functions (INPUT, OUTPUT, SHL, SHR, etc.)
- Date/time functions

## Files

- **pli_runtime.h** - Main header file with all function declarations
- **pli_io.c** - I/O operations implementation
- **pli_math.c** - Mathematical functions
- **pli_string.c** - String manipulation
- **pli_memory.c** - Memory management
- **pli_array.c** - Array operations
- **pli_convert.c** - Type conversions
- **pli_startup.c** - Runtime initialization and shutdown
- **pli_plm.c** - PL/M-specific functions
- **pli_conditions.c** - Exception/condition handling

## Data Types

```c
typedef int32_t pli_fixed_t;           // FIXED BINARY(31)
typedef int64_t pli_fixed_long_t;      // FIXED BINARY(63)
typedef float pli_float_t;             // FLOAT BINARY(24)
typedef double pli_float_long_t;       // FLOAT BINARY(53)
typedef uint8_t pli_bit_t;             // BIT
typedef char pli_char_t;               // CHARACTER
```

## I/O Functions

### Stream I/O (PUT/GET)

```c
void pli_put_skip(void);               // PUT SKIP
void pli_put_page(void);               // PUT PAGE
void pli_put_string(const char *s);    // PUT LIST(string)
void pli_put_fixed(pli_fixed_t n);     // PUT LIST(integer)
void pli_put_float(pli_float_long_t f); // PUT LIST(float)
void pli_put_char(char c);             // PUT EDIT(char)

void pli_get_skip(void);               // GET SKIP
void pli_get_string(char *buf, size_t maxlen);   // GET LIST(string)
pli_fixed_t pli_get_fixed(void);       // GET LIST(integer)
pli_float_long_t pli_get_float(void);  // GET LIST(float)
char pli_get_char(void);               // GET EDIT(char)
```

### File Operations

```c
pli_file_t *pli_file_open(const char *filename, int mode, int type);
void pli_file_close(pli_file_t *f);
void pli_file_read(pli_file_t *f, void *buf, size_t len);
void pli_file_write(pli_file_t *f, const void *buf, size_t len);
```

## Mathematical Functions

```c
// Arithmetic
pli_fixed_t pli_abs_fixed(pli_fixed_t x);
pli_float_long_t pli_abs_float(pli_float_long_t x);
pli_fixed_t pli_ceil(pli_float_long_t x);
pli_fixed_t pli_floor(pli_float_long_t x);
pli_fixed_t pli_max_fixed(pli_fixed_t a, pli_fixed_t b);
pli_fixed_t pli_min_fixed(pli_fixed_t a, pli_fixed_t b);
pli_fixed_t pli_mod(pli_fixed_t a, pli_fixed_t b);
pli_fixed_t pli_round(pli_float_long_t x);
pli_fixed_t pli_sign(pli_fixed_t x);
pli_fixed_t pli_trunc(pli_float_long_t x);

// Trigonometric
pli_float_long_t pli_sin(pli_float_long_t x);
pli_float_long_t pli_cos(pli_float_long_t x);
pli_float_long_t pli_tan(pli_float_long_t x);
pli_float_long_t pli_asin(pli_float_long_t x);
pli_float_long_t pli_acos(pli_float_long_t x);
pli_float_long_t pli_atan(pli_float_long_t x);
pli_float_long_t pli_sinh(pli_float_long_t x);
pli_float_long_t pli_cosh(pli_float_long_t x);
pli_float_long_t pli_tanh(pli_float_long_t x);

// Exponential/Logarithmic
pli_float_long_t pli_exp(pli_float_long_t x);
pli_float_long_t pli_log(pli_float_long_t x);
pli_float_long_t pli_log10(pli_float_long_t x);
pli_float_long_t pli_log2(pli_float_long_t x);
pli_float_long_t pli_sqrt(pli_float_long_t x);
```

## String Functions

```c
pli_fixed_t pli_index(const char *haystack, const char *needle);
pli_fixed_t pli_length(const char *s);
void pli_substr(char *dest, const char *src, pli_fixed_t start, pli_fixed_t len);
void pli_repeat(char *dest, const char *s, pli_fixed_t count);
void pli_trim(char *dest, const char *src);
pli_fixed_t pli_verify(const char *s, const char *set);
void pli_concat(char *dest, const char *s1, const char *s2);
```

### VARYING String Support

```c
pli_string_t *pli_string_create(size_t max_len);
void pli_string_destroy(pli_string_t *s);
void pli_string_assign(pli_string_t *dest, const char *src);
void pli_string_concat(pli_string_t *dest, const char *s1, const char *s2);
```

## Memory Management

```c
void *pli_allocate(size_t size);       // ALLOCATE
void pli_free(void *ptr);              // FREE
void *pli_addr(void *var);             // ADDR(var)
void *pli_null(void);                  // NULL()
size_t pli_size(void *ptr);            // SIZE(ptr)
pli_fixed_t pli_allocation(void *ptr); // ALLOCATION(ptr)
```

## Array Functions

```c
pli_fixed_t pli_dim(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_hbound(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_lbound(pli_array_t *arr, pli_fixed_t dimension);
pli_fixed_t pli_sum(pli_array_t *arr);
pli_fixed_t pli_prod(pli_array_t *arr);

pli_array_t *pli_array_create(int32_t ndims, int32_t *lower, int32_t *upper, size_t elem_size);
void pli_array_destroy(pli_array_t *arr);
```

## Type Conversion

```c
pli_fixed_t pli_binary(const char *s);         // BINARY(string)
void pli_bit(char *dest, pli_fixed_t value, size_t len);  // BIT(value, len)
void pli_char(char *dest, pli_fixed_t value);  // CHAR(value)
pli_fixed_t pli_decimal(const char *s);        // DECIMAL(string)
pli_fixed_t pli_fixed(pli_float_long_t x);     // FIXED(float)
pli_float_long_t pli_float(pli_fixed_t x);     // FLOAT(fixed)
void pli_unspec(pli_bit_t *dest, const void *src, size_t len);  // UNSPEC
```

## Condition Handling

```c
void pli_on(const char *condition, void (*handler)(void));  // ON condition
void pli_signal(const char *condition);                     // SIGNAL
void pli_revert(const char *condition);                     // REVERT

// Condition query functions
pli_fixed_t pli_oncode(void);          // ONCODE()
void pli_onchar(char *dest);           // ONCHAR()
void pli_onsource(char *dest);         // ONSOURCE()
void pli_onfile(char *dest);           // ONFILE()
void pli_onkey(char *dest);            // ONKEY()
```

### Standard Conditions

- **ERROR** - General error condition
- **ENDFILE** - End of file reached
- **CONVERSION** - Data conversion error
- **FIXEDOVERFLOW** - Integer overflow
- **ZERODIVIDE** - Division by zero

## PL/M-Specific Functions

```c
// I/O Port Access (x86/x86-64 only)
uint8_t pli_input(uint16_t port);      // INPUT(port)
void pli_output(uint16_t port, uint8_t value);  // OUTPUT(port) = value

// Bit Manipulation
uint8_t pli_shl(uint8_t value, uint8_t count);  // SHL(value, count)
uint8_t pli_shr(uint8_t value, uint8_t count);  // SHR(value, count)
uint8_t pli_rol(uint8_t value, uint8_t count);  // ROL(value, count)
uint8_t pli_ror(uint8_t value, uint8_t count);  // ROR(value, count)

// Byte/Word Operations
uint8_t pli_high_byte(uint16_t value); // HIGH(value)
uint8_t pli_low_byte(uint16_t value);  // LOW(value)
uint16_t pli_double(uint8_t low, uint8_t high);  // DOUBLE(low, high)

// Memory Operations
void pli_move(void *dest, const void *src, size_t count);  // MOVE
```

## Startup/Shutdown

```c
void pli_init(void);    // Initialize runtime (called automatically)
void pli_finish(void);  // Cleanup runtime (called automatically)
void pli_stop(void);    // STOP statement
void pli_exit(int code); // EXIT with code
```

## Date/Time Functions

```c
void pli_date(char *dest);      // DATE() - Returns "YYYY-MM-DD"
void pli_time(char *dest);      // TIME() - Returns "HH:MM:SS"
void pli_datetime(char *dest);  // DATETIME() - Returns "YYYY-MM-DD HH:MM:SS"
```

## Usage

Link with `-lpli` when compiling PL/I programs:

```bash
pli -o myprogram myprogram.pli -lpli
```

Or manually:
```bash
plicom myprogram.pli -o myprogram.s
as myprogram.s -o myprogram.o
ld myprogram.o -lpli -lc -o myprogram
```

## Building

```bash
cd pli/libpli
make
```

This creates `libpli.a` which is installed to `$(libdir)`.

## Platform Notes

### I/O Port Functions (PL/M)

The `pli_input()` and `pli_output()` functions use inline assembly for direct hardware port access:

- **x86/x86-64**: Implemented using `inb`/`outb` instructions
- **Other architectures**: Stub implementations (return 0 / do nothing)

For embedded systems or custom hardware, modify `pli_plm.c` to provide platform-specific implementations.

### Threading

The runtime library is not thread-safe. For multi-threaded programs, synchronization must be handled by the application.

## Standards Compliance

- Compatible with IBM PL/I built-in functions
- Supports Intel PL/M-86/386 built-ins
- PL/I uses 1-based array indexing (PL/M uses 0-based)
- File I/O follows PL/I stream/record semantics

## See Also

- PL/I Language Reference
- Intel PL/M-86 User's Guide
- pli(1) - PL/I compiler driver
- plicom(1) - PL/I compiler proper
