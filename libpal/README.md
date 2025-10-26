# PAL Runtime Library (libpal)

Comprehensive runtime support library for Paradox Application Language (PAL) and ObjectPAL programs compiled with the PCC PAL compiler frontend.

## Overview

The PAL Runtime Library provides all built-in functions and runtime features needed to execute PAL/ObjectPAL programs, including:

- String manipulation (20+ functions)
- Math and numeric operations (15+ functions)
- Date and time handling (15+ functions)
- Array operations (dynamic arrays with insert/delete)
- Type conversions
- UI functions (message boxes, user interaction)
- System functions (file operations, environment)
- Database operation stubs
- Exception handling support
- Memory management

## Features

### String Functions

- `pal_string_new()` - Create new string
- `pal_upper()`, `pal_lower()` - Case conversion
- `pal_substr()` - Substring extraction (1-based indexing)
- `pal_trim()`, `pal_ltrim()`, `pal_rtrim()` - Whitespace trimming
- `pal_strpos()` - Find substring position
- `pal_concat()` - String concatenation
- `pal_format()` - Number formatting
- `pal_fill()` - Create filled string

### Math Functions

- Basic: `pal_abs()`, `pal_sqrt()`, `pal_power()`
- Trigonometric: `pal_sin()`, `pal_cos()`, `pal_tan()`
- Logarithmic: `pal_ln()`, `pal_log()`, `pal_exp()`
- Rounding: `pal_round()`, `pal_trunc()`, `pal_int()`, `pal_frac()`
- Utilities: `pal_max()`, `pal_min()`, `pal_mod()`, `pal_random()`

### Date/Time Functions

- Current: `pal_today()`, `pal_now()`
- Creation: `pal_date()`, `pal_time()`, `pal_datetime()`
- Extraction: `pal_year()`, `pal_month()`, `pal_day()`, `pal_hour()`, etc.
- Formatting: `pal_datetostr()`, `pal_strtodate()`
- Arithmetic: `pal_adddays()`, `pal_datediff()`

### Array Functions

- `pal_array_new()` - Create dynamic array
- `pal_arrayget()`, `pal_arrayset()` - Element access
- `pal_arrayinsert()`, `pal_arraydelete()` - Dynamic modification
- `pal_arrayresize()` - Resize array
- `pal_arraysize()` - Get array length

### UI Functions

Cross-platform message boxes and user interaction:

- `pal_msginfo()`, `pal_msgwarning()`, `pal_msgerror()` - Display messages
- `pal_msgquestion()`, `pal_msgyesno()` - User prompts
- `pal_beep()`, `pal_sleep()`, `pal_wait()` - User interaction

Platform support:
- **Windows**: Native MessageBox API
- **Unix/Linux**: Terminal-based dialog boxes

### System Functions

- `pal_execute()`, `pal_shell()` - Execute commands
- `pal_getenv()`, `pal_setenv()` - Environment variables
- `pal_fileexists()` - Check file existence
- `pal_filecopy()`, `pal_filedelete()`, `pal_filerename()` - File operations

### Type Conversions

- `pal_numval()` - String to number
- `pal_strval()` - Number to string
- `pal_logical()` - Convert to boolean
- `pal_toint()`, `pal_tonum()` - Numeric conversions

## Data Types

The library provides PAL-compatible data types:

```c
typedef int8_t   PAL_ShortInt;
typedef int16_t  PAL_SmallInt;
typedef int32_t  PAL_LongInt;
typedef double   PAL_Number;      /* BCD emulated as double */
typedef double   PAL_Currency;
typedef int8_t   PAL_Logical;
typedef struct   PAL_String;      /* Pascal-style string */
typedef struct   PAL_Date;        /* Year, month, day */
typedef struct   PAL_Time;        /* Hour, minute, second */
typedef struct   PAL_DateTime;    /* Combined date/time */
typedef struct   PAL_Array;       /* Dynamic array */
typedef struct   PAL_Variant;     /* Can hold any type */
```

## Building

The library is built as part of the PCC build process:

```bash
cd libpal
make
```

To build and run tests:

```bash
make tests
make check
```

## Installation

Install the library and headers:

```bash
make install
```

This installs:
- `libpal.a` to `$(libdir)`
- `palrt.h` to `$(includedir)/pal/`

## Usage

### In PAL Programs

The runtime library is automatically linked when compiling PAL programs:

```bash
palcom -o program.o myprogram.pal
```

### In C Programs

You can also use the PAL runtime from C:

```c
#include <pal/palrt.h>

int main(void)
{
    PAL_String *s;

    pal_runtime_init();

    s = pal_string_new("Hello, PAL!");
    pal_msginfo("Message", pal_string_cstr(s));
    pal_string_free(s);

    pal_runtime_cleanup();
    return 0;
}
```

Compile with:

```bash
gcc -o program program.c -lpal -lm
```

## Testing

The library includes comprehensive test suites:

### String Tests (`test_string`)
- String creation and manipulation
- Case conversion
- Substring operations
- Trimming and searching
- Concatenation

### Math Tests (`test_math`)
- Basic arithmetic operations
- Trigonometric functions
- Rounding and truncation
- Logarithms and exponentials

### Date/Time Tests (`test_datetime`)
- Date/time creation
- Component extraction
- Date arithmetic
- Formatting and parsing

### Array Tests (`test_array`)
- Array creation and access
- Element insertion and deletion
- Dynamic resizing

Run all tests:

```bash
make check
```

Expected output:
```
Running PAL runtime library tests...
Running tests/test_string...
All string tests PASSED!

Running tests/test_math...
All math tests PASSED!

Running tests/test_datetime...
All date/time tests PASSED!

Running tests/test_array...
All array tests PASSED!

All tests passed!
```

## Architecture

### String Implementation

PAL strings use a Pascal-style structure with length prefix:

```c
typedef struct {
    uint16_t length;     /* Current length */
    uint16_t capacity;   /* Allocated capacity */
    char *data;          /* Null-terminated C string */
} PAL_String;
```

This allows efficient length queries and maintains compatibility with C string functions.

### Array Implementation

Dynamic arrays with automatic resizing:

```c
typedef struct {
    void *data;          /* Element storage */
    size_t elem_size;    /* Size of each element */
    int32_t length;      /* Current length */
    int32_t capacity;    /* Allocated capacity */
} PAL_Array;
```

Arrays grow automatically on insertion (doubling strategy for O(1) amortized insertion).

### Memory Management

The library provides wrappers around standard C memory functions:

- `pal_malloc()` - Allocate memory
- `pal_realloc()` - Reallocate memory
- `pal_free()` - Free memory

All string and array structures manage their own memory and must be freed with their respective `_free()` functions.

## Database Support

Database functions are currently implemented as stubs that log operations:

- `pal_moveto()` - Navigate to record
- `pal_locate()` - Search for record
- `pal_lockrecord()` - Lock record for editing
- `pal_unlockrecord()` - Unlock record
- `pal_post()` - Save changes
- `pal_resync()` - Refresh table

Future versions may integrate with actual database engines (SQLite, Paradox files, ODBC).

## Exception Handling

The library provides basic exception support:

```c
void pal_set_exception(int code, const char *message,
                       const char *file, int line);
PAL_Exception *pal_get_exception(void);
void pal_clear_exception(void);
```

This supports PAL's try-except blocks and error handling.

## Platform Support

The library is designed to be cross-platform:

- **Windows**: Native Win32 API for UI (MessageBox, Beep)
- **Unix/Linux**: Terminal-based UI, POSIX file operations
- **macOS**: POSIX-compliant (same as Linux)

Platform-specific code is isolated using `#ifdef` directives.

## Performance Considerations

- **Strings**: Immutable design (operations return new strings)
- **Arrays**: Amortized O(1) insertion at end
- **Math**: Thin wrappers around standard C math library
- **Date/Time**: Uses standard C `time.h` functions

For performance-critical code, consider:
- Reusing string buffers when possible
- Pre-allocating arrays with known size
- Using direct array access for tight loops

## Future Enhancements

Planned improvements:

1. **Database Integration**
   - SQLite backend
   - Native Paradox file support
   - ODBC connectivity

2. **Extended String Support**
   - Unicode/UTF-8 support
   - Regular expressions
   - Advanced formatting

3. **Improved UI**
   - GTK+ dialogs for Linux
   - macOS native dialogs
   - Custom dialog builder

4. **Threading Support**
   - Thread-safe string operations
   - Concurrent array access
   - Thread-local exception handling

## License

Copyright (c) 2025 PCC Project

This library is part of the Portable C Compiler (PCC) project.

## See Also

- [PAL Compiler Frontend](../paradox/README.md)
- [PAL Language Reference](../paradox/tests/README.md)
- PCC Documentation
