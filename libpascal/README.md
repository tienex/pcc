# libpascal - Pascal Runtime Library for PCC

A comprehensive runtime support library for Pascal programs compiled with the Portable C Compiler (PCC).

## Overview

libpascal provides essential runtime support for Pascal programs, including:

- **I/O Operations**: Text and binary file I/O with Pascal semantics
- **String Handling**: Both short strings (Turbo Pascal) and long strings (Delphi)
- **Mathematical Functions**: Standard Pascal mathematical operations
- **Memory Management**: Dynamic allocation with Pascal semantics
- **Set Operations**: Complete support for Pascal set types
- **Runtime Error Handling**: Comprehensive error detection and reporting

## Features

### I/O Support

#### Text File Operations
- Standard I/O: `read`, `readln`, `write`, `writeln`
- File management: `assign`, `reset`, `rewrite`, `close`, `flush`
- File status: `eof`, `eoln`
- Both standard I/O and file-specific operations

#### Binary File Operations
- Binary file I/O with typed elements
- File positioning: `seek`, `filepos`, `filesize`
- Element-based reading and writing

#### File Management
- File operations: `erase`, `rename`

### String Operations

#### Short Strings (Turbo Pascal/Delphi style)
- Maximum 255 characters
- Length-prefixed format
- Operations: `length`, `concat`, `copy`, `pos`
- Case conversion: `upcase`, `lowercase`
- Editing: `insert`, `delete`, `trim`
- Comparison: case-sensitive and case-insensitive

#### Long Strings (Delphi style)
- Dynamic allocation
- No length limit (memory-dependent)
- Operations: `setlength`, `concat`, `assign`
- Automatic memory management

### Mathematical Functions

#### Standard Functions
- Absolute value: `abs` (integer and real)
- Square: `sqr` (integer and real)
- Square root: `sqrt`
- Trigonometric: `sin`, `cos`, `arctan`
- Exponential/logarithmic: `exp`, `ln`
- Rounding: `round`, `trunc`

#### Ordinal Operations
- `ord`, `chr`, `succ`, `pred`
- `odd` (test for odd numbers)

#### Type Conversions
- Integer to string: `int_to_str`
- Real to string: `real_to_str`
- String to integer: `str_to_int`
- String to real: `str_to_real`

#### Utility Functions (Turbo/Delphi extensions)
- Increment/decrement: `inc`, `dec`
- Byte manipulation: `hi`, `lo`, `swap`

### Memory Management

#### Dynamic Allocation
- `new`: Allocate and initialize memory
- `dispose`: Free allocated memory
- `getmem`: Raw memory allocation
- `freemem`: Raw memory deallocation

#### Memory Operations
- `fillchar`: Fill memory block with a value
- `move`: Copy memory blocks (handles overlapping)

### Set Operations

Complete support for Pascal set types (0..255):
- Initialization: `set_init`
- Manipulation: `set_add`, `set_remove`, `set_contains`
- Set algebra: `set_union`, `set_intersection`, `set_difference`
- Comparison: `set_equal`, `set_subset`

### Runtime Error Handling

#### Error Codes
- Range check errors
- Stack/heap overflow
- Integer overflow
- Division by zero
- File errors (not found, not open, disk full)
- Pointer errors (nil, invalid)
- Read/write errors

#### Error Handling Features
- Descriptive error messages
- Signal handlers for hardware exceptions
- Customizable error handler
- Automatic cleanup on program termination

## Usage

### Building

The library is built as part of the PCC build system:

```bash
./configure
make
make install
```

### Linking

When compiling Pascal programs, link with libpascal:

```bash
pascal myprogram.pas -lpascal
```

Or using the C compiler directly:

```bash
pcc myprogram.c -lpascal -lm
```

Note: The `-lm` flag is required for mathematical functions.

### Example Pascal Program

```pascal
program HelloWorld;

var
  f: text;
  name: string[50];
  count: integer;

begin
  { Standard I/O }
  writeln('Enter your name:');
  readln(name);
  writeln('Hello, ', name, '!');

  { File I/O }
  assign(f, 'output.txt');
  rewrite(f);
  writeln(f, 'This is a test');
  close(f);

  { Mathematical operations }
  count := round(sqrt(16.0));
  writeln('Square root of 16 = ', count);
end.
```

## API Reference

### File Types

- `PascalString`: Short string (max 255 chars)
- `PascalLongString`: Dynamic long string
- `PascalTextFile`: Text file handle
- `PascalFile`: Binary file handle
- `PascalSet`: Set type (0..255)

### Standard I/O Functions

```c
void pascal_write_integer(int value);
void pascal_write_real(double value);
void pascal_write_char(char value);
void pascal_write_string(const char *str);
void pascal_writeln(void);

void pascal_read_integer(int *value);
void pascal_read_real(double *value);
void pascal_read_char(char *value);
void pascal_read_string(char *str, size_t maxlen);
void pascal_readln(void);
```

### File Operations

```c
void pascal_assign(PascalTextFile *f, const char *filename);
void pascal_reset_text(PascalTextFile *f);
void pascal_rewrite_text(PascalTextFile *f);
void pascal_close_text(PascalTextFile *f);
int pascal_eof_text(PascalTextFile *f);
int pascal_eoln_text(PascalTextFile *f);
```

### String Operations

```c
void pascal_str_assign(PascalString *dest, const char *src);
void pascal_str_concat(PascalString *dest, const PascalString *s1, const PascalString *s2);
int pascal_str_pos(const PascalString *substr, const PascalString *str);
void pascal_str_upcase(PascalString *str);
```

### Mathematical Functions

```c
double pascal_sqrt(double x);
double pascal_sin(double x);
double pascal_cos(double x);
int pascal_round(double x);
int pascal_trunc(double x);
```

### Memory Management

```c
void *pascal_new(size_t size);
void pascal_dispose(void *ptr);
void pascal_fillchar(void *dest, size_t count, uint8_t value);
void pascal_move(const void *src, void *dest, size_t count);
```

## Implementation Details

### String Format

Short strings use a length-prefixed format:
```
[0]: length (0-255)
[1..256]: character data
```

Long strings use a structure with:
- Dynamic allocation
- Separate length and capacity fields
- Null-terminated data

### Error Handling

The library uses a consistent error handling approach:
1. Errors are reported via `pascal_runtime_error()`
2. Default handler prints error and exits
3. Custom handlers can be installed
4. Signal handlers catch hardware exceptions

### Thread Safety

Note: The current implementation is not thread-safe. Multi-threaded Pascal programs should use external synchronization.

## Compatibility

### Dialect Support

The runtime library is designed to support multiple Pascal dialects:
- ISO 7185 Standard Pascal
- ISO 10206 Extended Pascal
- Turbo Pascal / Borland Pascal
- Delphi Object Pascal
- Free Pascal

### Platform Support

libpascal is portable and works on:
- Unix/Linux systems
- BSD variants
- macOS
- Windows (with appropriate C runtime)

## Testing

Test programs are located in `../pascal/tests/`:
- `hello_iso.pas`: ISO Pascal test
- `hello_borland.pas`: Borland Pascal test
- `hello_delphi.pas`: Delphi test

## Contributing

Contributions are welcome! Areas for improvement:
- Additional string operations
- Unicode support
- Thread-safe variants
- Performance optimizations
- Extended I/O formats

## License

Part of the Portable C Compiler (PCC) project.
See the main PCC LICENSE file for details.

## See Also

- PCC Pascal Compiler: `../pascal/README.md`
- PCC Main Documentation: `../README.md`
- ISO 7185:1990 - Programming languages — Pascal
- Turbo Pascal 7.0 documentation
- Delphi Language Guide

## Authors

Part of the PCC Pascal compiler project.
