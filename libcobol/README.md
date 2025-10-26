# COBOL Runtime Library (libcobol)

This is the runtime library for the PCC COBOL compiler, providing essential runtime support for COBOL programs including OO COBOL features and multi-vendor dialect compatibility.

## Features

### Core I/O Operations
- **ACCEPT**: Read from standard input
- **DISPLAY**: Write to standard output with formatting
- Console I/O with COBOL field formatting

### Numeric Operations
- **ADD, SUBTRACT, MULTIPLY, DIVIDE**: Arithmetic operations
- **COMPUTE**: Expression evaluation
- PICTURE clause numeric type conversions
- Support for signed/unsigned, decimal, and edited numerics

### String Operations
- **MOVE**: Copy data between fields with proper formatting
- **STRING**: Concatenate multiple fields
- **UNSTRING**: Split strings by delimiters
- **INSPECT**: Search and replace within strings
- Field comparison with proper COBOL semantics

### File I/O
- Sequential file organization
- Indexed file organization (partial)
- Relative file organization (partial)
- OPEN, CLOSE, READ, WRITE, REWRITE operations
- File status handling

### Packed Decimal Support
- Pack/unpack BCD (Binary Coded Decimal) data
- Arithmetic on packed decimal fields
- COMP-3 (IBM) support

### Object-Oriented Support
- Object allocation and deallocation
- Reference counting
- Method invocation framework
- Class hierarchy support

### Intrinsic Functions
- LENGTH: Get field length
- UPPER-CASE, LOWER-CASE: Case conversion
- REVERSE: Reverse string
- NUMVAL: Convert string to numeric
- CURRENT-DATE: Get current date/time

### Memory Management
- Safe allocation with error handling
- Automatic cleanup support
- Memory leak prevention

### Error Handling
- Exception tracking
- Status code management
- Fatal error handling
- Debug tracing support

## Building

The library is built automatically as part of the PCC build process:

```bash
./configure
make
make install
```

## Usage in COBOL Programs

The runtime library is automatically linked when compiling COBOL programs:

```bash
cobol myprogram.cob -o myprogram
```

To explicitly link against libcobol:

```bash
cobol -c myprogram.cob -o myprogram.o
cc myprogram.o -lcobol -o myprogram
```

## API Reference

### Field Definition

```c
#include <cobol.h>

cobol_field_t my_field = {
    .data = buffer,
    .size = 80,
    .type = COB_TYPE_ALPHANUMERIC,
    .digits = 0,
    .scale = 0,
    .sign = 0
};
```

### I/O Operations

```c
__cobol_accept(&my_field);      // Read from stdin
__cobol_display(&my_field);     // Write to stdout
```

### Numeric Operations

```c
__cobol_add(&result, &op1, &op2);
__cobol_subtract(&result, &op1, &op2);
__cobol_multiply(&result, &op1, &op2);
__cobol_divide(&result, &op1, &op2);
```

### String Operations

```c
__cobol_move(&dest, &src);
__cobol_string(&dest, sources, count);
__cobol_unstring(&src, dests, count, delimiter);
```

### File I/O

```c
cobol_file_t *file = __cobol_file_open("data.txt",
                                       COB_OPEN_INPUT,
                                       COB_ORG_SEQUENTIAL,
                                       COB_ACCESS_SEQUENTIAL);
__cobol_file_read(file, &record);
__cobol_file_close(file);
```

## Dialect Support

The runtime library supports vendor-specific features:

- **IBM**: COMP-3 packed decimal, SQLIMS extensions
- **DEC**: VAX-specific numeric formats
- **HP**: HP-UX compatibility
- **Microsoft**: Windows-specific features

## Thread Safety

The current implementation is **not thread-safe**. Each COBOL program runs in a single thread. Multi-threaded support may be added in future versions.

## Standards Compliance

This runtime library implements:
- COBOL-85 standard features
- COBOL-2002 OO extensions
- COBOL-2014 features (partial)
- Vendor-specific extensions

## Error Codes

File operation status codes:
- `0`: Success
- `10`: End of file
- `22`: Duplicate key
- `23`: Record not found
- `41`: File already open
- `42`: File not open

## Performance Considerations

- Numeric operations use double-precision floating point
- String operations minimize memory allocation
- File I/O uses buffered streams
- Object allocation uses reference counting

## Limitations

- Indexed file support is incomplete
- Method dispatch uses simple lookup (no vtable optimization yet)
- Screen section not implemented
- Report writer not implemented

## Future Enhancements

- Complete indexed file implementation
- B-tree indexing for ISAM files
- Full screen I/O support
- Report writer functionality
- Multi-threading support
- Unicode support

## License

Same as PCC - BSD-style license

## Contributing

Contributions are welcome! Please ensure:
- Code follows existing style
- All functions have error checking
- Memory leaks are prevented
- Tests are provided for new features
