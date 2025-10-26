# PL/I Example Programs

This directory contains example programs demonstrating various PL/I language features and the PCC PL/I compiler capabilities.

## Basic Examples

### hello_simple.pli
The simplest possible PL/I program - just prints "Hello, World!"

```pli
HELLO: PROCEDURE OPTIONS(MAIN);
    PUT SKIP LIST('Hello, World!');
END HELLO;
```

### data_types.pli
Demonstrates all PL/I data types:
- FIXED BINARY (integers)
- FLOAT BINARY (floating-point)
- CHARACTER (strings)
- BIT (bit strings)
- POINTER
- Arrays
- Structures

### control_flow.pli
Shows control structures:
- IF...THEN...ELSE
- DO...END blocks
- DO WHILE loops
- DO variable = start TO end BY increment
- SELECT...WHEN...OTHERWISE
- GOTO and labels

## Advanced Examples

### recursion.pli
Demonstrates recursive procedures with factorial and Fibonacci calculations.

### file_io.pli
File I/O operations:
- OPEN/CLOSE files
- READ/WRITE operations
- STREAM vs RECORD I/O
- Error handling

### structures.pli
Complex data structures:
- Nested structures
- Arrays of structures
- Structure assignment
- Structure members

### exception_handling.pli
ON condition mechanism:
- ZERODIVIDE
- CONVERSION
- ENDFILE
- ERROR
- User-defined handlers

## PL/M Examples

### hardware_io.plm
PL/M-specific features:
- INPUT/OUTPUT port access
- Interrupt handlers
- LITERALLY macros
- AT clause for absolute addressing

### embedded_system.plm
Real embedded system example:
- Device drivers
- Interrupt service routines
- Memory-mapped I/O
- Bit manipulation

## Runtime Library Examples

### math_functions.pli
Mathematical function usage:
- Trigonometric (SIN, COS, TAN)
- Exponential (EXP, LOG)
- Other (SQRT, ABS, etc.)

### string_operations.pli
String manipulation:
- INDEX, LENGTH, SUBSTR
- TRIM, VERIFY
- Concatenation
- VARYING strings

### dynamic_memory.pli
Memory management:
- ALLOCATE/FREE
- Pointers
- ADDR, NULL
- SIZE, ALLOCATION

## Compiling Examples

### Standard PL/I
```bash
pli -o program program.pli
./program
```

### PL/M
```bash
pli -d plm86 -o firmware firmware.plm
```

### With optimization
```bash
pli -O2 -o program program.pli
```

### Generate assembly only
```bash
pli -S -o program.s program.pli
```

## Notes

- All examples are compatible with standard PL/I
- PL/M examples require `-d plm` or `-d plm86` dialect flag
- Some examples require specific hardware for PL/M I/O operations
- Runtime library is automatically linked

## Learning Path

1. Start with `hello_simple.pli`
2. Learn data types with `data_types.pli`
3. Master control flow in `control_flow.pli`
4. Explore procedures in `recursion.pli`
5. Advanced: `file_io.pli`, `exception_handling.pli`
6. For embedded: PL/M examples

## See Also

- PL/I Language Reference
- libpli documentation (../libpli/README.md)
- PL/I tutorial (../docs/tutorial.md)
