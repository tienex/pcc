# PCC PL/I Compiler

A portable PL/I compiler supporting multiple PL dialects, built on the PCC (Portable C Compiler) architecture.

## Overview

This is a full-featured PL/I compiler that supports:
- **PL/I** - Standard IBM PL/I language
- **PL/M** - Intel's PL/M for microprocessors (8080, 8086, 80286, 80386)
- **PL/C** - Teaching dialect with relaxed rules
- **PL/1** - Alternative name for PL/I
- Multiple PL/I variants (Subset G, Optimizing PL/I)

## Architecture

The compiler follows the classic PCC two-pass architecture:

```
Source (.pli, .plm) → [plicom] → Assembly (.s) → [as] → Object (.o) → [ld] → Executable
```

### Components

- **pli** - Driver program (frontend)
  - Manages compilation pipeline
  - Handles file types and linking
  - Similar to `cc` for C

- **plicom** - PL/I compiler proper (backend)
  - Lexical analysis (scan.l)
  - Syntax analysis (pligram.y)
  - Semantic analysis
  - Intermediate code generation

### Directory Structure

```
pli/
├── pli/              # Driver program
│   └── pli.c         # Main driver implementation
├── plicom/           # Compiler proper
│   ├── scan.l        # Lexical analyzer (Flex)
│   ├── pligram.y     # Parser grammar (Bison/Yacc)
│   ├── pass1.h       # Frontend data structures
│   ├── error.h       # Error reporting interface
│   ├── dialect.h     # Dialect configuration
│   ├── main.c        # Compiler entry point
│   ├── error.c       # Clang-style diagnostics
│   ├── dialect.c     # Dialect support
│   ├── symtab.c      # Symbol table management
│   ├── types.c       # Type system
│   └── builtins.c    # Built-in functions
└── tests/            # Test programs
    ├── hello.pli
    ├── factorial.pli
    ├── fibonacci.pli
    ├── array_demo.pli
    └── plm_blink.plm
```

## Language Support

### Data Types

**PL/I Standard Types:**
- `FIXED BINARY` / `FIXED DECIMAL` - Integer types with precision
- `FLOAT BINARY` / `FLOAT DECIMAL` - Floating-point types
- `BIT(n)` - Bit strings
- `CHARACTER(n)` - Character strings
- `POINTER` - Pointer types
- `OFFSET` - Offset types
- `AREA` - Area types
- `FILE` - File types
- `LABEL` - Label types
- `ENTRY` - Entry point types

**PL/M Types:**
- `BYTE` - 8-bit unsigned
- `WORD` - 16-bit unsigned
- `DWORD` - 32-bit unsigned (PL/M-386)
- `ADDRESS` - Pointer type
- `INTEGER` - Signed integer
- `REAL` - Floating point

### Storage Classes

- `AUTOMATIC` - Automatic (local) storage
- `STATIC` - Static storage
- `BASED` - Based variables
- `CONTROLLED` - Controlled storage
- `DEFINED` - Defined variables

### Attributes

- `ALIGNED` / `UNALIGNED` - Alignment control
- `INITIAL(value)` - Initialization
- `EXTERNAL` / `INTERNAL` - Linkage
- `RECURSIVE` - Recursive procedures
- `REENTRANT` - Reentrant procedures
- `RETURNS(type)` - Function return type

### Control Structures

- `DO ... END` - Block statements
- `DO var = start TO end BY inc` - Counted loops
- `DO WHILE(condition)` - While loops
- `DO UNTIL(condition)` - Until loops (PL/I)
- `DO CASE expr` - Case statement (PL/M)
- `IF ... THEN ... ELSE` - Conditional
- `SELECT ... WHEN ... OTHERWISE` - Multi-way branch
- `GOTO` - Unconditional jump
- `CALL` - Procedure call
- `RETURN` - Return from procedure
- `LEAVE` - Exit loop
- `ITERATE` - Continue loop

### I/O Statements

- `GET` / `PUT` - Stream I/O
- `READ` / `WRITE` - Record I/O
- `OPEN` / `CLOSE` - File management

### Built-in Functions

**Arithmetic:**
- ABS, CEIL, FLOOR, MAX, MIN, MOD, ROUND, SIGN, SQRT

**Trigonometric:**
- SIN, COS, TAN, ASIN, ACOS, ATAN, SINH, COSH, TANH

**String:**
- INDEX, LENGTH, SUBSTR, TRIM, VERIFY

**Array:**
- DIM, HBOUND, LBOUND, SUM, PROD

**Storage:**
- ADDR, NULL, SIZE, ALLOCATION

**Conversion:**
- BINARY, CHAR, DECIMAL, FIXED, FLOAT, UNSPEC

**PL/M:**
- SHL, SHR, ROL, ROR, HIGH, LOW, DOUBLE, LAST

## Usage

### Basic Compilation

```bash
# Compile and link
pli hello.pli -o hello

# Compile only (generate object file)
pli -c program.pli

# Generate assembly only
pli -S program.pli

# Specify dialect
pli -d plm86 embedded.plm -o firmware
```

### Dialect Selection

Use the `-d` option to select a dialect:

```bash
pli -d pli standard.pli       # Standard PL/I (default)
pli -d plm microcode.plm      # Generic PL/M
pli -d plm86 driver.plm       # PL/M-86
pli -d plm386 system.plm      # PL/M-386
pli -d plc teaching.pli       # PL/C (relaxed rules)
pli -d subset portable.pli    # PL/I Subset G
```

### Compiler Options

```
-c              Compile to object (.o) but don't link
-S              Compile to assembly (.s) only
-o <file>       Specify output file
-d <dialect>    Set language dialect
-v              Verbose mode
-l<library>     Link with library
-L<path>        Add library search path
-O<level>       Optimization level
-h              Show help
```

### Compiler Proper Options (plicom)

```
-o <file>       Set output file
-d <dialect>    Set dialect
-W<warning>     Enable specific warning
-w              Disable all warnings
-v              Verbose mode
-h              Show help
```

## Examples

### Hello World (PL/I)

```pli
HELLO: PROCEDURE OPTIONS(MAIN);
    PUT SKIP LIST('Hello, World!');
END HELLO;
```

### Recursive Factorial

```pli
FACTORIAL: PROCEDURE(N) RECURSIVE RETURNS(FIXED BINARY(31));
    DECLARE N FIXED BINARY(31);
    IF N <= 1 THEN
        RETURN(1);
    ELSE
        RETURN(N * FACTORIAL(N - 1));
END FACTORIAL;
```

### PL/M Hardware Access

```plm
DEVICE_INIT: PROCEDURE;
    DECLARE PORT_A LITERALLY '0FFH';
    DECLARE STATUS BYTE;

    OUTPUT(PORT_A) = 0;
    STATUS = INPUT(PORT_A + 1);
END DEVICE_INIT;
```

## Dialect Features

### Standard PL/I
- Full language support
- Based variables
- Controlled storage
- Multitasking (TASK, EVENT)
- Exception handling (ON conditions)
- Preprocessor (%DECLARE, %INCLUDE)
- Picture specifications
- File I/O

### PL/M
- Simplified syntax
- Hardware I/O (INPUT/OUTPUT)
- LITERALLY macros
- AT clause for absolute addressing
- INTERRUPT procedures
- PUBLIC/EXTERNAL linkage
- BYTE/WORD/DWORD types
- Limited to embedded systems

### PL/C
- Teaching-oriented
- Relaxed declaration rules
- Implicit declarations allowed
- Enhanced error messages
- Based on PL/I subset

## Building

The PL/I compiler is built as part of the PCC build system:

```bash
# Build everything including PL/I
make all-full

# Or build just PL/I
cd pli && make
```

## Error Reporting

The compiler uses Clang-style diagnostics with source context:

```
example.pli:5:12: error: undeclared identifier 'foo'
    CALL foo();
         ^
```

Features:
- Color output (on TTY)
- Source line context
- Column indicators
- Multiple severity levels (note, warning, error, fatal)

## Compatibility

### Target Architectures
All architectures supported by PCC:
- x86 (i386, i86)
- x86-64 (amd64)
- ARM
- MIPS
- PowerPC
- SPARC
- VAX
- M68K
- PDP-11, PDP-10
- And more...

### Operating Systems
- Linux
- FreeBSD, OpenBSD, NetBSD
- macOS
- Windows (via MinGW/Cygwin)
- Other Unix-like systems

## References

- IBM PL/I Language Reference
- Intel PL/M-86 User's Guide
- Intel PL/M-386 Programmer's Guide
- PL/I ANSI Standard (X3.74)
- PCC Architecture Documentation

## License

Copyright (c) 2025 PCC PL/I Compiler
Part of the Portable C Compiler (PCC) project.

## See Also

- `cc` - C compiler driver
- `pascal` - Pascal compiler
- `f77` - Fortran 77 compiler
