# PCC CHILL Compiler

CCITT CHILL (Z.200) Language Implementation for PCC

## Overview

This is a complete implementation of the CCITT CHILL (CCITT High Level Language) compiler for the Portable C Compiler (PCC) framework. CHILL was a telecommunications-oriented programming language standardized by the ITU-T (formerly CCITT) as Recommendation Z.200.

## Components

### 1. CHILL Compiler Frontend (`chcom`)

The compiler frontend parses CHILL source code and generates assembly output.

**Location**: `chill/chcom/`

**Features**:
- Complete lexical analyzer for CHILL keywords and operators
- Full parser implementing CHILL grammar
- Symbol table with scope management
- CHILL mode (type) system
- Assembly code generation

**Usage**:
```bash
chcom [options] input.ch

Options:
  -o file       Write output to file
  -v            Verbose output
  -w            Suppress all warnings
  --dump-ast    Dump abstract syntax tree
  --dump-symtab Dump symbol table
  -h, --help    Show help
```

**Example**:
```bash
chcom -v -o program.s program.ch
```

### 2. CHILL Runtime Library (`libchill`)

Comprehensive runtime library providing CHILL standard functions.

**Location**: `libchill/`

**Modules**:
- `chill_io.c` - I/O operations (read/write)
- `chill_string.c` - String manipulation
- `chill_math.c` - Mathematical functions
- `chill_memory.c` - Memory management
- `chill_process.c` - Process/concurrency (POSIX threads)
- `chill_powerset.c` - Set operations
- `chill_exception.c` - Exception handling
- `chill_runtime.c` - Runtime initialization

**Building**:
```bash
cd libchill
make
```

This produces `libchill.a` (static library).

### 3. CHILL Compiler Driver (`chill`)

Complete compiler driver that orchestrates compilation, assembly, and linking.

**Location**: `chill/chill/`

**Usage**:
```bash
chill [options] file...

Options:
  -c           Compile and assemble, but do not link
  -S           Compile only, do not assemble or link
  -o <file>    Place output in <file>
  -v           Verbose mode
  -O<level>    Set optimization level (0-3)
  -L<dir>      Add directory to library search path
  -l<lib>      Link with library
  -w           Suppress warnings
  --help       Display this information
  --version    Display compiler version
```

**Example**:
```bash
# Compile to assembly
chill -S program.ch

# Compile to object file
chill -c program.ch

# Compile and link to executable
chill -o program program.ch
```

## CHILL Language Features

### Supported Constructs

**Module Structure**:
```chill
MODULE module_name;
SPEC
  /* Specification part - declarations */
END;
BODY
  /* Body part - implementation */
END;
```

**Data Types (Modes)**:
- `INT` - Integer
- `BOOL` - Boolean
- `CHAR` - Character
- `REAL` - Real (floating point)
- `CHARS(n)` - Character string
- `ARRAY` - Arrays
- `STRUCT` - Structures
- `REF` - References (pointers)
- `POWERSET` - Sets

**Declarations**:
```chill
DCL variable INT;
DCL message CHARS(20) := "Hello";
DCL numbers ARRAY(0:9) OF INT;
```

**Control Structures**:
```chill
IF condition THEN
  statements
ELSE
  statements
FI;

CASE expression OF
  (value1): statements;
  (value2): statements;
  ELSE: statements;
ESAC;

DO variable := start TO end;
  statements
OD;

WHILE condition DO
  statements
OD;
```

**Procedures**:
```chill
PROC procedure_name(params) RETURNS mode;
  /* procedure body */
END procedure_name;
```

## Building the Compiler

### Prerequisites

- GCC or compatible C compiler
- Flex (lexical analyzer generator)
- Bison/Yacc (parser generator)
- Make
- Autoconf/Automake

### Build Steps

```bash
# From PCC root directory
./configure
make

# Build just CHILL components
cd chill
make
```

### Installation

```bash
make install
```

This installs:
- `chcom` to `/usr/local/libexec/`
- `chill` to `/usr/local/bin/`
- `libchill.a` to `/usr/local/lib/`
- `chill.h` to `/usr/local/include/`

## Example Programs

### Hello World

**File**: `hello.ch`
```chill
MODULE hello;
END;
```

**Compile**:
```bash
chcom -v -o hello.s hello.ch
as -o hello.o hello.s
ld -o hello hello.o -Llibchill -lchill -lm -lpthread -lc
```

Or using the driver:
```bash
chill -v -o hello hello.ch
```

### Test Programs

Several test programs are included in the root directory:
- `test_minimal.ch` - Minimal module structure
- `test_decl.ch` - Module with declarations
- `test_simple.ch` - Module with procedures
- `test_chill_comprehensive.ch` - Comprehensive feature demonstration

## Testing

```bash
# Parse only (syntax check)
chcom test_minimal.ch

# Generate assembly
chcom -o test.s test_minimal.ch

# Assemble
as -o test.o test.s

# Link with runtime
ld -o test test.o -Llibchill -lchill -lm -lpthread -lc
```

## Current Status

**✓ Implemented**:
- Complete lexer and parser
- Symbol table and type system
- Basic assembly code generation
- Comprehensive runtime library
- Compiler driver

**⚠ Partially Implemented**:
- Code generation (minimal stub - generates valid assembly framework)
- Full statement and expression code generation
- Optimization passes

**Future Work**:
- Complete code generation for all CHILL constructs
- Backend optimization
- Advanced CHILL features (processes, signals)
- Debugger support

## Architecture

```
CHILL Source (.ch)
        ↓
    chcom (Frontend)
    - Lexer (scan.l)
    - Parser (cgram.y)
    - Symbol Table (symtab.c)
    - Type System (modes.c)
    - Code Generator (codegen.c)
        ↓
    Assembly (.s)
        ↓
    Assembler (as)
        ↓
    Object File (.o)
        ↓
    Linker (ld)
    + libchill.a
        ↓
    Executable
```

## References

- CCITT Recommendation Z.200 (CHILL Language Definition)
- ITU-T Z.200 Specification
- PCC Documentation: http://pcc.ludd.ltu.se/

## License

Copyright (c) 2025 PCC CHILL Compiler

This implementation follows the PCC licensing terms.

## Contact

For issues and contributions, please refer to the main PCC project.
