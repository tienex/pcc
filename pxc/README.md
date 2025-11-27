# PXC - Xbase++ Compiler

PXC is an Xbase++ compiler built on the PCC (Portable C Compiler) architecture.

## Overview

Xbase++ is a programming language that is part of the xBase family of languages, which includes dBase, Clipper, FoxPro, and Harbour. PXC aims to provide a modern, portable compiler for Xbase++ programs.

## Features

- **Xbase++ Language Support**: Implements core Xbase++ language features including:
  - Dynamic typing with support for Character, Numeric, Date, Logical, Array, Object types
  - Functions and procedures
  - Control structures (IF/ENDIF, DO WHILE/ENDDO, FOR/NEXT, DO CASE/ENDCASE)
  - Object-oriented programming (CLASS/ENDCLASS, METHOD/ENDMETHOD)
  - Database operations (USE, SELECT, SKIP, SEEK, etc.)
  - Code blocks `{|x| x + 1}`
  - Array operations
  - 100+ built-in functions

- **Multiple Target Architectures**: Leverages PCC's backend to support multiple architectures:
  - x86 (32-bit and 64-bit)
  - ARM
  - And many more via PCC backends

- **Portable**: Generates portable assembly code or can emit C code for maximum portability

## Directory Structure

```
pxc/
├── pxc/             # Driver program (orchestrates compilation)
│   └── pxc.c
├── pxccom/          # Compiler proper (frontend)
│   ├── pass1.h      # Frontend data structures
│   ├── scan.l       # Lexical analyzer (Flex)
│   ├── xgram.y      # Parser grammar (Yacc)
│   ├── main.c       # Compiler entry point
│   ├── symtab.c     # Symbol table management
│   ├── types.c      # Type system
│   ├── tree.c       # AST operations
│   └── builtins.c   # Built-in functions
├── tests/           # Test programs
└── README.md        # This file
```

## Building

PXC is built as part of the PCC build system:

```bash
# From the pcc directory
./configure
make
```

Or build just the Xbase++ compiler:

```bash
cd pxc
make
```

## Usage

Basic usage:

```bash
# Compile a single Xbase++ source file
pxc program.prg -o program

# Compile to assembly only
pxc -S program.prg

# Compile to object file only
pxc -c program.prg

# Verbose output
pxc -v program.prg

# Generate C code instead of assembly
pxc --emit-c program.prg
```

Options:
- `-c`: Compile to object files, do not link
- `-S`: Compile to assembly, do not assemble
- `-o <file>`: Specify output file
- `-O`: Enable optimizations
- `-v`: Verbose output
- `-g`: Generate debug information
- `-L<dir>`: Add library search path
- `-l<lib>`: Link with library
- `--emit-c`: Generate C code instead of assembly

## Example Programs

### Hello World

```xbase
FUNCTION Main()
   ? "Hello, World!"
   RETURN NIL
```

### Variables and Control Flow

```xbase
FUNCTION Calculate(n)
   LOCAL i, sum := 0

   FOR i := 1 TO n
      sum += i
   NEXT

   RETURN sum
```

### Objects and Classes

```xbase
CLASS Person
   DATA name
   DATA age

   METHOD New(cName, nAge)
      ::name := cName
      ::age := nAge
      RETURN Self
   ENDMETHOD

   METHOD GetInfo()
      RETURN "Name: " + ::name + ", Age: " + STR(::age)
   ENDMETHOD
ENDCLASS

FUNCTION Main()
   LOCAL p := Person():New("John", 30)
   ? p:GetInfo()
   RETURN NIL
```

### Arrays

```xbase
FUNCTION ArrayExample()
   LOCAL arr := {1, 2, 3, 4, 5}
   LOCAL i

   FOR i := 1 TO LEN(arr)
      ? arr[i]
   NEXT

   AADD(arr, 6)
   ASORT(arr)

   RETURN NIL
```

## Language Features

### Data Types

- **Character**: String values
- **Numeric**: Integer and floating-point numbers
- **Date**: Date values
- **Logical**: Boolean values (.T. or .F.)
- **Array**: Dynamic arrays
- **Object**: Object instances
- **CodeBlock**: Executable code blocks
- **NIL**: Null value

### Built-in Functions

PXC supports over 100 built-in functions including:

**String Functions**: LEN, SUBSTR, UPPER, LOWER, TRIM, AT, STRTRAN, etc.

**Numeric Functions**: ABS, INT, ROUND, SQRT, EXP, LOG, SIN, COS, MIN, MAX, etc.

**Date Functions**: DATE, YEAR, MONTH, DAY, CTOD, DTOC, DTOS, etc.

**Array Functions**: ALEN, AADD, ASORT, ASCAN, AEVAL, etc.

**Type Functions**: VALTYPE, ISNIL, ISNUMBER, ISCHARACTER, etc.

**Database Functions**: RECNO, EOF, BOF, DBSEEK, DBSKIP, etc.

**I/O Functions**: FOPEN, FCREATE, FREAD, FWRITE, FILE, etc.

## Implementation Status

Current implementation provides:
- ✅ Lexical analysis (tokenization)
- ✅ Syntax analysis (parsing)
- ✅ Symbol table management
- ✅ Type system
- ✅ AST construction
- ✅ Built-in function definitions
- ⏳ Semantic analysis (in progress)
- ⏳ Code generation (in progress)
- ⏳ Runtime library (planned)

## Contributing

PXC is part of the PCC project. Contributions are welcome!

## License

Copyright (c) 2025 PCC Xbase++ Compiler

## See Also

- [PCC Documentation](../../README.md)
- [Xbase++ Language Reference](https://docs.alaska-software.com/)
- [Harbour Project](https://harbour.github.io/)
