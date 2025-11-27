# BLISS Compiler for PCC

This directory contains the BLISS (Basic Language for Implementation of System Software) compiler implementation for the Portable C Compiler (PCC) infrastructure.

## Overview

BLISS is a system programming language developed at Carnegie Mellon University in the early 1970s. It was designed for writing operating systems and system software, particularly for the DEC PDP-10 and VAX systems. BLISS was notable for being one of the first languages to use expressions as the fundamental building block, rather than statements.

## Directory Structure

```
bliss/
├── bliss/          - Driver program (front-end)
│   ├── bliss.c     - Main driver orchestrating compilation
│   └── Makefile.in - Build configuration
├── bcom/           - BLISS compiler frontend
│   ├── scan.l      - Lexical analyzer (BLISS tokenizer)
│   ├── bgram.y     - Parser grammar (Yacc/Bison)
│   ├── pass1.h     - Frontend data structures
│   ├── main.c      - Compiler main entry point
│   ├── symtab.c    - Symbol table management
│   ├── trees.c     - AST/IR generation
│   └── Makefile.in - Build configuration
└── README.md       - This file
```

## Features

The BLISS compiler supports core BLISS language features:

### Language Constructs
- **Module Structure**: MODULE, BEGIN, END
- **Declarations**: GLOBAL, BIND, LITERAL, LOCAL, OWN, EXTERNAL, FORWARD, REGISTER, STACKLOCAL
- **Routines**: ROUTINE with parameters
- **Control Flow**: IF-THEN-ELSE, CASE, SELECTONE, SELECTA, SELECTU
- **Loops**: DO-WHILE, DO-UNTIL, WHILE-DO, UNTIL-DO, INCR, DECR
- **Block Expressions**: BEGIN-END with local declarations
- **Labels**: LABEL declarations and LEAVE statements

### Data Structures
- **Vectors**: VECTOR, BLOCKVECTOR
- **Structures**: STRUCTURE with FIELD definitions
- **Literals**: Compile-time constants with LITERAL
- **Bindings**: BIND for compile-time name binding

### Operators
- **Arithmetic**: +, -, *, /
- **Logical**: AND, OR, XOR, EQV, NOT
- **Comparison**: EQL, EQLU, NEQ, NEQU, LSS, LSSU, LEQ, LEQU, GTR, GTRU, GEQ, GEQU
- **Special**: REF (address-of), ^ (pointer dereference), . (field access)

### Constants
- **Decimal**: 123
- **Octal**: %O'777
- **Hexadecimal**: %X'FF
- **Binary**: %B'1010
- **Character**: %C'A'
- **Angle brackets**: <n> for constants
- **Strings**: 'string literal'

### Comments
- Line comments: `! comment text` (BLISS-style)

## Building

The BLISS compiler is built as part of the PCC build system:

```bash
# Configure PCC (from top-level directory)
./configure

# Build everything including BLISS
make

# Or build just BLISS
cd bliss
make
```

This will create:
- `bcom` - The BLISS compiler frontend (in `bcom/`)
- `bliss` - The driver program (in `bliss/`)

## Usage

### Basic Compilation

```bash
# Compile a BLISS source file to executable
bliss hello.bli

# Compile to assembly
bliss -S hello.bli -o hello.s

# Compile to object file
bliss -c hello.bli -o hello.o

# Verbose output
bliss -v hello.bli
```

### File Extensions

The BLISS compiler recognizes the following extensions:
- `.bli` - BLISS source file
- `.bliss` - BLISS source file (alternative)
- `.b32` - BLISS-32 (VAX) source
- `.b36` - BLISS-36 (PDP-10) source
- `.req` - BLISS require file (library/header)

## Architecture

The BLISS compiler follows the PCC multi-pass architecture:

1. **Frontend (bcom)**:
   - Lexical analysis (scan.l)
   - Parsing (bgram.y)
   - Symbol table construction
   - AST to MIP IR conversion

2. **Backend (PCC MIP)**:
   - Machine-independent IR optimization
   - Architecture-specific code generation
   - Register allocation
   - Assembly emission

By generating MIP IR, the BLISS compiler automatically supports all 18 PCC backend targets:
- x86 (i386, amd64, i86)
- ARM
- PowerPC
- MIPS (32/64-bit)
- SPARC64
- WebAssembly (WASM)
- VAX, PDP-11, PDP-10, PDP-7
- HP PA-RISC
- Motorola 68k
- And others

## Example BLISS Program

```bliss
MODULE hello =
BEGIN

FORWARD ROUTINE print_message;

GLOBAL ROUTINE main : NOVALUE =
BEGIN
    print_message()
END;

ROUTINE print_message : NOVALUE =
BEGIN
    ! Print a message
    LITERAL message = 'Hello, BLISS!';
    ! Implementation would call OS primitives
END;

END
ELUDOM
```

## Current Status

This is an initial implementation of the BLISS compiler for PCC. It includes:
- ✓ Complete lexical analyzer with BLISS syntax
- ✓ Parser grammar covering major BLISS constructs
- ✓ Symbol table management
- ✓ Basic IR generation framework
- ○ Full semantic analysis (in progress)
- ○ Complete MIP IR generation (in progress)
- ○ Standard library support (planned)

## References

- **BLISS Language Manual**: DEC documentation
- **BLISS-11 Programmer's Reference**: PDP-11 implementation
- **BLISS-32 Programmer's Reference**: VAX implementation
- **Original BLISS Paper**: Wulf et al., "BLISS: A Language for Systems Programming" (1971)

## Compatibility

This compiler aims to support BLISS-32 (VAX) syntax as a baseline, with extensions for modern targets. The exact dialect may be refined as the implementation matures.

## License

Copyright (c) 2025 PCC BLISS Compiler
Part of the Portable C Compiler project.
