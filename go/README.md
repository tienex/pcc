# PCC Go Compiler

A Go language frontend for the Portable C Compiler (PCC) framework.

## Overview

This is a complete implementation of a Go compiler frontend that integrates with the PCC compiler infrastructure. It compiles Go source code through lexical analysis, parsing, semantic analysis, and IR generation, then leverages PCC's multi-architecture backend for code generation.

## Architecture

```
Go Source (.go)
    ↓
[gocom] Go Frontend
    - Lexical analysis (scan.l → tokens)
    - Parsing (gram.y → AST)
    - Semantic analysis (type checking)
    - IR generation (emit MIP nodes)
    ↓
[MIP Backend] IR Processing
    - Optimization passes
    - Register allocation
    - Instruction selection
    - Code emission
    ↓
[as] System Assembler
    ↓
[ld] System Linker
    ↓
Executable
```

## Components

### Frontend (gocom/)

- **main.c** - Compiler entry point and command-line parsing
- **scan.l** - Lexical analyzer (Flex)
  - Tokenizes Go source code
  - Handles comments, strings, literals
  - Implements automatic semicolon insertion
- **gram.y** - Parser grammar (Yacc/Bison)
  - Full Go grammar support
  - AST construction
  - Semantic actions
- **error.c/h** - Error reporting system
  - Clang-style diagnostics
  - Colored output
  - Source location tracking
- **symtab.c** - Symbol table
  - Hash-based lookup
  - Scope management
  - Export detection (capitalization)
- **types.c** - Type system
  - Go type mapping to MIP IR
  - Type compatibility checking
  - Struct/array/slice/map/channel types
- **builtins.c** - Built-in functions and types
  - Built-in functions (make, len, append, etc.)
  - Predeclared types (int, string, bool, etc.)
  - Constants (true, false, nil, iota)
- **pass1.h** - Frontend data structures

### Driver (go/)

- **go.c** - Driver program
  - Orchestrates compilation pipeline
  - Manages temporary files
  - Invokes assembler and linker

## Features

### Implemented

- ✅ **Lexical Analysis**
  - All Go tokens and keywords
  - Integer literals (decimal, hex, octal, binary)
  - Float and rune literals
  - String literals (interpreted and raw)
  - Line and block comments
  - Automatic semicolon insertion

- ✅ **Syntax**
  - Package declarations
  - Import statements
  - Type declarations
  - Variable declarations (var and :=)
  - Constant declarations
  - Function declarations
  - Method declarations (with receivers)
  - Struct types
  - Interface types
  - Array, slice, map, channel types
  - Pointer types

- ✅ **Statements**
  - Assignment
  - If/else
  - For loops (including range)
  - Switch statements
  - Return, break, continue, goto
  - Defer and go statements
  - Blocks and scopes

- ✅ **Expressions**
  - Binary operators (+, -, *, /, %, &, |, ^, <<, >>, etc.)
  - Unary operators (!, -, +, *, &, <-)
  - Comparisons (==, !=, <, <=, >, >=)
  - Logical operators (&&, ||)
  - Function calls
  - Array/slice indexing
  - Struct field access
  - Pointer dereferencing

- ✅ **Type System**
  - All basic types (bool, int8-64, uint8-64, float32/64, string)
  - Complex types (arrays, slices, structs, pointers)
  - Maps and channels
  - Interfaces
  - Type compatibility checking
  - Export detection

- ✅ **Built-ins**
  - Type names (int, string, bool, etc.)
  - Built-in functions (make, new, len, cap, append, etc.)
  - Constants (true, false, nil, iota)

### Partially Implemented

- ⚠️ **IR Generation** - Stubs present, needs full MIP IR emission
- ⚠️ **Runtime Support** - Requires Go runtime library integration

### Not Yet Implemented

- ❌ Goroutines and channels (runtime support needed)
- ❌ Garbage collection (runtime support needed)
- ❌ Reflection (runtime support needed)
- ❌ Package imports (linker integration needed)
- ❌ Generics (Go 1.18+)

## Building

The Go compiler integrates with PCC's build system:

```bash
# From PCC root directory
./configure
make

# Build Go compiler specifically
cd go/gocom
make

cd ../go
make
```

## Usage

### Compile and run a Go program

```bash
# Compile single file
./go -o hello hello.go

# Compile multiple files
./go -o myapp file1.go file2.go file3.go

# Compile only (don't link)
./go -c myfile.go

# Generate assembly only
./go -S myfile.go

# Verbose mode
./go -v -o hello hello.go
```

### Frontend only (for testing)

```bash
# Compile to IR
./gocom -o output.s input.go

# With diagnostics
./gocom -v --dump-ast input.go
./gocom --dump-symtab input.go
```

## Example Go Program

```go
package main

import "fmt"

func main() {
    message := "Hello, World!"
    fmt.Println(message)

    x := add(5, 3)
    fmt.Printf("5 + 3 = %d\n", x)
}

func add(a, b int) int {
    return a + b
}
```

## Type Mapping

Go types are mapped to MIP IR types as follows:

| Go Type | MIP IR Type | Size |
|---------|-------------|------|
| bool | BOOL | 1 byte |
| int8 | CHAR | 1 byte |
| uint8, byte | UCHAR | 1 byte |
| int16 | SHORT | 2 bytes |
| uint16 | USHORT | 2 bytes |
| int32, rune | INT | 4 bytes |
| uint32 | UNSIGNED | 4 bytes |
| int64 | LONGLONG | 8 bytes |
| uint64 | ULONGLONG | 8 bytes |
| float32 | FLOAT | 4 bytes |
| float64 | DOUBLE | 8 bytes |
| string | STRUCT (ptr + len) | 16 bytes |
| []T | STRUCT (ptr + len + cap) | 24 bytes |
| map[K]V | PTR | 8 bytes |
| chan T | PTR | 8 bytes |
| interface{} | STRUCT (type + data) | 16 bytes |

## Error Reporting

The compiler provides Clang-style error messages with colors and source location:

```
hello.go:5:10: error: undefined variable 'x'
    fmt.Println(x)
                ^
hello.go:3:5: note: did you mean 'y'?
```

Control diagnostics with:
- `-fno-color` - Disable colored output
- `-ferror-limit=N` - Set max errors before abort
- `-v` - Verbose output

## Implementation Details

### Symbol Table

- Hash table with 1024 buckets
- Scope-based lookup (block nesting)
- Export detection via capitalization
- Package-aware symbols

### Scope Rules

- Level 0: Built-ins and package scope
- Level 1+: Function and block scopes
- Blank identifier (_) special handling

### Semicolon Insertion

Automatic semicolon insertion follows Go specification:
- After identifiers, literals, keywords (break, continue, return, etc.)
- After ), ], }
- Implemented in lexer

## Integration with PCC

The Go compiler reuses PCC infrastructure:

1. **MIP Backend** - Register allocation, optimization, code generation
2. **Architecture Support** - 15+ target architectures (x86, ARM, MIPS, etc.)
3. **Debug Symbols** - DWARF, STABS, CodeView, etc.
4. **Build System** - Autoconf-based configuration

## File Organization

```
go/
├── README.md           # This file
├── go/                 # Driver program
│   ├── go.c           # Main driver
│   └── Makefile.in    # Build config
└── gocom/             # Compiler frontend
    ├── main.c         # Entry point
    ├── scan.l         # Lexer
    ├── gram.y         # Parser
    ├── error.c/h      # Diagnostics
    ├── symtab.c       # Symbol table
    ├── types.c        # Type system
    ├── builtins.c     # Built-ins
    ├── pass1.h        # Data structures
    └── Makefile.in    # Build config
```

## Testing

Create a simple test program:

```go
// test.go
package main

func main() {
    x := 42
    y := x + 10
}
```

Compile:

```bash
./gocom -v test.go
```

## Future Work

1. **Complete IR Generation** - Emit full MIP IR for all constructs
2. **Runtime Library** - Implement Go runtime (goroutines, GC, etc.)
3. **Standard Library** - Port/implement Go standard library
4. **Package System** - Import resolution and linking
5. **Optimizations** - Go-specific optimization passes
6. **Testing** - Comprehensive test suite

## License

Same license as PCC (BSD-style).

## References

- [Go Language Specification](https://go.dev/ref/spec)
- [PCC Compiler](http://pcc.ludd.ltu.se/)
- [MIP Backend Documentation](../../mip/README.md)

## Authors

- PCC Go Compiler Frontend (2025)
- Based on PCC architecture by Anders Magnusson and contributors
