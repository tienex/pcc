# Common LISP Compiler for PCC

This directory contains a Common LISP compiler implementation for the Portable C Compiler (PCC) framework.

## Overview

The Common LISP compiler (`lisp` / `lcom`) integrates with PCC's multi-language compiler infrastructure, using the same MIP (Machine-Independent Passes) backend and architecture-specific code generation as the C, C++, Pascal, and Fortran 77 compilers.

## Architecture

The compiler follows PCC's standard three-stage pipeline:

```
LISP Source (.lisp, .lsp, .cl)
    ↓
[Frontend: lcom - LISP Compiler]
    ├─ Lexical analysis (scan.l)
    ├─ Parsing (lgram.y)
    ├─ Semantic analysis
    └─ Code generation
    ↓
[MIP: Machine-Independent Intermediate Representation]
    ├─ Optimization passes
    ├─ Register allocation
    └─ Code generation
    ↓
[Backend: Architecture-Specific Code Generation]
    └─ Assembly emission
    ↓
Assembly Code → Assembler → Linker → Executable
```

## Components

### Driver Program (`lisp/lisp/`)
- **lisp.c**: Main driver that orchestrates the compilation pipeline
- Similar to `cc`, `pascal`, and `f77` drivers
- Handles file type detection, compilation phases, and linking

### Compiler Frontend (`lisp/lcom/`)
- **main.c**: Entry point and command-line processing
- **scan.l**: Flex-based lexical analyzer for LISP syntax
- **lgram.y**: Yacc-based grammar parser for Common LISP
- **pass1.h**: Data structures and declarations
- **symtab.c**: Symbol table management
- **types.c**: Type system and AST construction
- **error.c**: Error handling and diagnostics
- **builtins.c**: Built-in function definitions
- **codegen.c**: Code generation to assembly/IR

## Supported Features

### Data Types
- **NIL**: Empty list / false value
- **T**: True value
- **Integers**: Long integer numbers
- **Floats**: Double-precision floating point
- **Strings**: String literals
- **Symbols**: Identifiers and variables
- **Cons cells**: Pairs (car/cdr)
- **Lists**: Linked lists of cons cells
- **Functions**: First-class functions and lambdas

### Special Forms
- `defun`: Define functions
- `defvar`, `defparameter`, `defconstant`: Define variables
- `setq`: Set variable values
- `let`, `let*`: Local variable binding
- `lambda`: Anonymous functions
- `if`, `cond`, `case`, `when`, `unless`: Conditionals
- `progn`, `prog1`, `prog2`: Sequential execution
- `and`, `or`, `not`: Logical operators
- `do`, `dolist`, `dotimes`, `loop`: Iteration
- `return`: Early return from functions

### Built-in Functions

#### List Operations
- `car`, `cdr`, `cons`, `list`
- `append`, `reverse`, `length`, `nth`
- `first`, `rest`, `last`

#### Arithmetic
- `+`, `-`, `*`, `/`, `mod`, `rem`
- `1+`, `1-`, `abs`, `max`, `min`

#### Comparison
- `=`, `/=`, `<`, `>`, `<=`, `>=`
- `eq`, `eql`, `equal`

#### Type Predicates
- `null`, `atom`, `consp`, `listp`
- `numberp`, `integerp`, `floatp`
- `symbolp`, `stringp`

#### I/O
- `print`, `princ`, `prin1`, `write`
- `format`, `read`, `read-line`

#### String Operations
- `string`, `string-upcase`, `string-downcase`
- `string=`, `string<`, `concatenate`

#### Higher-Order Functions
- `funcall`, `apply`, `mapcar`, `mapc`
- `reduce`, `filter`

## Usage

### Compilation

```bash
# Compile a single LISP file
lisp hello.lisp -o hello

# Compile to assembly
lisp -S factorial.lisp

# Compile to object file
lisp -c mylib.lisp

# Verbose output
lisp -v program.lisp

# Link with libraries
lisp main.lisp -lm -o program
```

### File Extensions

The compiler recognizes the following file extensions:
- `.lisp` - Standard Common LISP source
- `.lsp` - Alternative LISP extension
- `.cl` - Common LISP extension
- `.l` - Generic LISP extension

### Compiler Options

```
Usage: lisp [options] file...

Options:
  -o <file>     Place output in <file>
  -c            Compile to object files, don't link
  -S            Compile to assembly, don't assemble
  -L<dir>       Add directory to library search path
  -l<library>   Link with library
  -v            Verbose output
  -h, --help    Show help message
```

### Frontend Options (lcom)

```
Usage: lcom [options] [file]

Options:
  -o file       Write output to file
  -W<warning>   Enable warning (all, unused, strict, etc.)
  -w            Suppress all warnings
  -ferror-limit=N  Set maximum errors before abort (default: 20)
  -fno-color    Disable colored diagnostics
  -fno-caret    Disable caret diagnostics
  -v            Verbose output
  --dump-ast    Dump abstract syntax tree
  --dump-symtab Dump symbol table
  -h, --help    Show help message
```

## Example Programs

### Hello World
```lisp
(defun hello ()
  (print "Hello, World!"))

(hello)
```

### Factorial
```lisp
(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(print (factorial 5))
```

### List Operations
```lisp
(defun sum-list (lst)
  (if (null lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(print (sum-list (list 1 2 3 4 5)))
```

## Building

The Common LISP compiler is built as part of the PCC build system:

```bash
cd lisp
make
make install
```

Or from the top-level PCC directory:
```bash
./configure
make
make install
```

## Implementation Notes

### Code Generation

The compiler generates x86-64 assembly code (or other architectures supported by PCC). Key points:

- **Registers**: Uses standard calling conventions
  - `%rax`: Return values and primary accumulator
  - `%rbx`, `%rcx`, `%rdx`: General purpose
  - `%rdi`, `%rsi`, etc.: Function arguments

- **Memory Model**:
  - NIL represented as 0
  - T represented as 1
  - Tagged pointers for runtime type checking (future work)

- **Function Calls**: Standard C calling convention for interoperability

### Limitations

This is a simplified Common LISP compiler intended to demonstrate PCC's multi-language support. Current limitations:

- No garbage collection (manual memory management)
- Limited runtime library
- Subset of Common LISP standard
- No CLOS (Common Lisp Object System)
- No packages/modules
- No macros (yet)
- No reader macros
- Basic optimization only

### Future Enhancements

Potential improvements:
- Full Common LISP standard compliance
- Garbage collection integration
- CLOS support
- Package system
- Macro expansion
- Read-time evaluation
- Comprehensive runtime library
- Better optimization passes
- Interactive REPL mode

## Integration with PCC

The Common LISP compiler integrates seamlessly with PCC's infrastructure:

- **MIP Backend**: Shares optimization and register allocation
- **Multi-Architecture**: Supports all PCC target architectures (x86, ARM, MIPS, etc.)
- **Debug Formats**: DWARF, STABS, etc.
- **Assembly Formats**: GNU AS, NASM, etc.

## Testing

Test programs are located in `tests/`:

```bash
cd tests
../lisp/lisp hello.lisp -o hello
./hello
```

## License

Copyright (c) 2025 PCC Common LISP Compiler

Part of the Portable C Compiler project.

## References

- PCC Project: http://pcc.ludd.ltu.se/
- Common LISP: https://common-lisp.net/
- Common LISP HyperSpec: http://www.lispworks.com/documentation/HyperSpec/
