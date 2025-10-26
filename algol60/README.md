# ALGOL 60+ Compiler for PCC

This directory contains the ALGOL 60+ compiler implementation for the Portable C Compiler (PCC) infrastructure.

## Overview

ALGOL 60+ is a modern implementation of the classic ALGOL 60 programming language, integrated into the PCC multi-language compiler framework.

## Features

- **ALGOL 60 Standard Compliance**: Implements core ALGOL 60 features
- **Block Structure**: Full support for nested blocks and scope
- **Procedures**: Procedures with call-by-value and call-by-name parameters
- **Arrays**: Multi-dimensional arrays with arbitrary bounds
- **Type System**: integer, real, boolean, and string types
- **Control Structures**: if-then-else, for loops (step-until and while), while loops, goto
- **Mathematical Functions**: Standard transfer functions (sin, cos, sqrt, ln, exp, etc.)
- **Extensions**: String type and I/O operations

## Directory Structure

```
algol60/
├── algol/          - Driver program (algol command)
├── acom/           - ALGOL 60+ compiler frontend
├── tests/          - Test programs
└── README.md       - This file
```

## Building

The ALGOL 60+ compiler is built as part of the PCC build system:

```bash
./configure
make
```

## Usage

Compile an ALGOL 60+ program:

```bash
algol -o program program.alg
```

Options:
- `-c`: Compile to object file only
- `-S`: Compile to assembly only
- `-o <file>`: Specify output filename
- `-v`: Verbose output

## File Extensions

- `.alg`: ALGOL 60+ source files
- `.alg60`: ALGOL 60 source files
- `.algol`: ALGOL source files

## Example Programs

See the `tests/` directory for example programs:

- `hello.alg`: Simple hello world
- `factorial.alg`: Recursive factorial computation
- `fibonacci.alg`: Fibonacci sequence
- `array.alg`: Array operations
- `mathfunc.alg`: Mathematical functions

## Language Reference

### Basic Syntax

```algol
comment This is a comment;

begin
  integer n;
  real x;
  boolean flag;

  n := 42;
  x := 3.14159;
  flag := true
end
```

### Procedures

```algol
integer procedure factorial(n);
  value n;
  integer n;
begin
  if n <= 1 then
    factorial := 1
  else
    factorial := n * factorial(n - 1)
end;
```

### Arrays

```algol
integer array A[1:10];
real array Matrix[0:9, 0:9];
```

### Control Structures

```algol
if x > 0 then
  y := sqrt(x)
else
  y := 0;

for i := 1 step 1 until 10 do
  A[i] := i * i;

while x > 0.001 do
  x := x / 2;
```

### Built-in Functions

- Arithmetic: `abs`, `sign`, `sqrt`
- Trigonometric: `sin`, `cos`, `arctan`
- Logarithmic: `ln`, `exp`
- Rounding: `entier` (floor function)
- I/O: `read`, `write`, `print`

## Implementation Notes

- The compiler uses Flex for lexical analysis and Yacc for parsing
- The frontend generates PCC intermediate representation (IR)
- The IR is consumed by PCC's architecture-specific backends
- Supports all PCC target architectures (x86, AMD64, ARM, WASM, etc.)

## Differences from Standard ALGOL 60

Extensions:
- String type and string literals
- Simplified I/O (`read`, `write`)
- Underscore allowed in identifiers
- Single-line comments with `comment ... ;`

Limitations:
- Jensen's device (call-by-name) not fully implemented
- Switch declarations not fully implemented
- Some advanced features may be incomplete

## Contributing

This is part of the PCC project. For contributions and bug reports, please use the main PCC repository.

## License

Part of the Portable C Compiler (PCC) project.
Copyright (c) 2025

## See Also

- PCC documentation
- ALGOL 60 Revised Report (1963)
- Original PCC language frontends: C, Pascal, Fortran-77
