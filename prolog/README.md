# PCC Prolog Compiler

A Prolog compiler compatible with both Turbo Prolog and GNU Prolog, integrated into the Portable C Compiler (PCC) framework.

## Features

- **Dual Compatibility**: Supports both Turbo Prolog and GNU Prolog syntax
- **Turbo Prolog Features**:
  - Domain declarations
  - Predicate type declarations
  - Structured sections (domains, predicates, clauses, goal)
  - Database predicates
  - Constants section

- **GNU Prolog / ISO Prolog Features**:
  - Dynamic predicates
  - Multifile predicates
  - Module system
  - Operator definitions
  - Meta-predicates
  - Standard built-in predicates

## Usage

### Compiling Prolog Programs

```bash
# Compile in GNU Prolog mode (default)
prolog -o myprogram myprogram.pl

# Compile in Turbo Prolog mode
prolog -t -o myprogram myprogram.pl

# Compile only (don't link)
prolog -c myprogram.pl

# Verbose output
prolog -v -o myprogram myprogram.pl
```

### Output Formats

The compiler can generate different output formats:

```bash
# C code (default)
plogcom -f c -o output.c input.pl

# Bytecode
plogcom -f bc -o output.bc input.pl

# Warren Abstract Machine (WAM)
plogcom -f wam -o output.wam input.pl
```

## Building

The Prolog compiler is built as part of the PCC build system:

```bash
./configure
make
make install
```

To build only the Prolog compiler:

```bash
cd prolog
make
```

## File Structure

```
prolog/
├── prolog/          # Driver program
│   ├── prolog.c     # Main driver
│   └── Makefile.in  # Driver makefile
├── plogcom/         # Compiler implementation
│   ├── main.c       # Compiler entry point
│   ├── pass1.h      # Data structures
│   ├── scan.l       # Lexical analyzer
│   ├── plgram.y     # Parser grammar
│   ├── symtab.c     # Symbol table
│   ├── terms.c      # Term manipulation
│   ├── builtins.c   # Built-in predicates
│   ├── codegen.c    # Code generation
│   ├── error.c      # Error handling
│   ├── util.c       # Utilities
│   └── Makefile.in  # Compiler makefile
├── tests/           # Test cases
│   ├── test_gnu_prolog.pl    # GNU Prolog tests
│   ├── test_turbo_prolog.pl  # Turbo Prolog tests
│   └── test_simple.pl        # Simple compatibility tests
└── README.md        # This file
```

## Built-in Predicates

### Control Predicates
- `true`, `fail`, `!` (cut), `call/1`

### Type Checking
- `var/1`, `nonvar/1`, `atom/1`, `number/1`, `integer/1`, `float/1`
- `compound/1`, `atomic/1`

### Term Comparison
- `=/2` (unification), `\=/2`, `==/2`, `\==/2`
- `@</2`, `@=</2`, `@>/2`, `@>=/2`

### Arithmetic
- `is/2`, `=:=/2`, `=\=/2`, `</2`, `=</2`, `>/2`, `>=/2`

### Term Manipulation
- `functor/3`, `arg/3`, `copy_term/2`, `=../2`

### Database Manipulation
- `asserta/1`, `assertz/1`, `retract/1`, `retractall/1`

### Meta-predicates
- `findall/3`, `bagof/3`, `setof/3`

### I/O
- `write/1`, `writeln/1`, `read/1`, `get/1`, `put/1`, `nl/0`

### List Operations
- `append/3`, `member/2`, `length/2`, `reverse/2`, `sort/2`

## Examples

### GNU Prolog Style

```prolog
% Facts
parent(tom, bob).
parent(tom, liz).

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Dynamic predicates
:- dynamic count/1.

% Query
?- grandparent(tom, X).
```

### Turbo Prolog Style

```prolog
domains
  name = symbol
  age = integer

predicates
  parent(name, name)
  age_of(name, age)

clauses
  parent(tom, bob).
  parent(tom, liz).
  age_of(tom, 65).
  age_of(bob, 40).

goal
  parent(X, Y), write(X), nl, fail.
```

## Compatibility Notes

- Both Turbo Prolog and GNU Prolog syntax are supported
- The compiler auto-detects the dialect based on the presence of section keywords
- Use `-t` flag to force Turbo Prolog mode
- Use `-g` flag to force GNU Prolog mode (default)

## License

Copyright (c) 2025 PCC Prolog Compiler Project

See the top-level PCC license for details.

## Contributing

Contributions are welcome! Please ensure that any additions maintain compatibility with both Turbo Prolog and GNU Prolog where possible.
