# Perl 5 Frontend for PCC

This directory contains the Perl 5 programming language frontend for the Portable C Compiler (PCC).

## Overview

The Perl 5 frontend compiles Perl 5 source code into intermediate representation (IR) that can be processed by the PCC middle-end and backend infrastructure. This allows Perl 5 programs to be compiled to native machine code for any architecture supported by PCC.

## Architecture

The Perl 5 compiler follows the standard PCC frontend architecture:

```
Perl 5 Source (.pl, .pm)
    ↓
[Lexical Analysis] (scan.l)
    ↓
[Parsing] (pgram.y)
    ↓
[Semantic Analysis] (trees.c, symtab.c)
    ↓
[Code Generation] (codegen.c)
    ↓
PCC Intermediate Representation
    ↓
[PCC Middle-End] (mip/)
    ↓
[Backend] (arch/*)
    ↓
Native Assembly/Object Code
```

## Directory Structure

```
perl5/
├── perlcom/              # Perl 5 compiler backend
│   ├── pass1.h           # Frontend definitions
│   ├── scan.l            # Lexical analyzer (Flex)
│   ├── pgram.y           # Parser grammar (Bison)
│   ├── main.c            # Main compiler driver
│   ├── trees.c           # AST construction
│   ├── symtab.c          # Symbol table management
│   ├── codegen.c         # Code generation
│   └── Makefile.in       # Build system
│
├── perl5/                # Compiler driver program
│   ├── perl5.c           # Driver implementation
│   └── Makefile.in       # Build system
│
├── tests/                # Test suite
│   ├── test_*.pl         # Test programs
│   └── run_tests.sh      # Test runner
│
└── README.md             # This file
```

## Components

### 1. Lexical Analyzer (`scan.l`)

The lexer tokenizes Perl 5 source code into tokens. It handles:

- **Keywords**: `sub`, `my`, `our`, `if`, `while`, `for`, `foreach`, etc.
- **Variables**: `$scalar`, `@array`, `%hash`, `&sub`, `*glob`
- **Special Variables**: `$_`, `$@`, `$!`, `$$`, `$0`, etc.
- **Literals**: integers, floats, strings, regex patterns
- **Operators**: arithmetic, comparison, logical, string, assignment
- **Comments**: `#` single-line and POD multi-line comments

### 2. Parser (`pgram.y`)

The parser implements Perl 5 grammar using Bison. It handles:

- **Subroutine definitions**: `sub name { ... }`
- **Variable declarations**: `my $var`, `our @array`, `local %hash`
- **Control structures**: `if/elsif/else`, `unless`, `while`, `until`, `for`, `foreach`
- **Expressions**: All Perl operators with correct precedence
- **Built-in functions**: `print`, `push`, `pop`, `keys`, `values`, etc.
- **Regular expressions**: Pattern matching and substitution

### 3. Semantic Analyzer (`trees.c`, `symtab.c`)

Semantic analysis includes:

- **Symbol table management**: Tracks variables, scopes, and types
- **Scope management**: Lexical scoping (`my`), package globals (`our`)
- **AST construction**: Builds abstract syntax tree
- **Type checking**: Validates operations (where applicable)
- **Auto-vivification**: Perl-style automatic variable creation

### 4. Code Generator (`codegen.c`)

Generates intermediate representation for:

- **Variable access**: Scalars, arrays, hashes
- **Assignments**: All assignment operators
- **Control flow**: Conditionals, loops, jumps
- **Function calls**: Subroutines and built-in functions
- **Expressions**: All operators
- **Regular expressions**: Basic pattern support

### 5. Driver (`perl5.c`)

The driver orchestrates compilation:

- **Command-line parsing**: Options like `-o`, `-c`, `-S`, `-v`, `-O`
- **Multi-file compilation**: Handles multiple input files
- **Pipeline management**: Coordinates compilation, assembly, linking
- **Temporary files**: Manages intermediate files

## Features Supported

### Core Language Features

✅ **Variables**
- Scalars: `$scalar`
- Arrays: `@array`
- Hashes: `%hash`
- Special variables: `$_`, `$@`, `$!`, etc.

✅ **Data Types**
- Integers (decimal, hex, octal, binary)
- Floating-point numbers
- Strings (double-quoted, single-quoted)
- References (basic support)

✅ **Operators**
- Arithmetic: `+`, `-`, `*`, `/`, `%`, `**`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`, `<=>`
- String: `eq`, `ne`, `lt`, `gt`, `le`, `ge`, `cmp`
- Logical: `&&`, `||`, `!`, `and`, `or`, `not`, `xor`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `.=`, etc.
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- String concatenation: `.`
- Pattern matching: `=~`, `!~`

✅ **Control Structures**
- Conditionals: `if/elsif/else`, `unless`
- Loops: `while`, `until`, `for`, `foreach`, `do/while`, `do/until`
- Loop control: `next`, `last`, `redo`

✅ **Subroutines**
- Definition: `sub name { ... }`
- Parameters: `my ($a, $b) = @_;`
- Return values: `return $value;`
- Calls: `name(args)`

✅ **Built-in Functions**
- I/O: `print`, `printf`, `say`
- Array: `push`, `pop`, `shift`, `unshift`
- Hash: `keys`, `values`, `each`, `delete`, `exists`
- String: `length`, `substr`, `index`, `rindex`, `chomp`, `chop`
- List: `join`, `split`, `grep`, `map`, `sort`, `reverse`

✅ **Scoping**
- Lexical: `my` variables
- Package: `our` variables
- Dynamic: `local` variables
- State: `state` variables

✅ **Comments**
- Single-line: `# comment`
- Multi-line: POD (`=pod` ... `=cut`)

### Planned Features

⏳ **Advanced Features** (Future work)
- Object-oriented Perl (bless, packages, methods)
- Regular expression engine (currently stub)
- File I/O operations
- Module system (`use`, `require`)
- References and dereferencing
- Anonymous subroutines
- Closures
- Exceptions (`eval`, `die`)
- Pragmas and attributes

## Building

### Prerequisites

- C compiler (GCC or Clang)
- GNU Make
- Flex (lexer generator)
- Bison (parser generator)
- PCC build system configured

### Build Instructions

From the top-level PCC directory:

```bash
./configure
make
```

Or build just the Perl 5 frontend:

```bash
cd perl5
make
```

This will build:
- `perlcom/perlcom` - The Perl 5 compiler backend
- `perl5/perl5` - The compiler driver

### Installation

```bash
make install
```

This installs:
- `perl5` to `$(bindir)/perl5`
- `perlcom` to `$(libexecdir)/perlcom`

## Usage

### Basic Compilation

Compile a Perl script to assembly:

```bash
perl5 -S -o output.s input.pl
```

Compile to object file:

```bash
perl5 -c -o output.o input.pl
```

Compile and link to executable:

```bash
perl5 -o program input.pl
```

### Options

- `-o file` - Write output to file
- `-c` - Compile and assemble, but do not link
- `-S` - Compile only to assembly, do not assemble
- `-v` - Verbose mode (show compilation steps)
- `-O` - Enable optimizations
- `-save-temps` - Keep temporary files

### Examples

Compile hello world:

```bash
perl5 tests/test_hello.pl
./a.out
```

Compile with optimizations:

```bash
perl5 -O -o optimized tests/test_operators.pl
```

Generate assembly for inspection:

```bash
perl5 -S -o program.s tests/test_control.pl
cat program.s
```

## Testing

Run the test suite:

```bash
cd tests
./run_tests.sh
```

This will compile all test programs and report results.

Individual test:

```bash
cd tests
../perl5/perl5 -v -S test_hello.pl
```

## Implementation Details

### Type System

Perl's dynamic typing is handled through:

- **PERLSV** - Scalar value (can be int, float, string, reference)
- **PERLAV** - Array value
- **PERLHV** - Hash value
- **PERLCV** - Code value (subroutine)
- **PERLGV** - Glob value

All values are boxed at runtime to support dynamic typing.

### Symbol Table

The symbol table uses:

- Hash-based lookup (256 buckets)
- Chaining for collisions
- Scope levels for lexical scoping
- Separate namespaces for different sigils (`$`, `@`, `%`, `&`)

### Code Generation

Code generation produces PCC IR that:

- Maps Perl variables to PCC symbols
- Generates control flow graphs
- Handles type conversions
- Emits function calls for built-ins
- Interfaces with runtime library (future work)

## Limitations

Current limitations:

1. **No Runtime Library**: The compiler generates code but doesn't yet have a complete runtime library for dynamic typing, reference counting, etc.

2. **Basic Regex**: Regular expressions are parsed but not fully compiled.

3. **No OO Support**: Object-oriented features (bless, packages, methods) not yet implemented.

4. **Limited I/O**: File operations are stubbed.

5. **No Modules**: `use` and `require` are recognized but not functional.

6. **Static Analysis**: Limited to what can be determined at compile time; much of Perl's dynamism is deferred to runtime.

## Future Work

Planned improvements:

- [ ] Complete runtime library with reference counting
- [ ] Regular expression compiler
- [ ] Object-oriented features
- [ ] Module system
- [ ] XS interface for C extensions
- [ ] Debugger support
- [ ] Profiler integration
- [ ] More comprehensive optimizations

## Contributing

When contributing to the Perl 5 frontend:

1. Follow PCC coding style
2. Add tests for new features
3. Update documentation
4. Ensure backward compatibility
5. Test on multiple architectures

## References

- [Perl 5 Language Reference](https://perldoc.perl.org/)
- [PCC Documentation](https://pcc.ludd.ltu.se/)
- [Compiler Design Principles](https://www.cs.princeton.edu/~appel/modern/)

## License

This code follows the PCC license (BSD 3-clause). See the top-level LICENSE file for details.

## Authors

- Perl 5 Frontend implementation (2025)
- Based on PCC infrastructure by the PCC developers

## Contact

For questions or issues with the Perl 5 frontend, please file an issue in the PCC repository.
