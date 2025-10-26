# Python Frontend Compiler for PCC

## Overview

This is a Python frontend compiler integrated into the Portable C Compiler (PCC) infrastructure. It compiles Python source code into PCC's intermediate representation (IR), which can then be compiled to machine code using PCC's existing backends for multiple architectures.

## Features

### Supported Python Features

#### Language Constructs
- **Function Definitions**: `def` statements with parameters
- **Control Flow**: `if/elif/else`, `while` loops
- **Expressions**: Arithmetic, logical, bitwise operations
- **Assignment**: Simple variable assignment
- **Return Statements**: Function returns
- **Function Calls**: With multiple arguments

#### Operators
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`, `//` (floor division), `**` (power)
- **Comparison**: `==`, `!=`, `<`, `<=`, `>`, `>=`
- **Logical**: `and`, `or`, `not`
- **Bitwise**: `&`, `|`, `^`, `~`, `<<`, `>>`
- **Assignment**: `=`, `+=`, `-=`, `*=`, `/=`

#### Data Types
- **Integers**: `int` (mapped to LONGLONG in PCC IR)
- **Strings**: `str` (mapped to CHAR pointers)
- **Booleans**: `True`, `False`, `None`

### Architecture

The Python compiler follows a classic 3-phase compilation model:

```
Python Source (.py)
    ↓
[Lexer] - Tokenization
    ↓
[Parser] - AST Construction
    ↓
[Code Generator] - PCC IR Generation
    ↓
PCC IR (NODE tree)
    ↓
[PCC Pass 2] - Optimization & Code Generation
    ↓
[PCC Backend] - Architecture-specific assembly
    ↓
Assembly (.s)
```

## File Structure

```
cc/pycom/
├── README.md           # This file
├── Makefile           # Build system
├── pycom.h            # Main header file with data structures
├── main.c             # Driver program
├── lexer.c            # Lexical analyzer (tokenizer)
├── parser.c           # Syntax analyzer (AST builder)
├── symtab.c           # Symbol table management
├── codegen.c          # IR code generation
├── util.c             # Utility functions
└── examples/          # Example Python programs
    ├── hello.py
    ├── factorial.py
    ├── fibonacci.py
    ├── loops.py
    └── arithmetic.py
```

## Building

### Prerequisites
- GCC or compatible C compiler
- Make build system
- PCC headers (in `../../mip` and `../../common`)

### Build Instructions

```bash
cd cc/pycom
make
```

This will produce the `pycom` executable.

### Clean Build

```bash
make clean       # Remove object files and executable
make distclean   # Remove all generated files
```

## Usage

### Basic Compilation

```bash
./pycom input.py
```

This compiles `input.py` and outputs to stdout.

### Options

```bash
./pycom [options] <input.py>

Options:
  -o <file>    Write output to <file>
  -v           Verbose mode (shows AST and compilation stages)
  -h           Show help message
```

### Examples

```bash
# Compile with verbose output
./pycom -v examples/hello.py

# Compile and save to file
./pycom -o output.s examples/factorial.py

# Run all tests
make test
```

## Implementation Details

### Lexer (lexer.c)

The lexer performs tokenization with the following features:
- **Indentation-based scoping**: Tracks INDENT/DEDENT tokens for Python's significant whitespace
- **String/number literals**: Parses integer, float, and string constants
- **Keyword recognition**: Identifies Python keywords vs. identifiers
- **Operator tokenization**: Handles single and multi-character operators
- **Comment skipping**: Ignores `#` comments

### Parser (parser.c)

The parser builds an Abstract Syntax Tree (AST) using recursive descent:
- **Expression parsing**: Operator precedence climbing
  - Primary: literals, identifiers, parenthesized expressions
  - Unary: `-`, `not`, `~`
  - Multiplicative: `*`, `/`, `%`, `//`
  - Additive: `+`, `-`
  - Shift: `<<`, `>>`
  - Bitwise: `&`, `^`, `|`
  - Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
  - Logical: `and`, `or`

- **Statement parsing**:
  - Function definitions with parameter lists
  - If/else conditionals
  - While loops
  - Return statements
  - Assignments
  - Expression statements

### Symbol Table (symtab.c)

Manages variable and function symbols:
- **Scoped lookup**: Searches current scope, then parent scopes
- **Symbol properties**: Name, type, offset, function/parameter flags
- **Automatic offset allocation**: Assigns stack offsets to variables

### Code Generator (codegen.c)

Converts AST to PCC IR nodes:
- **Expression codegen**: Translates Python expressions to PCC node operations
  - Binary operators → PLUS, MINUS, MUL, DIV, MOD, AND, OR, ER, LS, RS
  - Comparisons → EQ, NE, LT, LE, GT, GE
  - Unary operators → UMINUS, COMPL
  - Function calls → CALL nodes with CM (comma) argument lists

- **Statement codegen**:
  - Assignments → ASSIGN nodes
  - Returns → RETURN nodes
  - Conditionals → CBRANCH (conditional branch) with labels
  - Loops → Label management for break/continue

- **Type mapping**:
  - Python `int` → PCC LONGLONG
  - Python `str` → PCC PTR | CHAR
  - Python `bool` → PCC INT

### AST Node Types

The compiler uses the following AST node types:

```c
AST_MODULE          // Top-level module
AST_FUNCTION_DEF    // Function definition
AST_RETURN          // Return statement
AST_IF              // If/else statement
AST_WHILE           // While loop
AST_ASSIGN          // Assignment
AST_EXPR_STMT       // Expression statement
AST_BINOP           // Binary operation
AST_UNOP            // Unary operation
AST_COMPARE         // Comparison
AST_CALL            // Function call
AST_NAME            // Variable/function name
AST_NUMBER          // Integer literal
AST_STRING          // String literal
AST_PASS            // Pass statement
AST_BREAK           // Break statement
AST_CONTINUE        // Continue statement
```

## Integration with PCC

### IR Compatibility

The Python compiler generates standard PCC IR nodes that are compatible with:
- **18 PCC backends**: x86, x86-64, ARM, MIPS, PowerPC, SPARC, WebAssembly, and more
- **PCC optimization passes**: Register allocation, peephole optimization
- **PCC code emitters**: Assembly generation for all supported architectures

### Type System Mapping

| Python Type | PCC IR Type | Description |
|-------------|-------------|-------------|
| `int` | `LONGLONG` | 64-bit signed integer |
| `float` | `DOUBLE` | Double-precision float (future) |
| `str` | `PTR\|CHAR` | Pointer to character array |
| `bool` | `INT` | Integer (0 or 1) |
| `None` | `VOID` | No value |

### Node Operations

| Python Operator | PCC Node | Description |
|-----------------|----------|-------------|
| `+` | `PLUS` | Addition |
| `-` | `MINUS` | Subtraction |
| `*` | `MUL` | Multiplication |
| `/` | `DIV` | Division |
| `%` | `MOD` | Modulo |
| `<<` | `LS` | Left shift |
| `>>` | `RS` | Right shift |
| `&` | `AND` | Bitwise AND |
| `\|` | `OR` | Bitwise OR |
| `^` | `ER` | Bitwise XOR |
| `~` | `COMPL` | Bitwise complement |
| `==` | `EQ` | Equal |
| `!=` | `NE` | Not equal |
| `<` | `LT` | Less than |
| `<=` | `LE` | Less or equal |
| `>` | `GT` | Greater than |
| `>=` | `GE` | Greater or equal |
| `-x` | `UMINUS` | Unary minus |

## Limitations and Future Work

### Current Limitations

1. **No classes**: Object-oriented features not yet implemented
2. **No lists/dicts**: Only scalar types supported
3. **No for loops**: Only while loops supported
4. **No exceptions**: Try/except not implemented
5. **No imports**: Module system not available
6. **No type annotations**: Type hints ignored
7. **Limited runtime**: No garbage collection or dynamic typing

### Planned Features

1. **Enhanced type system**: Support for lists, tuples, dictionaries
2. **For loops**: Iterator-based loops with `for...in`
3. **Classes and objects**: Basic OOP support
4. **Exception handling**: Try/except/finally
5. **Module imports**: Import system integration
6. **Standard library**: Built-in functions (len, range, etc.)
7. **Lambda expressions**: Anonymous functions
8. **List comprehensions**: Pythonic iteration constructs
9. **Garbage collection**: Automatic memory management
10. **Dynamic typing**: Runtime type checking

## Testing

### Running Tests

```bash
make test
```

This runs the compiler on all example programs in the `examples/` directory.

### Example Programs

1. **hello.py**: Simple function definition and call
2. **factorial.py**: Recursive factorial calculation
3. **fibonacci.py**: Recursive Fibonacci with loops
4. **loops.py**: While loop examples
5. **arithmetic.py**: Arithmetic and bitwise operations

### Adding New Tests

Create a `.py` file in `examples/` and run:

```bash
./pycom -v examples/your_test.py
```

## Contributing

Contributions are welcome! Areas for improvement:
- Additional Python language features
- Optimization passes
- Better error messages
- More comprehensive testing
- Integration with PCC build system

## License

This code follows the PCC licensing:
- Copyright notices from Caldera International Inc. apply to PCC infrastructure
- New Python frontend code uses compatible BSD-style license

## References

- [Python Language Reference](https://docs.python.org/3/reference/)
- [PCC - Portable C Compiler](http://pcc.ludd.ltu.se/)
- Compiler design resources and textbooks

## Contact

For questions or issues, please refer to the main PCC project documentation or submit issues to the PCC repository.
