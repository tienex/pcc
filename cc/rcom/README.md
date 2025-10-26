# Ruby Front-End Compiler (rcom) for PCC

## Overview

This is a Ruby front-end compiler for the Portable C Compiler (PCC) infrastructure. It compiles Ruby source code to P1ND intermediate representation, which can then be compiled to machine code for any of PCC's supported architectures.

## Architecture

The Ruby compiler follows the PCC two-pass architecture:

```
Ruby Source → Lexer → Parser → P1ND IR → Pass 2 → Machine Code
```

### Components

- **scan.l**: Lexical analyzer (tokenizer) for Ruby syntax
- **cgram.y**: Grammar parser that builds Abstract Syntax Trees
- **pass1.h**: Ruby-specific IR definitions and extensions
- **trees.c**: Tree manipulation and building functions
- **symtabs.c**: Symbol table management for variables, methods, and classes
- **main.c**: Compiler entry point and command-line processing
- **pftn.c**: Function/method definition handling
- **init.c**: Initialization code handling

## Supported Features

### Current Implementation

- **Basic Expressions**: Arithmetic, logical, comparison operators
- **Variables**: Local variables, instance variables (`@var`), class variables (`@@var`), global variables (`$var`)
- **Control Flow**: `if/elsif/else`, `while`, `for..in`, `case/when`
- **Methods**: Method definitions with parameters
- **Classes**: Class definitions with optional inheritance
- **Literals**: Integers, floats, strings, symbols, arrays
- **Method Calls**: Object-oriented method invocation
- **Built-in Methods**: `puts`, `print`, `gets`

### Ruby-Specific Features

1. **Symbols**: Converted to string constants (`:symbol`)
2. **Instance Variables**: `@variable` - tracked with RUBY_IVAR class
3. **Class Variables**: `@@variable` - tracked with RUBY_CVAR class
4. **Global Variables**: `$variable` - tracked with RUBY_GVAR class
5. **Blocks**: Basic block support with `{ }` and `do..end` syntax
6. **Method Visibility**: `private`, `protected`, `public` keywords

## Limitations

This is a **minimal implementation** with several limitations:

1. **Static Typing**: Ruby's dynamic type system is simplified to static types
2. **Object Model**: Simplified object model (classes mapped to structs)
3. **Metaprogramming**: No support for `eval`, `define_method`, etc.
4. **Garbage Collection**: No automatic memory management
5. **Standard Library**: No Ruby standard library support
6. **Advanced Features**: No modules, mixins, or metaclasses
7. **Blocks/Procs**: Limited block support (treated as function pointers)

## Building

To build the Ruby compiler, you need to configure PCC with Ruby support:

```bash
./configure --enable-ruby
make
```

This will create the `rcom` executable in the build directory.

## Usage

Compile a Ruby source file:

```bash
rcom input.rb output.s
```

### Command-Line Options

- `-X<flags>`: Pass 1 debugging flags
  - `-Xb`: Debug tree building
  - `-Xd`: Debug declarations
  - `-Xt`: Debug type matching
- `-g`: Generate debugging information
- `-O`: Enable optimizations
- `-v`: Print version information
- `-s`: Print compilation statistics

## Example

### Simple Ruby Program

```ruby
def greet(name)
  puts "Hello, " + name
end

class Calculator
  def add(x, y)
    return x + y
  end
end

calc = Calculator.new
result = calc.add(5, 3)
puts result

greet("World")
```

### Compilation

```bash
rcom example.rb example.s  # Compile to assembly
as example.s -o example.o   # Assemble
ld example.o -o example     # Link
./example                   # Run
```

## Target Architectures

The Ruby compiler can target all PCC-supported architectures:

- **Modern**: amd64, ARM, WASM
- **Classic RISC**: MIPS, MIPS64, PowerPC, SPARC64
- **Classic CISC**: i386, i86, Motorola 68k, VAX
- **Historic**: PDP-7, PDP-10, Nova, M16C, and more

## Implementation Details

### P1ND IR Mapping

Ruby constructs are mapped to P1ND IR nodes:

| Ruby Construct | P1ND Operator | Notes |
|---------------|---------------|-------|
| `a + b` | `PLUS` | Arithmetic addition |
| `a && b` | `ANDAND` | Logical AND |
| `a == b` | `EQ` | Equality comparison |
| `obj.method(args)` | `CALL` | Method call (receiver as first arg) |
| `return x` | `RETURN` | Function return |
| `if cond then ... end` | `IF` | Conditional |
| `while cond do ... end` | `WHILE` | Loop |
| `a = b` | `ASSIGN` | Assignment |

### Symbol Table

The symbol table tracks:
- Variable names and their storage classes
- Method/function signatures
- Class definitions
- Scope levels for nested contexts
- Ruby-specific metadata (visibility, variable types)

### Type System

Ruby's dynamic types are mapped to C types:
- Integers → `INT` or `LONG`
- Floats → `DOUBLE`
- Strings → `PTR | CHAR` (char pointers)
- Objects → `STRTY` (struct types)
- Symbols → String constants

## Development Roadmap

### Phase 1: Core Language (Current)
- ✓ Variables and assignments
- ✓ Arithmetic and logical expressions
- ✓ Control flow (if, while, for)
- ✓ Method definitions
- ✓ Class definitions

### Phase 2: Object-Oriented Features
- [ ] Instance variables and methods
- [ ] Constructors (`initialize`)
- [ ] Inheritance with `super`
- [ ] Method visibility enforcement

### Phase 3: Advanced Features
- [ ] Blocks with yield
- [ ] Procs and lambdas
- [ ] Modules and mixins
- [ ] Exception handling (begin/rescue/end)

### Phase 4: Optimization
- [ ] Type inference
- [ ] Dead code elimination
- [ ] Constant folding
- [ ] Inline expansion

## Testing

Create test files in `tests/` directory:

```ruby
# tests/test_arithmetic.rb
a = 5
b = 3
c = a + b
puts c  # Should output 8
```

Run tests:
```bash
make test
```

## Contributing

When contributing to the Ruby front-end:

1. Follow PCC coding style
2. Ensure all Ruby constructs map to valid P1ND IR
3. Test on multiple architectures (amd64, ARM, WASM minimum)
4. Document new features in this README
5. Add test cases for new functionality

## References

- **PCC Documentation**: See `ARCHITECTURE_EXPLORATION_SUMMARY.md`
- **Ruby Language**: https://www.ruby-lang.org/
- **IR Guide**: See `ruby_frontend_guide.txt`
- **Original PCC**: http://pcc.ludd.ltu.se/

## License

Same as PCC - BSD-style license (see parent directory for details)

## Authors

- Ruby front-end implementation: Generated with Claude Code
- PCC infrastructure: Anders Magnusson and contributors

## Status

**Alpha**: This is an experimental compiler. It demonstrates how to add a new language front-end to PCC but is not production-ready. Use for educational purposes and experimentation.
