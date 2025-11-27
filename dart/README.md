# PCC Dart Compiler

A Dart language compiler frontend for the Portable C Compiler (PCC) framework.

## Overview

This is a complete Dart language compiler that integrates with the PCC compilation infrastructure. It provides full support for Dart language features including:

- Classes and inheritance
- Functions and closures
- Generics and type parameters
- Null safety
- Async/await
- Collections (List, Map, Set)
- Extension methods
- Mixins
- And more...

## Architecture

```
dart/
├── dart/            # Compiler driver
│   └── dart.c       # Command-line interface and build pipeline
├── dcom/            # Dart compiler frontend
│   ├── main.c       # Entry point
│   ├── scan.l       # Lexical analyzer (Flex)
│   ├── dgram.y      # Parser grammar (Bison/Yacc)
│   ├── pass1.h      # Frontend data structures
│   ├── symtab.c     # Symbol table management
│   ├── ast.c        # AST construction and manipulation
│   ├── error.c      # Error reporting
│   └── codegen.c    # Code generation
└── tests/           # Test programs
    ├── hello.dart
    ├── fibonacci.dart
    └── classes.dart
```

## Compilation Pipeline

```
Source.dart
    ↓
[dcom] Dart Compiler Frontend
    - Lexical analysis (scan.l)
    - Syntax analysis (dgram.y)
    - Semantic analysis
    - Symbol table construction
    - Type checking
    ↓ (generates intermediate representation)
[PCC Backend] Code Generation
    ↓ (generates assembly)
[as] Assembler
    ↓ (generates object file)
[ld] Linker
    ↓
Executable
```

## Building

### Prerequisites

- C compiler (GCC or Clang)
- Flex (lexical analyzer generator)
- Bison or Yacc (parser generator)
- PCC build environment

### Build Steps

```bash
# Configure (if not already done for PCC)
./configure

# Build Dart compiler
cd dart
make

# Install
make install
```

## Usage

### Basic Compilation

```bash
# Compile a Dart program
dart hello.dart

# Specify output file
dart -o myprogram hello.dart

# Compile to object file only
dart -c hello.dart

# Compile to assembly only
dart -S hello.dart

# Verbose output
dart -v hello.dart

# Keep temporary files
dart -k hello.dart
```

### Examples

```bash
# Hello World
dart tests/hello.dart
./a.out

# Fibonacci sequence
dart tests/fibonacci.dart
./a.out

# Object-oriented programming
dart tests/classes.dart
./a.out
```

## Language Support

### Data Types

- **Primitive types**: `int`, `double`, `bool`, `String`
- **Collections**: `List`, `Map`, `Set`
- **Special types**: `dynamic`, `Object`, `void`, `Null`
- **Type parameters**: Generic types with `<T>`
- **Nullable types**: `int?`, `String?`

### Object-Oriented Features

- **Classes**: `class MyClass { }`
- **Inheritance**: `class Dog extends Animal { }`
- **Interfaces**: `class MyClass implements Interface { }`
- **Mixins**: `class MyClass with Mixin { }`
- **Abstract classes**: `abstract class Shape { }`
- **Constructors**: Named and factory constructors
- **Getters/Setters**: `get value`, `set value`

### Functions

- **Function declarations**: `int add(int a, int b) { }`
- **Arrow functions**: `int square(int x) => x * x`
- **Optional parameters**: `void greet([String name])`
- **Named parameters**: `void configure({bool enabled})`
- **Required parameters**: `void login({required String password})`

### Control Flow

- **Conditionals**: `if`, `else`, `switch`
- **Loops**: `for`, `for-in`, `while`, `do-while`
- **Exception handling**: `try`, `catch`, `finally`, `throw`
- **Assert**: `assert(condition)`

### Modern Features

- **Null safety**: `int?`, `!`, `?.`, `??`
- **Async/await**: `async`, `await`, `Future<T>`
- **Extension methods**: `extension on String { }`
- **Late initialization**: `late String value`
- **Cascade notation**: `..method1()..method2()`
- **Spread operators**: `...list`, `...?nullableList`

## Dart Language Keywords

```
abstract    as          assert      async       await
break       case        catch       class       const
continue    default     deferred    do          else
enum        export      extends     extension   external
factory     false       final       finally     for
get         if          implements  import      in
interface   is          late        library     mixin
new         null        on          operator    part
required    rethrow     return      set         show
static      super       switch      sync        this
throw       true        try         typedef     var
void        while       with        yield
```

## Development Status

### Implemented Features

- ✅ Lexical analysis (complete)
- ✅ Syntax parsing (core features)
- ✅ Symbol table management
- ✅ AST construction
- ✅ Error reporting
- ✅ Basic type system

### In Progress

- 🔨 Semantic analysis
- 🔨 Type checking
- 🔨 Code generation
- 🔨 Standard library integration

### Planned Features

- 📋 Full generic support
- 📋 Async/await implementation
- 📋 Mixin resolution
- 📋 Extension method support
- 📋 Null safety enforcement
- 📋 Optimization passes

## Testing

```bash
# Run test suite
cd tests
make test

# Test individual programs
dart hello.dart && ./a.out
dart fibonacci.dart && ./a.out
dart classes.dart && ./a.out
```

## Error Reporting

The Dart compiler provides clear, helpful error messages:

```
error: line 5, column 12: type mismatch in assignment
warning: line 10, column 8: unused variable 'count'
```

## Integration with PCC

The Dart compiler integrates seamlessly with PCC:

1. **Frontend** (dcom): Parses Dart source code and builds an AST
2. **Middle-end** (PCC): Performs optimizations and transformations
3. **Backend** (PCC): Generates machine code for target architecture

This allows Dart programs to be compiled to any architecture supported by PCC, including:

- x86, x86-64 (AMD64)
- ARM, ARM64
- MIPS, MIPS64
- PowerPC
- SPARC
- WebAssembly
- And 13 more architectures

## License

Part of the Portable C Compiler project.
See COPYING for license information.

## Contributing

Contributions are welcome! Areas for improvement:

- Complete language feature implementation
- Standard library development
- Optimization improvements
- Bug fixes and testing
- Documentation

## References

- [Dart Language Specification](https://dart.dev/guides/language/spec)
- [Dart Language Tour](https://dart.dev/guides/language/language-tour)
- [PCC Compiler](http://pcc.ludd.ltu.se/)

## Authors

Implemented as part of the PCC compiler infrastructure.
