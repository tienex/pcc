# PCSC - Portable C# Compiler

## Quick Start

```bash
# Compile with latest C# (12.0)
pcsc program.cs

# Compile with specific version
pcsc -langversion:7.3 program.cs

# Show available features for a version
pcsc -langversion:8.0 --features

# Compile with verbose output
pcsc -v -langversion:latest program.cs
```

## Features

- ✅ **All C# Versions**: Support for C# 1.0 through 12.0
- ✅ **Version-Specific Validation**: Features enforced based on selected version
- ✅ **Shared ARC**: Automatic reference counting for reference types
- ✅ **Architecture Neutral**: Compile to platform-independent IL
- ✅ **Endian Neutral**: Portable module format for all platforms

## Supported Versions

| Version | Key Features |
|---------|--------------|
| C# 1.0-2.0 | Generics, nullable types, iterators |
| C# 3.0 | LINQ, lambdas, extension methods |
| C# 4.0-5.0 | Dynamic binding, async/await |
| C# 6.0-7.x | Null-conditional, tuples, pattern matching |
| C# 8.0-9.0 | Nullable refs, records, init-only |
| C# 10.0-12.0 | Global usings, primary constructors |

## Installation

```bash
./configure
make
make install
```

Installs:
- `/usr/local/bin/pcsc` - Compiler driver
- `/usr/local/libexec/cscom` - Compiler frontend

## Usage Examples

```bash
# Compile as C# 2.0 (generics era)
pcsc -langversion:2.0 program.cs

# Compile as C# 7.3 for x86-64
pcsc -langversion:7.3 --arch=x86-64 program.cs

# Show C# 8.0 features
pcsc -langversion:8.0 --features

# Compile with ARC disabled
pcsc -fno-arc program.cs

# Verbose compilation
pcsc -v -o myapp.csm program.cs
```

## Documentation

- **[CSHARP_COMPILER.md](CSHARP_COMPILER.md)** - Complete compiler documentation
- **[CSHARP_VERSIONS.md](CSHARP_VERSIONS.md)** - Version features and migration guide
- **[ARC_LIBRARY.md](ARC_LIBRARY.md)** - ARC integration details

## Test Suite

See `csharp/tests/` for version-specific examples:
- `test_csharp20.cs` - C# 2.0 features
- `test_csharp50.cs` - C# 5.0 async/await
- `test_csharp70.cs` - C# 7.0 tuples and pattern matching
- `test_csharp90.cs` - C# 9.0 records

Run tests:
```bash
cd csharp/tests
pcsc -langversion:2.0 -v test_csharp20.cs
pcsc -langversion:5.0 -v test_csharp50.cs
pcsc -langversion:7.0 -v test_csharp70.cs
```

## Command-Line Options

```
-o <file>         Output file (default: output.csm)
-m <name>         Module name
-langversion:<v>  C# language version (1.0-12.0, default: latest)
-O <level>        Optimization level (0-3)
-farc             Enable ARC (default)
-fno-arc          Disable ARC
-g                Emit debug information
-v                Verbose output
--features        Show available language features
--arch=<arch>     Target architecture
--endian=<end>    Target endianness
--version         Print version
--help            Print help
```

## Architecture

PCSC compiles C# source code to architecture-neutral modules that can be instantiated on any platform:

```
Source (.cs) → [Lexer] → [Parser] → [Semantic Analysis] → [Code Gen]
                                                              ↓
                                                      Module (.csm)
                                           (Architecture & Endian Neutral)
```

## License

BSD License (see COPYING file)

## Project

Part of the Portable C Compiler (PCC) project
- Website: http://pcc.ludd.ltu.se/
- Repository: https://github.com/tienex/pcc
