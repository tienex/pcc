# C# Compiler (C# 1.0 - 12.0)

## Quick Start

```bash
# Compile with latest C# (12.0)
csharp program.cs

# Compile with specific version
csharp -langversion:7.3 program.cs

# Show available features for a version
csharp -langversion:8.0 --features

# Compile with verbose output
csharp -v -langversion:latest program.cs
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

## Documentation

- **[CSHARP_COMPILER.md](CSHARP_COMPILER.md)** - Complete compiler documentation
- **[CSHARP_VERSIONS.md](CSHARP_VERSIONS.md)** - Version features and migration guide
- **[ARC_LIBRARY.md](ARC_LIBRARY.md)** - ARC integration details

## Examples

See `csharp/tests/` for version-specific examples:
- `test_csharp20.cs` - C# 2.0 features
- `test_csharp50.cs` - C# 5.0 async/await
- `test_csharp70.cs` - C# 7.0 tuples and pattern matching
- `test_csharp90.cs` - C# 9.0 records

## Building

```bash
./configure
make
make install
```

Installs:
- `/usr/local/bin/csharp` - Compiler driver
- `/usr/local/libexec/cscom` - Compiler frontend
