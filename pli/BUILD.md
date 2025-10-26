# Building the PL/I Compiler

This document describes how to build, install, and test the PCC PL/I compiler.

## Prerequisites

### Required Tools

- **C Compiler**: GCC 4.x or later, or Clang 3.x or later
- **Make**: GNU Make 3.8 or later
- **Flex**: Lexical analyzer generator (2.5.x or later)
- **Bison/Yacc**: Parser generator (Bison 2.4 or later, or compatible Yacc)
- **Assembler**: GNU as or compatible
- **Linker**: GNU ld or compatible

### Optional Tools

- **ar**: For creating static libraries
- **ranlib**: For indexing archives
- **install**: For installation (GNU coreutils)

### System Requirements

- Linux, BSD, macOS, or other Unix-like OS
- At least 50MB disk space for build
- At least 100MB for installation

## Quick Start

```bash
# From the pli/ directory
cd pli

# Build everything
make

# Install (requires root/sudo)
sudo make install
```

## Detailed Build Instructions

### 1. Build the Runtime Library

```bash
cd libpli
make
```

This creates `libpli.a` containing all runtime functions:
- I/O operations
- Mathematical functions
- String manipulation
- Memory management
- Array operations
- Type conversions
- Condition handling
- PL/M-specific functions

### 2. Build the Compiler Proper

```bash
cd ../plicom
make
```

Build steps:
1. **Lexer generation**: `scan.l` → `scan.c` (via Flex)
2. **Parser generation**: `pligram.y` → `pligram.c` (via Bison/Yacc)
3. **Compilation**: Compile all C sources
4. **Linking**: Create `plicom` executable

This creates the `plicom` compiler which processes PL/I source to assembly.

### 3. Build the Driver

```bash
cd ../pli
make
```

This creates the `pli` driver program which manages the compilation pipeline.

### 4. Build Everything

From the `pli/` directory:

```bash
make
```

This builds all three components in order:
1. libpli (runtime library)
2. plicom (compiler proper)
3. pli (driver)

## Installation

### Standard Installation

```bash
# From pli/ directory
sudo make install
```

Default installation paths:
- `/usr/local/bin/pli` - Driver program
- `/usr/local/libexec/plicom` - Compiler proper
- `/usr/local/lib/libpli.a` - Runtime library
- `/usr/local/include/pli_runtime.h` - Runtime header
- `/usr/local/share/man/man1/pli.1` - Man page for pli
- `/usr/local/share/man/man1/plicom.1` - Man page for plicom

### Custom Installation Prefix

```bash
./configure --prefix=/opt/pcc
make
sudo make install
```

This installs to `/opt/pcc` instead of `/usr/local`.

## Configuration

### Configure Script Integration

When building from the top-level PCC directory:

```bash
cd /path/to/pcc
./configure --enable-pli
make all-full
sudo make install
```

This builds C, C++, Pascal, Fortran, and PL/I compilers.

### Dialect Configuration

The compiler supports multiple PL/I dialects configured at compile time. See `plicom/dialect.c` for supported dialects:

- PL/I (standard)
- PL/I Subset G
- PL/I Optimizing
- PL/M
- PL/M-86
- PL/M-386
- PL/C

## Testing

### Run Test Programs

```bash
cd tests

# Test basic functionality
../pli/pli -o hello hello.pli
./hello

# Test runtime library
../pli/pli -o runtime_test runtime_test.pli
./runtime_test

# Test string functions
../pli/pli -o string_test string_test.pli
./string_test

# Test memory management
../pli/pli -o memory_test memory_test.pli
./memory_test

# Test condition handling
../pli/pli -o condition_test condition_test.pli
./condition_test
```

### Test PL/M Dialect

```bash
../pli/pli -d plm86 -o blink plm_blink.plm
```

### Generate Assembly Only

```bash
../pli/pli -S -o test.s hello.pli
cat test.s
```

## Troubleshooting

### Build Errors

**Error**: `flex: command not found`
```bash
# Ubuntu/Debian
sudo apt-get install flex

# macOS
brew install flex
```

**Error**: `yacc: command not found` or `bison: command not found`
```bash
# Ubuntu/Debian
sudo apt-get install bison

# macOS
brew install bison
```

**Error**: `cannot find -lpli`
- Ensure `libpli/` was built successfully
- Check that `libpli.a` exists in `libpli/`
- Make sure library path includes build directory

### Runtime Errors

**Error**: `undefined reference to pli_*`
- Ensure linking with `-lpli`
- Check that `libpli.a` is installed in `/usr/local/lib`
- Add `-L/path/to/libpli` if using non-standard location

**Error**: `pli_runtime.h: No such file or directory`
- Install the runtime header: `sudo make install` from `libpli/`
- Or add `-I/path/to/libpli` to include path

## Development Build

For development and testing:

```bash
# Build with debug symbols
make CFLAGS="-g -O0"

# Build with warnings
make CFLAGS="-Wall -Wextra -g"

# Clean and rebuild
make clean
make
```

### Debugging

```bash
# Debug the compiler
gdb --args plicom/plicom -o test.s test.pli

# Debug generated code
pli -g -o test test.pli
gdb ./test
```

## Cross-Compilation

To build for a different architecture:

```bash
./configure --target=arm-linux-gnueabi
make
```

Supported targets include all PCC architectures:
- x86 (i386, i86)
- x86-64 (amd64)
- ARM
- MIPS
- PowerPC
- SPARC
- VAX
- M68K
- PDP-11, PDP-10
- WebAssembly

## Performance Optimization

### Compiler Optimization

```bash
# Build with optimization
make CFLAGS="-O2"

# Aggressive optimization
make CFLAGS="-O3 -march=native"
```

### Runtime Library Optimization

Edit `libpli/Makefile.in` to add optimization flags:

```makefile
CFLAGS = -O2 -march=native
```

Then rebuild:

```bash
cd libpli
make clean
make
```

## Cleaning

```bash
# Clean object files and executables
make clean

# Clean everything including generated files
make distclean
```

## Directory Structure

```
pli/
├── BUILD.md              # This file
├── README.md             # Overview
├── Makefile.in           # Top-level makefile
├── libpli/               # Runtime library
│   ├── Makefile.in
│   ├── pli_runtime.h     # Public API
│   ├── pli_io.c          # I/O functions
│   ├── pli_math.c        # Math functions
│   ├── pli_string.c      # String functions
│   ├── pli_memory.c      # Memory management
│   ├── pli_array.c       # Array operations
│   ├── pli_convert.c     # Type conversions
│   ├── pli_startup.c     # Initialization
│   ├── pli_plm.c         # PL/M functions
│   └── pli_conditions.c  # Exception handling
├── plicom/               # Compiler proper
│   ├── Makefile.in
│   ├── main.c            # Entry point
│   ├── scan.l            # Lexer
│   ├── pligram.y         # Parser
│   ├── pass1.h           # Frontend structures
│   ├── error.c/h         # Diagnostics
│   ├── dialect.c/h       # Dialect support
│   ├── symtab.c          # Symbol table
│   ├── types.c           # Type system
│   ├── builtins.c        # Built-in functions
│   └── codegen.c/h       # Code generation
├── pli/                  # Driver program
│   ├── Makefile.in
│   └── pli.c             # Driver implementation
├── tests/                # Test programs
│   ├── hello.pli
│   ├── factorial.pli
│   ├── fibonacci.pli
│   ├── array_demo.pli
│   ├── runtime_test.pli
│   ├── string_test.pli
│   ├── memory_test.pli
│   ├── condition_test.pli
│   └── plm_blink.plm
├── examples/             # Example programs
│   └── README.md
└── man/                  # Manual pages
    ├── pli.1
    └── plicom.1
```

## Integration with PCC

When building as part of the full PCC suite:

```bash
# From top-level pcc/ directory
./configure
make all-full    # Builds C, C++, Pascal, Fortran, PL/I
sudo make install
```

The PL/I compiler is integrated into the PCC build system and shares infrastructure with other compilers.

## Platform-Specific Notes

### Linux

Standard build should work. Ensure development packages are installed:

```bash
sudo apt-get install build-essential flex bison
```

### macOS

Use Homebrew for dependencies:

```bash
brew install flex bison
export PATH="/usr/local/opt/bison/bin:$PATH"
export PATH="/usr/local/opt/flex/bin:$PATH"
make
```

### FreeBSD/OpenBSD

```bash
pkg install gmake flex bison
gmake  # Use GNU make
```

### Windows (MinGW/Cygwin)

Install MinGW or Cygwin with development tools, then:

```bash
make
```

## See Also

- PL/I README (README.md)
- Runtime Library Documentation (libpli/README.md)
- Example Programs (examples/README.md)
- Manual Pages (man/pli.1, man/plicom.1)
- PCC Documentation (../README.md)

## Support

For build issues, see:
- GitHub Issues: https://github.com/pcc/pcc/issues
- PCC Mailing List
- Documentation: https://pcc.ludd.ltu.se/

## License

The PL/I compiler is part of the Portable C Compiler project and uses the same license.
