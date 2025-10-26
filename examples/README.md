# PCC Bootstrap Examples

This directory contains example scripts demonstrating various bootstrap scenarios for the Portable C Compiler.

## Examples

### 1. Native Bootstrap (`bootstrap-native.sh`)

Demonstrates a standard 3-stage native bootstrap:
- Stage 0: Build PCC with system compiler (gcc/clang)
- Stage 1: Build PCC with Stage 0 PCC
- Stage 2: Build PCC with Stage 1 PCC (verification)

**Usage:**
```bash
./examples/bootstrap-native.sh
```

**No prerequisites required** - uses your system's default C compiler.

### 2. Cross-Compilation Bootstrap (`bootstrap-cross.sh`)

Demonstrates building PCC as a cross-compiler for ARM:
- Builds on x86_64
- Runs on ARM (aarch64 or armhf)
- Generates ARM code

**Prerequisites:**
```bash
sudo apt-get install gcc-arm-linux-gnueabihf
```

**Usage:**
```bash
./examples/bootstrap-cross.sh
```

### 3. Canadian Cross Bootstrap (`bootstrap-canadian.sh`)

Demonstrates the complex "Canadian Cross" scenario:
- Build system: x86_64 (where compilation happens)
- Host system: aarch64 (where the compiler will run)
- Target system: MIPS (what the compiler generates code for)

**Prerequisites:**
```bash
sudo apt-get install gcc-aarch64-linux-gnu gcc-mips-linux-gnu
```

**Usage:**
```bash
./examples/bootstrap-canadian.sh
```

## Understanding the Examples

Each example script:
1. Verifies prerequisites (cross-compilers if needed)
2. Runs the appropriate bootstrap command
3. Shows where the resulting compiler is installed
4. Provides instructions for testing the compiler

## Customizing Examples

You can modify these examples to:
- Change target architectures
- Adjust number of stages
- Set different installation prefixes
- Enable/disable stage comparison
- Use different system compilers

Example customizations:

```bash
# Use clang instead of gcc
./bootstrap.sh --cc=clang --stages=3 --compare-stages

# Target different architecture
./bootstrap.sh --host=powerpc-linux-gnu --stages=2

# More verbose output
./bootstrap.sh --stages=3 --verbose --compare-stages

# Keep going despite comparison failures
./bootstrap.sh --stages=3 --compare-stages --keep-going
```

## See Also

- [../BOOTSTRAP.md](../BOOTSTRAP.md) - Complete bootstrap documentation
- [../README.md](../README.md) - General PCC documentation
- `../bootstrap.sh --help` - Full bootstrap script options
