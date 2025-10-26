# PCC Multi-Stage Bootstrap Guide

This document describes the multi-stage bootstrapping process for the Portable C Compiler (PCC), including native builds, cross-compilation, and Canadian cross builds.

## Table of Contents

1. [Introduction](#introduction)
2. [Bootstrap Concepts](#bootstrap-concepts)
3. [Quick Start](#quick-start)
4. [Bootstrap Stages](#bootstrap-stages)
5. [Native Bootstrap](#native-bootstrap)
6. [Cross-Compilation Bootstrap](#cross-compilation-bootstrap)
7. [Canadian Cross Bootstrap](#canadian-cross-bootstrap)
8. [Bootstrap Verification](#bootstrap-verification)
9. [Troubleshooting](#troubleshooting)
10. [Advanced Usage](#advanced-usage)

## Introduction

Bootstrapping is the process of using an existing compiler to build a new compiler, then using that new compiler to rebuild itself. This process:

- Verifies that the compiler can compile itself correctly
- Ensures reproducible builds (stage N and stage N+1 should be identical)
- Validates cross-compilation capabilities
- Builds confidence in the compiler's correctness

PCC supports three types of bootstrapping:

1. **Native Bootstrap**: Build PCC on and for the same system
2. **Cross Bootstrap**: Build PCC on one system to target another
3. **Canadian Cross**: Build PCC on one system, to run on a second system, targeting a third

## Bootstrap Concepts

### System Triplets

A system triplet describes a platform: `CPU-VENDOR-OS` (e.g., `x86_64-unknown-linux-gnu`)

- **Build**: The system where compilation occurs
- **Host**: The system where the compiler will run
- **Target**: The system for which the compiler generates code

### Bootstrap Stages

- **Stage 0**: Built with an existing compiler (gcc, clang, etc.)
- **Stage 1**: Built using the Stage 0 compiler
- **Stage 2**: Built using the Stage 1 compiler (for verification)
- **Stage N**: Additional stages for thorough testing

## Quick Start

### Simple 3-Stage Native Bootstrap

```bash
# Using the bootstrap script
./bootstrap.sh --stages=3 --compare-stages

# Or using Make
make bootstrap
```

### Using an Existing Build

If you already have PCC configured:

```bash
make all
make bootstrap-quick    # 2-stage bootstrap
```

## Bootstrap Stages

### Stage 0: Bootstrap Stage

Stage 0 is built using your system's existing C compiler:

```bash
./configure --prefix=/usr/local/stage0
make
make install
```

**Compiler Used**: System compiler (gcc, clang, etc.)
**Output**: First PCC compiler binary
**Purpose**: Create initial working PCC

### Stage 1: First Self-Compilation

Stage 1 is built using the Stage 0 PCC compiler:

```bash
export CC=/usr/local/stage0/bin/pcc
./configure --prefix=/usr/local/stage1
make
make install
```

**Compiler Used**: Stage 0 PCC
**Output**: First self-compiled PCC
**Purpose**: Verify PCC can compile itself

### Stage 2: Verification Stage

Stage 2 is built using the Stage 1 PCC compiler:

```bash
export CC=/usr/local/stage1/bin/pcc
./configure --prefix=/usr/local/stage2
make
make install
```

**Compiler Used**: Stage 1 PCC
**Output**: Second self-compiled PCC
**Purpose**: Verify reproducibility (should be identical to Stage 1)

### Verification

Compare Stage 1 and Stage 2 binaries:

```bash
cmp /usr/local/stage1/bin/pcc /usr/local/stage2/bin/pcc
```

If they're identical (or differ only in debug info), the bootstrap succeeded!

## Native Bootstrap

Native bootstrap builds PCC on and for the same system.

### Method 1: Using bootstrap.sh

```bash
./bootstrap.sh \
  --stages=3 \
  --compare-stages \
  --jobs=8 \
  --prefix=/opt/pcc
```

**Options:**
- `--stages=N`: Number of stages to build (default: 3)
- `--compare-stages`: Compare successive stages for reproducibility
- `--jobs=N`: Parallel jobs (default: CPU count)
- `--prefix=DIR`: Installation prefix
- `--builddir=DIR`: Build directory (default: ./bootstrap-build)
- `--cc=COMPILER`: Initial compiler (default: gcc)

### Method 2: Manual Bootstrap

```bash
# Stage 0
mkdir stage0 && cd stage0
../configure --prefix=$PWD/install
make -j$(nproc)
make install
cd ..

# Stage 1
mkdir stage1 && cd stage1
CC=$PWD/../stage0/install/bin/pcc ../configure --prefix=$PWD/install
make -j$(nproc)
make install
cd ..

# Stage 2
mkdir stage2 && cd stage2
CC=$PWD/../stage1/install/bin/pcc ../configure --prefix=$PWD/install
make -j$(nproc)
make install
cd ..

# Compare
cmp stage1/install/bin/pcc stage2/install/bin/pcc
```

### Method 3: Using Make Targets

```bash
# Configure first
./configure

# Then bootstrap
make bootstrap          # 3-stage with verification
make bootstrap-quick    # 2-stage, faster
make bootstrap-full     # 3-stage with full comparison
```

## Cross-Compilation Bootstrap

Cross-compilation builds a compiler that runs on one system but generates code for another.

**Example**: Build on x86_64 Linux, run on ARM Linux

### Using bootstrap.sh

```bash
./bootstrap.sh \
  --build=x86_64-linux-gnu \
  --host=arm-linux-gnueabihf \
  --target=arm-linux-gnueabihf \
  --stages=2
```

**Stage 0**: Cross-compiler (runs on x86_64, targets ARM)
**Stage 1**: Native ARM compiler (would need to run on ARM to verify)

### Manual Cross-Compilation

```bash
# Install ARM cross-toolchain first
sudo apt-get install gcc-arm-linux-gnueabihf

# Stage 0: Build cross-compiler
mkdir cross-stage0 && cd cross-stage0
../configure \
  --build=x86_64-linux-gnu \
  --host=arm-linux-gnueabihf \
  --target=arm-linux-gnueabihf \
  --prefix=$PWD/install
make -j$(nproc)
make install
cd ..

# Stage 1: Build on ARM (transfer files to ARM system)
# scp -r . arm-system:pcc/
# ssh arm-system
# cd pcc
mkdir arm-stage1 && cd arm-stage1
CC=$PWD/../cross-stage0/install/bin/pcc ../configure --prefix=$PWD/install
make -j$(nproc)
make install
```

## Canadian Cross Bootstrap

Canadian cross builds a compiler on one system (build), that runs on another (host), and generates code for a third (target).

**Example**: Build on x86_64 Linux, run on ARM Linux, generate MIPS code

### Using bootstrap.sh

```bash
./bootstrap.sh \
  --build=x86_64-linux-gnu \
  --host=arm-linux-gnueabihf \
  --target=mips-linux-gnu \
  --stages=2
```

**Requirements**:
- Cross-toolchain for host (arm-linux-gnueabihf)
- Cross-toolchain for target (mips-linux-gnu)

### Manual Canadian Cross

```bash
# Install cross-toolchains
sudo apt-get install \
  gcc-arm-linux-gnueabihf \
  gcc-mips-linux-gnu

# Stage 0: Build cross-compiler (x86_64 -> ARM, targeting MIPS)
mkdir canadian-stage0 && cd canadian-stage0
../configure \
  --build=x86_64-linux-gnu \
  --host=arm-linux-gnueabihf \
  --target=mips-linux-gnu \
  --prefix=$PWD/install \
  CC_FOR_BUILD=gcc \
  CC=arm-linux-gnueabihf-gcc
make -j$(nproc)
make install
cd ..
```

## Bootstrap Verification

### Automatic Verification

The bootstrap script automatically verifies builds when using `--compare-stages`:

```bash
./bootstrap.sh --stages=3 --compare-stages
```

This compares:
- Stage 0 vs Stage 1
- Stage 1 vs Stage 2

### Manual Verification

```bash
# Byte-by-byte comparison
cmp stage1/install/bin/pcc stage2/install/bin/pcc

# If they differ, try comparing without debug info
cp stage1/install/bin/pcc pcc1
cp stage2/install/bin/pcc pcc2
strip pcc1 pcc2
cmp pcc1 pcc2

# Detailed differences
objdump -d stage1/install/bin/pcc > stage1.asm
objdump -d stage2/install/bin/pcc > stage2.asm
diff -u stage1.asm stage2.asm
```

### Expected Results

**Success**: Binaries are identical (or differ only in timestamps/debug info)
**Failure**: Binaries differ in code/data sections

Common reasons for failure:
- Non-deterministic code generation
- Time-based values embedded in binary
- Compiler bugs affecting self-compilation
- Environmental differences between stages

## Troubleshooting

### Bootstrap Fails at Stage 0

**Problem**: System compiler cannot build PCC

**Solutions**:
- Ensure you have a working C compiler (gcc, clang)
- Install required dependencies: `make`, `yacc/bison`, `lex/flex`
- Check for missing headers or libraries
- Try: `./configure --disable-tls --disable-gcc-compat`

### Stage 1 Differs from Stage 2

**Problem**: Reproducibility check fails

**Solutions**:
- Check if only debug info differs: `strip` both and compare
- Look for timestamp or build path embeddings
- Verify no environment changes between stages
- Use: `./bootstrap.sh --keep-going` to continue despite differences

### Cross-Compilation Fails

**Problem**: Cannot build for target platform

**Solutions**:
- Ensure cross-toolchain is installed
- Check triplet names: `config.guess`, `config.sub`
- Set `CC_FOR_BUILD` explicitly: `CC_FOR_BUILD=gcc`
- Verify assembler/linker for target: `--with-assembler=...`

### Out of Memory During Bootstrap

**Problem**: System runs out of memory

**Solutions**:
- Reduce parallel jobs: `--jobs=1` or `--jobs=2`
- Increase swap space
- Build stages sequentially instead of in parallel
- Use: `make bootstrap-quick` for fewer stages

### Permission Denied

**Problem**: Cannot write to installation directory

**Solutions**:
- Use a directory you own: `--prefix=$HOME/pcc`
- Run install with sudo: `sudo make install` (after building)
- Change ownership: `sudo chown -R $USER /usr/local/stageN`

## Advanced Usage

### Custom Bootstrap Compiler

Use a specific compiler for Stage 0:

```bash
./bootstrap.sh --cc=/opt/gcc-14/bin/gcc
```

### Different Prefixes Per Stage

```bash
./bootstrap.sh \
  --stages=3 \
  --prefix=/opt/pcc \
  --builddir=$HOME/pcc-bootstrap
```

Stages will be installed in:
- `/opt/pcc/stage0`
- `/opt/pcc/stage1`
- `/opt/pcc/stage2`

### Verbose Bootstrap

```bash
./bootstrap.sh --stages=3 --verbose --compare-stages
```

### Clean Bootstrap

```bash
# Clean before starting
./bootstrap.sh --clean --stages=3

# Or manually
make bootstrap-clean
rm -rf bootstrap-build
```

### Four-Stage Bootstrap

For extra confidence:

```bash
./bootstrap.sh --stages=4 --compare-stages
```

This compares:
- Stage 0 vs Stage 1
- Stage 1 vs Stage 2
- Stage 2 vs Stage 3

### Integration with Configure

Build with specific options:

```bash
# Edit bootstrap.sh or:
mkdir stage0 && cd stage0
../configure \
  --enable-tls \
  --enable-twopass \
  --with-assembler=yasm \
  --prefix=$PWD/install
make && make install
# Continue manually for stage1, stage2...
```

### Testing After Bootstrap

```bash
# Use final stage compiler
export PATH=/usr/local/stage2/bin:$PATH

# Test compilation
echo 'int main() { return 0; }' > test.c
pcc test.c -o test
./test

# Run test suite
make test
```

## Bootstrap Best Practices

1. **Always verify**: Use `--compare-stages` for production builds
2. **Use 3 stages minimum**: Stage 0 → 1 → 2 is the sweet spot
3. **Clean builds**: Use `--clean` or fresh directories
4. **Document**: Record compiler versions, flags, and results
5. **Test thoroughly**: Run test suite after bootstrap
6. **Save artifacts**: Keep stage binaries for debugging

## Bootstrap Performance

Typical bootstrap times (on modern hardware):

| Stages | Time (8 cores) | Time (1 core) |
|--------|----------------|---------------|
| 2      | 5-10 minutes   | 30-40 minutes |
| 3      | 10-15 minutes  | 50-70 minutes |
| 4      | 15-20 minutes  | 70-90 minutes |

Times vary based on:
- CPU speed and core count
- Disk I/O performance
- System load
- PCC configuration options

## References

- [GCC Bootstrap Documentation](https://gcc.gnu.org/install/build.html)
- [Autoconf Cross-Compilation](https://www.gnu.org/software/autoconf/manual/autoconf-2.69/html_node/Specifying-Target-Triplets.html)
- [Reproducible Builds](https://reproducible-builds.org/)

## See Also

- `README.md` - General PCC documentation
- `configure --help` - Configuration options
- `./bootstrap.sh --help` - Bootstrap script options
