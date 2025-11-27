# Multi-Architecture Testing Guide for PCC

This guide covers testing PCC across multiple architectures and platforms, including Windows (via MinGW), Wine execution, and 32-bit (i386) builds.

## Overview

PCC's bootstrap system supports testing on:

- **MinGW x86_64**: Windows 64-bit cross-compilation
- **MinGW i686**: Windows 32-bit cross-compilation  
- **Wine**: Running Windows binaries on Linux
- **i386 Native**: 32-bit Linux builds
- **Multi-arch**: Debian/Ubuntu multi-arch configurations

## Quick Start

### 1. Install Dependencies

```bash
# Run as root or with sudo
sudo ./setup-multiarch-deps.sh
```

This installs:
- MinGW cross-compilers (x86_64 and i686)
- Wine (wine, wine32, wine64)
- GCC multilib support for 32-bit builds
- Additional build tools

Supported distributions:
- Ubuntu/Debian
- Fedora/RHEL/CentOS
- Arch Linux

### 2. Run Multi-Arch Tests

```bash
./test-multiarch.sh
```

This automatically tests all supported configurations.

## Detailed Testing Procedures

### MinGW Cross-Compilation (Windows)

#### MinGW x86_64 (64-bit Windows)

```bash
# Configure
CC=x86_64-w64-mingw32-gcc \
AR=x86_64-w64-mingw32-ar \
RANLIB=x86_64-w64-mingw32-ranlib \
./configure --host=x86_64-w64-mingw32

# Build
make

# The resulting binaries will be .exe files
file cc/cc/cc.exe
# Output: cc/cc/cc.exe: PE32+ executable (console) x86-64
```

#### MinGW i686 (32-bit Windows)

```bash
# Configure
CC=i686-w64-mingw32-gcc \
AR=i686-w64-mingw32-ar \
RANLIB=i686-w64-mingw32-ranlib \
./configure --host=i686-w64-mingw32

# Build
make

# The resulting binaries will be 32-bit .exe files
file cc/cc/cc.exe
# Output: cc/cc/cc.exe: PE32 executable (console) Intel 80386
```

### Wine Testing

Wine allows running Windows binaries on Linux without Windows.

#### Testing MinGW x86_64 with Wine64

```bash
# Build for Windows first
CC=x86_64-w64-mingw32-gcc ./configure --host=x86_64-w64-mingw32
make

# Test with Wine
cat > test.c << 'EOF'
#include <stdio.h>
int main(void) {
    printf("Hello from Windows on Linux!\n");
    return 0;
}
EOF

# Compile with PCC Windows binary
wine64 cc/cc/cc.exe -o test.exe test.c

# Run the result
wine64 test.exe
# Output: Hello from Windows on Linux!
```

#### Testing MinGW i686 with Wine32

```bash
# Build for Windows 32-bit
CC=i686-w64-mingw32-gcc ./configure --host=i686-w64-mingw32
make

# Test with Wine
wine cc/cc/cc.exe -o test.exe test.c
wine test.exe
```

### i386 Native (32-bit Linux)

Build and run PCC as a 32-bit Linux application.

```bash
# Configure for i386
CFLAGS="-m32" LDFLAGS="-m32" \
./configure --build=i686-pc-linux-gnu

# Build
make

# Verify binary is 32-bit
file cc/cc/cc
# Output: cc/cc/cc: ELF 32-bit LSB executable, Intel 80386

# Test compilation
./cc/cc/cc -o test test.c
file test
# Output: test: ELF 32-bit LSB executable

# Run
./test
```

### Multi-Arch Configuration

For Debian/Ubuntu multi-arch systems:

```bash
# Configure with multi-arch support
./configure --enable-multiarch

# This automatically detects paths like:
# /usr/lib/x86_64-linux-gnu
# /usr/include/x86_64-linux-gnu

# Build
make

# The compiler will find libraries in multi-arch paths
./cc/cc/cc -v test.c
```

## Bootstrap with Multi-Arch

### MinGW Bootstrap

```bash
# Stage 0: Build with native compiler
./configure
make all-c
make install-c prefix=/tmp/pcc-stage0

# Stage 1: Cross-compile with stage 0
CC=/tmp/pcc-stage0/bin/pcc \
./configure --host=x86_64-w64-mingw32
make
```

### i386 Bootstrap

```bash
# Stage 0: Build for i386
CFLAGS="-m32" LDFLAGS="-m32" ./configure
make all-c
make install-c prefix=/tmp/pcc-i386-stage0

# Stage 1: Build with stage 0 i386 compiler
CC=/tmp/pcc-i386-stage0/bin/pcc ./configure
make
```

## Test Matrix

The `test-multiarch.sh` script tests these configurations:

| Test | Platform | Architecture | Execution |
|------|----------|--------------|-----------|
| mingw_x86_64 | Windows | 64-bit | Wine64 |
| mingw_i686 | Windows | 32-bit | Wine |
| i386_native | Linux | 32-bit | Native |
| multiarch | Linux | 64-bit | Native |

Each test performs:
1. Configure
2. Build
3. Basic compilation test
4. Runtime execution (if applicable)

## Troubleshooting

### MinGW Issues

**Problem**: MinGW compiler not found
```bash
# Ubuntu/Debian
sudo apt-get install gcc-mingw-w64 g++-mingw-w64

# Fedora
sudo dnf install mingw64-gcc mingw32-gcc
```

**Problem**: Headers not found during MinGW build
```bash
# Make sure mingw-w64 development packages are installed
sudo apt-get install mingw-w64-tools mingw-w64-common
```

### Wine Issues

**Problem**: Wine not found or not working
```bash
# Ubuntu/Debian
sudo dpkg --add-architecture i386
sudo apt-get update
sudo apt-get install wine wine32 wine64

# Initialize Wine
wine wineboot --init
```

**Problem**: Wine execution fails with missing DLLs
```bash
# Wine may need to install components on first run
# Just wait for the installation to complete
wine --version
```

### i386/Multilib Issues

**Problem**: Cannot find 32-bit libraries
```bash
# Ubuntu/Debian
sudo apt-get install gcc-multilib g++-multilib

# Fedora
sudo dnf install glibc-devel.i686 libgcc.i686
```

**Problem**: `/usr/include/gnu/stubs-32.h` not found
```bash
# Ubuntu
sudo apt-get install libc6-dev-i386

# Fedora
sudo dnf install glibc-devel.i686
```

## CI/CD Integration

Example GitHub Actions workflow:

```yaml
name: Multi-Arch Tests

on: [push, pull_request]

jobs:
  test-multiarch:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup dependencies
        run: |
          sudo dpkg --add-architecture i386
          sudo apt-get update
          sudo apt-get install -y \
            gcc-mingw-w64 g++-mingw-w64 \
            wine wine32 wine64 \
            gcc-multilib g++-multilib
      
      - name: Run multi-arch tests
        run: ./test-multiarch.sh
      
      - name: Upload test logs
        if: always()
        uses: actions/upload-artifact@v3
        with:
          name: test-logs
          path: test-multiarch/logs/
```

## Performance Notes

- **MinGW builds**: Similar performance to native Linux builds
- **Wine execution**: 10-20% slower than native Windows
- **i386 builds**: Comparable to x86_64 on modern CPUs
- **Cross-compilation**: Much faster than emulation (QEMU)

## Platform-Specific Notes

### Windows (MinGW)

- No POSIX functions (`fork`, `exec`, etc.) - use Windows APIs
- Different path separators (`\` vs `/`)
- Case-insensitive filesystems
- Different executable format (.exe)

### Wine Limitations

- Not all Windows APIs fully implemented
- Some features may behave differently
- Console I/O usually works well
- Complex GUI applications may have issues

### i386 Limitations

- Only 4GB address space (practical limit ~3GB)
- Fewer registers available
- May require `-m32` flag for dependencies

## References

- MinGW-w64: https://www.mingw-w64.org/
- Wine: https://www.winehq.org/
- Debian Multi-Arch: https://wiki.debian.org/Multiarch
- GCC Multilib: https://gcc.gnu.org/onlinedocs/gcc/Submodel-Options.html

## Summary

Multi-arch testing ensures PCC works across:
- ✅ Different operating systems (Linux, Windows)
- ✅ Different architectures (x86_64, i686)
- ✅ Different environments (native, Wine, cross-compilation)
- ✅ Different configurations (multilib, multi-arch)

This comprehensive testing increases confidence in PCC's portability and correctness.
