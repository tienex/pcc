# Testing C11 Support in PCC

## Overview

PCC includes comprehensive C11 support with automatic detection and portable fallback libraries. This document explains how to test the C11 features.

## Test Programs

Two test programs are provided to verify C11 Unicode and threading support:

### 1. test_unicode.c - C11 Unicode Support Test

Tests the `<uchar.h>` implementation:
- UTF-8 to UTF-32 conversion (`mbrtoc32`)
- UTF-32 to UTF-8 conversion (`c32rtomb`)
- UTF-16 conversion functions (`mbrtoc16`, `c16rtomb`)
- Round-trip conversions

**Compile and run:**
```bash
# On systems with native C11 support:
gcc -std=c11 test_unicode.c -o test_unicode
./test_unicode

# On systems using PCC with portable libunicode:
pcc test_unicode.c -o test_unicode
./test_unicode
```

**Expected output:**
```
Testing C11 Unicode support (uchar.h)
Input UTF-8 string: Hello, World!
Converting to UTF-32 codepoints:
  Codepoint 1: U+0048 ('H')
  Codepoint 2: U+0065 ('e')
  ...
  Codepoint 13: U+0021 ('!')

Testing round-trip conversion:
  UTF-32 U+0041 -> UTF-8: "A" (1 bytes)

Testing UTF-16 conversion:
  'A' -> UTF-16: 0x0041
  UTF-16 0x0041 -> UTF-8: "A"

C11 Unicode test passed!
```

### 2. test_threads.c - C11 Threading Support Test

Tests the `<threads.h>` implementation:
- Thread creation and joining
- Mutex operations
- Thread synchronization
- Concurrent counter updates

**Compile and run:**
```bash
# On systems with native C11 support:
gcc -std=c11 test_threads.c -o test_threads -lpthread
./test_threads

# On systems using PCC with portable libthread:
pcc test_threads.c -o test_threads
./test_threads
```

**Expected output:**
```
Testing C11 threading support (threads.h)
Creating 4 threads, each incrementing counter 10000 times

Thread 1 starting
Thread 1 finished: incremented counter 10000 times
...
Thread 4 finished: incremented counter 10000 times

Expected counter value: 40000
Actual counter value:   40000

C11 threading test PASSED!
```

## Build System Testing

### Check C11 Detection

Run configure and check the output:

```bash
./configure
```

Look for these lines in the output:
```
C11 support ...................... yes/no
Build portable Unicode library ... yes/no
Build portable Threading library . yes/no
```

### Check Generated Config

After running configure, check `config.h`:

```bash
grep -E "NEED_LIB|HAVE_UCHAR|HAVE_THREADS" config.h
```

**On systems WITH native C11 support:**
```c
#define HAVE_THREADS_H 1
#define HAVE_UCHAR_H 1
/* #undef NEED_LIBTHREAD */
/* #undef NEED_LIBUNICODE */
```

**On systems WITHOUT native C11 support:**
```c
/* #undef HAVE_THREADS_H */
/* #undef HAVE_UCHAR_H */
#define NEED_LIBTHREAD 1
#define NEED_LIBUNICODE 1
```

### Verify Automatic Linking

When the portable libraries are built, they should be automatically linked by the PCC driver.

Check that the driver includes the library flags:

```bash
# Build the driver
make -C cc/driver

# On a system needing the portable libraries, the driver will add:
# -lunicode (if NEED_LIBUNICODE is defined)
# -lthread -lpthread (if NEED_LIBTHREAD is defined)
```

This happens automatically in `cc/driver/platform.c:init_platform_specific()`

## Manual Library Testing

### Test libunicode Directly

```bash
cd libunicode
make

# Compile a test program
gcc -std=c11 -I. ../test_unicode.c uchar.o -o test_direct
./test_direct
```

### Test libthread Directly

```bash
cd libthread
make

# Compile a test program
gcc -std=c11 -I. ../test_threads.c threads.o -lpthread -o test_direct
./test_direct
```

## Testing on Different Systems

### Linux (glibc 2.16+)

Modern Linux systems typically have native C11 support:
```bash
./configure
# Should show: Build portable Unicode library ... no
#             Build portable Threading library . no
```

### Linux (older glibc)

Older systems may lack C11 headers:
```bash
./configure
# May show: Build portable Unicode library ... yes
#          Build portable Threading library . yes
```

### FreeBSD/OpenBSD/NetBSD

BSD systems vary in C11 support:
```bash
./configure --host=<your-bsd-target>
```

### Windows (MinGW)

```bash
./configure --host=i686-w64-mingw32
# libthread will use Windows threading API
```

## Verifying Installation

After `make install`:

### Check Installed Headers

```bash
ls -l /usr/local/include/uchar.h
ls -l /usr/local/include/threads.h
```

On systems needing the portable libraries, these should be installed.

### Check Installed Libraries

```bash
ls -l /usr/local/lib/libunicode.a
ls -l /usr/local/lib/libthread.a
```

On systems needing the portable libraries, these should be installed.

### Test Compiler Integration

Create a simple test:

```bash
cat > test.c << 'EOF'
#include <threads.h>
int main(void) {
    mtx_t m;
    mtx_init(&m, mtx_plain);
    mtx_destroy(&m);
    return 0;
}
EOF

pcc test.c -o test
./test
echo $?  # Should print 0
```

The compiler should automatically link libthread without needing `-lthread` on the command line (on systems where it's needed).

## Troubleshooting

### Libraries Not Built

**Problem:** Configure says libraries are not needed but you want to test them anyway.

**Solution:** Manually build the libraries:
```bash
cd libunicode && make && cd ..
cd libthread && make && cd ..
```

### Linking Errors

**Problem:** `undefined reference to 'mbrtoc32'` or similar

**Solutions:**
1. Check that config.h has correct NEED_* defines
2. Verify libraries are installed: `ls -l /usr/local/lib/lib*.a`
3. Manually add library flags: `pcc -lunicode -lthread test.c`

### Header Not Found

**Problem:** `uchar.h: No such file or directory`

**Solutions:**
1. Check installation: `ls /usr/local/include/uchar.h`
2. Manually specify include path: `pcc -I/usr/local/include test.c`

### Thread Test Fails

**Problem:** Thread test shows incorrect counter value

**Possible causes:**
1. Pthread library not linked (should be automatic)
2. Mutex implementation issue
3. Try: `pcc test_threads.c -lthread -lpthread -o test`

## Performance Testing

For performance comparison between native and portable implementations:

```bash
# Time the thread test
time ./test_threads

# Compare with native implementation if available
gcc -std=c11 test_threads.c -pthread -o test_native
time ./test_native
```

The portable libthread implementation should have minimal overhead since it's a thin wrapper around native threading APIs.

## Debugging

### Enable Debug Builds

```bash
./configure CFLAGS="-g -O0"
make clean
make
```

### Trace Library Calls

On Linux, use `ltrace` to see library calls:
```bash
ltrace -C ./test_threads 2>&1 | grep -E "pthread|mtx|thrd"
```

### Check Linked Libraries

```bash
ldd ./test_threads
# Should show libthread.a and pthread if needed
```

## Continuous Integration

For CI systems, add these checks:

```bash
#!/bin/bash
set -e

./configure
make

# Run tests
gcc -std=c11 test_unicode.c -o test_unicode && ./test_unicode
gcc -std=c11 test_threads.c -o test_threads -lpthread && ./test_threads

echo "All C11 tests passed!"
```

## Platform-Specific Notes

### macOS
- Native C11 support available on 10.9+
- May need `-stdlib=libc++` for C++11 features

### Windows
- MinGW provides C11 support
- MSVC may require `/std:c11` flag
- Condition variables require Windows Vista+

### Embedded Systems
- May lack pthread support
- Consider disabling threading: `./configure --disable-threads`

## Reporting Issues

When reporting C11 support issues, include:

1. Configure output showing C11 detection
2. Contents of `config.h` NEED_* and HAVE_* defines
3. Compiler version: `gcc --version` or `pcc --version`
4. OS and architecture: `uname -a`
5. Test program output
6. Link command and any errors

## See Also

- `C11_SUPPORT.md` - Complete C11 support documentation
- `libunicode/README.md` - Unicode library details
- `libthread/README.md` - Threading library details
