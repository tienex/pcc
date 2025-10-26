# C11 Support in PCC

## Overview

PCC now includes comprehensive C11 (ISO/IEC 9899:2011) standard support with portable implementations of Unicode and threading libraries. The build system automatically detects whether your platform has native C11 support and builds portable fallback libraries only when needed.

## Features

### C11 Standard Detection

The configure script automatically detects:
- C11 compiler support (`-std=c11`)
- Native `<uchar.h>` Unicode support
- Native `<threads.h>` threading support
- POSIX threads as a fallback on Unix-like systems

### Portable Libraries

PCC provides two portable libraries that are built automatically if your system lacks C11 support:

#### 1. libunicode - C11 Unicode Support

**Location**: `libunicode/`

Provides a complete implementation of C11 Unicode character utilities:

- **Types**:
  - `char16_t` - 16-bit character type (UTF-16)
  - `char32_t` - 32-bit character type (UTF-32)

- **Functions**:
  - `mbrtoc16()` - Convert multibyte to UTF-16
  - `c16rtomb()` - Convert UTF-16 to multibyte
  - `mbrtoc32()` - Convert multibyte to UTF-32
  - `c32rtomb()` - Convert UTF-32 to multibyte

- **Features**:
  - Full UTF-8, UTF-16, and UTF-32 support
  - Proper surrogate pair handling
  - Invalid sequence detection
  - Overlong encoding protection

#### 2. libthread - C11 Threading Support

**Location**: `libthread/`

Provides a complete implementation of C11 threading primitives:

- **Thread Management**:
  - `thrd_create()`, `thrd_join()`, `thrd_detach()`
  - `thrd_current()`, `thrd_equal()`
  - `thrd_sleep()`, `thrd_yield()`, `thrd_exit()`

- **Mutex Operations**:
  - `mtx_init()`, `mtx_destroy()`
  - `mtx_lock()`, `mtx_unlock()`, `mtx_trylock()`
  - `mtx_timedlock()` - with timeout support
  - Support for plain, recursive, and timed mutexes

- **Condition Variables**:
  - `cnd_init()`, `cnd_destroy()`
  - `cnd_signal()`, `cnd_broadcast()`
  - `cnd_wait()`, `cnd_timedwait()`

- **Thread-Local Storage**:
  - `tss_create()`, `tss_delete()`
  - `tss_get()`, `tss_set()`
  - Destructor support (POSIX only)

- **Call Once**:
  - `call_once()` - execute initialization exactly once

- **Platform Support**:
  - **POSIX**: Uses `pthread.h` on Linux, BSD, macOS, etc.
  - **Windows**: Uses native Windows threading API
  - **Automatic linking**: Links with `-lpthread` when needed

## Building

### Standard Build

```bash
./configure
make
make install
```

The configure script will:
1. Detect if your system has C11 support
2. Check for native `<uchar.h>` and `<threads.h>`
3. Automatically build portable libraries if needed
4. Link them automatically with your programs

### Configure Options

All standard PCC configure options still apply. The C11 libraries integrate seamlessly with the existing build system.

### Build Output

During configuration, you'll see:

```
C11 support ...................... yes/no
Build portable Unicode library ... yes/no
Build portable Threading library . yes/no
```

## Usage

### Using Unicode Support

```c
#include <uchar.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    // UTF-8 to UTF-32 conversion
    const char *utf8 = "Hello, 世界! 🌍";
    char32_t c32;
    mbstate_t state = {0};
    size_t len;

    const char *p = utf8;
    while (*p) {
        len = mbrtoc32(&c32, p, 4, &state);
        if (len == (size_t)-1 || len == (size_t)-2) {
            fprintf(stderr, "Invalid UTF-8 sequence\n");
            break;
        }
        printf("U+%04X ", (unsigned)c32);
        p += len;
    }
    printf("\n");

    return 0;
}
```

### Using Threading Support

```c
#include <threads.h>
#include <stdio.h>

mtx_t mutex;
int shared_counter = 0;

int worker_thread(void *arg) {
    int id = *(int *)arg;

    for (int i = 0; i < 1000; i++) {
        mtx_lock(&mutex);
        shared_counter++;
        mtx_unlock(&mutex);
    }

    printf("Thread %d finished\n", id);
    return 0;
}

int main(void) {
    thrd_t threads[4];
    int ids[4] = {1, 2, 3, 4};

    mtx_init(&mutex, mtx_plain);

    // Create threads
    for (int i = 0; i < 4; i++) {
        if (thrd_create(&threads[i], worker_thread, &ids[i]) != thrd_success) {
            fprintf(stderr, "Failed to create thread %d\n", i);
            return 1;
        }
    }

    // Wait for threads
    for (int i = 0; i < 4; i++) {
        thrd_join(threads[i], NULL);
    }

    printf("Final counter value: %d\n", shared_counter);
    mtx_destroy(&mutex);

    return 0;
}
```

## Automatic Linking

The portable libraries are **automatically linked** when needed by the PCC driver:

### How It Works

1. During `./configure`, the build system:
   - Checks for native `<uchar.h>` and `<threads.h>` support
   - Sets `NEED_LIBUNICODE` and/or `NEED_LIBTHREAD` in `config.h` if needed
   - Marks libraries for building via `BUILD_LIBUNICODE` and `BUILD_LIBTHREAD`

2. During compilation, the PCC driver (`cc/driver/platform.c`):
   - Checks the `NEED_*` defines from `config.h`
   - Automatically adds `-lunicode` to link flags if `NEED_LIBUNICODE` is defined
   - Automatically adds `-lthread -lpthread` to link flags if `NEED_LIBTHREAD` is defined
   - Includes these in the standard library flags, so no manual intervention needed

3. At install time:
   - Headers (`uchar.h`, `threads.h`) are installed to `$(includedir)`
   - Libraries (`libunicode.a`, `libthread.a`) are installed to `$(libdir)`
   - They become part of the standard PCC runtime environment

### What This Means for Users

**No manual library flags needed:**
```bash
# This just works - no -lunicode or -lthread needed:
pcc myprogram.c -o myprogram

# The driver automatically adds the necessary libraries based on config.h
```

**Headers automatically available:**
```c
#include <uchar.h>    // Works automatically
#include <threads.h>  // Works automatically
```

**Zero configuration:**
- Users don't need to know whether they're using native or portable C11
- The same code works everywhere
- Build scripts don't need platform-specific library flags

## Platform Compatibility

### Tested Platforms

- **Linux**: Full support (glibc, musl)
- **FreeBSD, OpenBSD, NetBSD**: Full support
- **macOS**: Full support
- **Windows**: Full support (MinGW, MSVC)
- **Solaris/Illumos**: Full support

### Requirements

- **Minimum**: C99 compiler for host
- **Recommended**: C11 compiler (for native support)
- **POSIX systems**: pthread library (usually provided by system)
- **Windows**: Windows Vista or later (for condition variables)

## Implementation Details

### Unicode Library

- **Encoding**: UTF-8 is used as the multibyte encoding
- **Validation**: Full UTF-8 validation including:
  - Overlong sequence detection
  - Surrogate pair validation
  - Code point range checking (U+0000 to U+10FFFF)
- **Surrogate Pairs**: Properly handled in UTF-16 conversions
- **Thread Safety**: All functions are reentrant and thread-safe

### Threading Library

- **POSIX Backend**:
  - Uses `pthread.h` primitives
  - Thread-local storage destructors fully supported
  - Automatic cleanup on thread exit

- **Windows Backend**:
  - Uses native Windows threads
  - Critical sections for mutexes
  - Condition variables (Vista+)
  - TLS via `TlsAlloc/TlsGetValue/TlsSetValue`

- **Performance**: Minimal overhead wrapper around native APIs

## Standards Compliance

Both libraries implement the C11 standard as specified in:
- **ISO/IEC 9899:2011** - C11 Standard
- Section 7.28 - Unicode utilities `<uchar.h>`
- Section 7.26 - Threads `<threads.h>`

## Known Limitations

### Unicode Library
- No locale support (assumes UTF-8 multibyte encoding)
- No normalization (NFD, NFC, etc.)
- No Unicode properties or case conversion

### Threading Library
- **Windows**: Thread-local storage destructors not supported
- **Windows XP**: No condition variable support (requires Vista+)
- `mtx_timedlock()` on Windows uses busy-waiting (performance impact)

## Contributing

When adding platform-specific code:
1. Test on both POSIX and Windows if possible
2. Ensure thread safety of all operations
3. Follow C11 standard semantics exactly
4. Add tests for new functionality

## License

These libraries are part of the Portable C Compiler (PCC) project and are distributed under the same BSD-style license as PCC. See the COPYING file for details.

## See Also

- `libunicode/README.md` - Detailed Unicode library documentation
- `libthread/README.md` - Detailed threading library documentation
- C11 Standard: ISO/IEC 9899:2011
- POSIX.1-2008: IEEE Std 1003.1-2008
