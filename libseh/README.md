# libseh - Structured Exception Handling Runtime Library for PCC

## Overview

`libseh` is a comprehensive cross-platform runtime library that provides full Structured Exception Handling (SEH) support for programs compiled with PCC. It enables SEH constructs (`__try`, `__except`, `__finally`, `__leave`) to work seamlessly on non-Windows platforms using DWARF exception handling, setjmp/longjmp, and signal handling.

## Features

- **Cross-platform SEH support**: Works on Windows, Linux, *BSD, macOS, and other Unix-like systems
- **DWARF integration**: Uses native DWARF exception handling on ELF platforms
- **Signal-to-exception mapping**: Converts Unix signals (SIGSEGV, SIGFPE, etc.) to SEH exceptions
- **C++ exception interoperability**: SEH and C++ exceptions can coexist and interact properly
- **Exception context capture**: Full CPU context (registers, IP, SP) captured on all platforms
- **Thread-safe**: Uses thread-local storage for exception chains
- **Automatic linking**: Automatically linked when using `-fseh` compiler flag
- **Compatible with native Windows SEH**: On Windows, delegates to native SEH mechanisms

## Architecture

### Windows Platforms

On Windows (x86/x64), the library provides thin wrappers around native SEH. The compiler generates code that uses Windows' built-in exception handling.

### Non-Windows Platforms

On Unix-like platforms, libseh provides SEH functionality through:

1. **Signal Handlers**: Convert hardware exceptions (segfaults, divide-by-zero, etc.) to SEH exceptions
2. **Exception Chain**: Thread-local linked list of exception handlers
3. **DWARF Unwinder**: On ELF platforms, integrates with the native DWARF unwinder
4. **setjmp/longjmp**: Fallback mechanism for exception control flow

## API

### Exception Constants

```c
#define EXCEPTION_EXECUTE_HANDLER      1
#define EXCEPTION_CONTINUE_SEARCH      0
#define EXCEPTION_CONTINUE_EXECUTION  (-1)

/* Common exception codes */
#define EXCEPTION_ACCESS_VIOLATION          0xC0000005L
#define EXCEPTION_INT_DIVIDE_BY_ZERO        0xC0000094L
#define EXCEPTION_STACK_OVERFLOW            0xC00000FDL
/* ... and more */
```

### Data Structures

```c
struct _seh_exception_record {
    unsigned long ExceptionCode;
    unsigned long ExceptionFlags;
    struct _seh_exception_record *ExceptionRecord;
    void *ExceptionAddress;
    unsigned long NumberParameters;
    unsigned long ExceptionInformation[15];
};

struct _seh_exception_pointers {
    struct _seh_exception_record *ExceptionRecord;
    void *ContextRecord;
};
```

### Runtime Functions

```c
/* Register/unregister exception handlers */
void _seh_register(struct _seh_registration *reg, void *handler, void *filter);
void _seh_unregister(struct _seh_registration *reg);

/* Execute finally block */
void _seh_execute_finally(struct _seh_registration *reg, void (*finally_block)(void));

/* Raise exceptions */
void _seh_raise_exception(unsigned long code, unsigned long flags,
                          unsigned long nparams, unsigned long *params);

/* Get exception information */
unsigned long _seh_get_exception_code(void);
struct _seh_exception_pointers *_seh_get_exception_info(void);

/* C++ exception interoperability */
void _seh_translate_cxx_exception(void *cxx_exception);
int _seh_is_cxx_exception(void);
void *_seh_get_cxx_exception(void);

/* Exception context (CPU registers, IP, SP) */
void *_seh_get_ip(void *context);
void *_seh_get_sp(void *context);
void _seh_set_ip(void *context, void *ip);
unsigned long _seh_get_register(void *context, int reg_index);
```

## Usage

### Compiling with SEH Support

To enable SEH support for non-Windows targets, use the `-fseh` flag:

```bash
pcc -fseh -o program program.c
```

The `-fseh` flag automatically links `libseh`, so you don't need to specify `-lseh` manually.

### Example Code

```c
#include <stdio.h>
#include <seh.h>

int main(void) {
    __try {
        int *p = NULL;
        *p = 42;  // Access violation
    }
    __except (EXCEPTION_EXECUTE_HANDLER) {
        printf("Exception caught: 0x%08lX\n", _seh_get_exception_code());
        return 1;
    }

    return 0;
}
```

### Try-Finally Example

```c
void process_file(const char *filename) {
    FILE *f = fopen(filename, "r");

    __try {
        // Process file
        // ...
    }
    __finally {
        if (f) fclose(f);  // Always executed
    }
}
```

### Nested Try Blocks

```c
__try {
    printf("Outer try\n");

    __try {
        printf("Inner try\n");
        // Potential exception
    }
    __except (EXCEPTION_CONTINUE_SEARCH) {
        printf("Inner handler\n");
    }

} __except (EXCEPTION_EXECUTE_HANDLER) {
    printf("Outer handler\n");
}
```

## Building

### From Source

```bash
cd libseh
make
sudo make install
```

### Build Options

- `CC`: C compiler (default: pcc)
- `PREFIX`: Installation prefix (default: /usr/local)
- `CFLAGS`: Additional compiler flags

Example:
```bash
make CC=gcc PREFIX=/opt/pcc
```

## Implementation Details

### Exception Chain

Each `__try` block creates a `_seh_registration` structure on the stack and links it into the thread-local exception chain. When an exception occurs, the chain is walked to find a matching handler.

### Signal Handling

On Unix platforms, libseh installs signal handlers for:
- `SIGSEGV` → `EXCEPTION_ACCESS_VIOLATION`
- `SIGFPE` → `EXCEPTION_FLT_DIVIDE_BY_ZERO`
- `SIGILL` → `EXCEPTION_ILLEGAL_INSTRUCTION`
- `SIGBUS` → `EXCEPTION_DATATYPE_MISALIGNMENT`
- `SIGTRAP` → `EXCEPTION_BREAKPOINT`

### DWARF Integration

On ELF platforms (Linux, *BSD), libseh provides a personality routine (`_seh_dwarf_personality`) that integrates with the DWARF unwinder. This allows proper stack unwinding through multiple frames.

### Thread Safety

All exception state is stored in thread-local storage (`__thread` variables), making libseh fully thread-safe.

## Platform Support

| Platform | Mechanism | Status |
|----------|-----------|--------|
| Windows x86 | Native SEH (FS:[0]) | Supported |
| Windows x64 | Native SEH (table-based) | Supported |
| Linux x86/x64 | DWARF + signals | Supported |
| FreeBSD/OpenBSD/NetBSD | DWARF + signals | Supported |
| macOS | DWARF + signals | Supported |
| Other Unix | setjmp + signals | Supported |

## Limitations

1. **Performance**: Signal-based exception handling has overhead compared to native SEH
2. **Asynchronous Signals**: Only synchronous signals (SIGSEGV, SIGFPE) are reliably converted
3. **Context Information**: Limited CPU context is available compared to Windows SEH
4. **Optimizations**: Some compiler optimizations may interfere with exception handling

## Future Enhancements

- [ ] Support for exception filters with full context access
- [ ] Better integration with C++ exceptions
- [ ] ARM/AArch64 support
- [ ] Performance optimizations
- [ ] More detailed exception information

## License

See the PCC license (BSD-style).

## See Also

- SEH_IMPLEMENTATION.md - Compiler-side SEH implementation
- PCC documentation
- Windows SEH documentation

## C++ Exception Interoperability

### Overview

libseh provides seamless interoperability between SEH and C++ exceptions on Unix/Linux/macOS platforms. This allows:

1. C++ exceptions to be caught by SEH `__except` handlers
2. SEH `__try`/`__except` blocks to work correctly in C++ code
3. Proper cleanup order (SEH finally blocks, then C++ destructors)

### Mixing SEH and C++

```cpp
#include <iostream>
#include <stdexcept>
#include <seh.h>

class Resource {
public:
    Resource() { std::cout << "Acquired\n"; }
    ~Resource() { std::cout << "Released\n"; }
};

int main() {
    __try {
        Resource r;  // C++ RAII object

        __try {
            throw std::runtime_error("C++ exception");
        }
        __except (EXCEPTION_EXECUTE_HANDLER) {
            std::cout << "Caught C++ exception in SEH handler!\n";

            if (_seh_is_cxx_exception()) {
                std::cout << "Exception code: 0x" << std::hex 
                          << _seh_get_exception_code() << "\n";
            }
        }
    }
    __finally {
        std::cout << "SEH finally block\n";
    }

    // Resource destructor runs after finally block
    return 0;
}
```

Compile with:
```bash
pcc++ -fseh -o test test.cpp
```

### How It Works

1. **C++ Exception → SEH**: When a C++ exception propagates through a SEH `__try` block, libseh's DWARF personality routine recognizes the C++ exception class and converts it to SEH exception code `0xE06D7363` (Microsoft C++ exception code).

2. **SEH Filter Execution**: SEH filter expressions can examine the exception and decide whether to handle it using `_seh_is_cxx_exception()` and `_seh_get_cxx_exception()`.

3. **Proper Cleanup**: C++ destructors run in reverse order of construction, interleaved with SEH finally blocks as appropriate.

### Platform-Specific Notes

- **macOS/Darwin**: Uses DWARF unwinder with Apple's exception handling ABI
- **Linux/BSD**: Uses standard Itanium C++ ABI with DWARF unwinding
- **Windows**: Uses native SEH which already interoperates with MSVC C++ exceptions

