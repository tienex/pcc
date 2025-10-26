# libseh - Structured Exception Handling Runtime Library for PCC

## Overview

`libseh` is a cross-platform runtime library that provides Structured Exception Handling (SEH) support for programs compiled with PCC. It enables SEH constructs (`__try`, `__except`, `__finally`, `__leave`) to work on non-Windows platforms using DWARF exception handling or setjmp/longjmp.

## Features

- **Cross-platform SEH support**: Works on Windows, Linux, *BSD, and other Unix-like systems
- **DWARF integration**: Uses native DWARF exception handling on ELF platforms
- **Signal-to-exception mapping**: Converts Unix signals (SIGSEGV, SIGFPE, etc.) to SEH exceptions
- **Thread-safe**: Uses thread-local storage for exception chains
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
```

## Usage

### Compiling with SEH Support

To enable SEH support for non-Windows targets, use the `-fseh` flag:

```bash
pcc -fseh -o program program.c -lseh
```

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
