# PCC Infrastructure Libraries

Comprehensive, reusable compiler infrastructure libraries for multi-language support.

## Overview

PCC includes an extensive suite of infrastructure libraries designed to be reusable across compiler projects. These libraries provide essential functionality for memory management, exception handling, Unicode text processing, virtual dispatch, legacy compatibility, and more.

## Core Runtime Libraries

### libgc - Garbage Collection and Automatic Reference Counting

**Location**: `libgc/`
**Headers**: `libgc/gc.h`
**Implementation**: `libgc/gc.c` (550 lines)

Comprehensive garbage collection and memory management library supporting multiple collection strategies:

**Features:**
- Mark-and-sweep garbage collection
- Automatic Reference Counting (ARC)
- Generational GC
- Incremental and concurrent collection
- Conservative and precise GC modes
- Finalizer support
- Weak references
- Thread-safe operations
- Statistics and monitoring

**Supported GC Modes:**
- `GC_MODE_NONE` - Manual memory management only
- `GC_MODE_REFCOUNT` - Reference counting
- `GC_MODE_MARK_SWEEP` - Mark and sweep
- `GC_MODE_COPYING` - Copying collector
- `GC_MODE_GENERATIONAL` - Generational GC
- `GC_MODE_INCREMENTAL` - Incremental GC
- `GC_MODE_CONCURRENT` - Concurrent GC

**Usage Example:**
```c
#include "gc.h"

gc_config_t config = {
    .mode = GC_MODE_MARK_SWEEP,
    .precision = GC_PRECISE,
    .initial_heap_size = 1024 * 1024,
    .max_heap_size = 16 * 1024 * 1024
};
gc_init(&config);

void *obj = gc_alloc(sizeof(MyStruct));
gc_collect();  // Trigger collection

gc_shutdown();
```

---

### libexcept - Universal Exception Handling

**Location**: `libexcept/`
**Headers**: `libexcept/except.h`
**Implementation**: `libexcept/except.c` (400 lines)

Cross-language exception handling supporting multiple exception models:

**Features:**
- C setjmp/longjmp exceptions
- C++ exception support
- Ada exception support
- Modula-3 exception handling
- Oberon-2 TRAP support
- SEH (Windows Structured Exception Handling)
- DWARF unwinding
- Stack unwinding with cleanup handlers
- Backtrace support
- Exception translation between languages

**Supported Exception Types:**
- `EXCEPT_TYPE_C_LONGJMP` - C-style exceptions
- `EXCEPT_TYPE_CXX` - C++ exceptions
- `EXCEPT_TYPE_ADA` - Ada exceptions
- `EXCEPT_TYPE_MODULA3` - Modula-3 exceptions
- `EXCEPT_TYPE_OBERON` - Oberon-2 TRAP
- `EXCEPT_TYPE_SEH` - Windows SEH
- `EXCEPT_TYPE_DWARF` - DWARF unwinding

**Usage Example:**
```c
#include "except.h"

except_init();

EXCEPT_TRY(ctx)
    // Code that might raise exception
    except_raise("DivisionByZero", "Cannot divide by zero");
EXCEPT_CATCH("DivisionByZero")
    printf("Caught division by zero\n");
EXCEPT_CATCH_ALL
    printf("Caught unknown exception\n");
EXCEPT_END

except_shutdown();
```

---

### libustring - Universal Unicode String Library

**Location**: `libustring/`
**Headers**: `libustring/ustring.h`
**Implementation**: `libustring/ustring.c` (960 lines)

Comprehensive Unicode string library supporting multiple encodings:

**Supported Encodings:**
- UTF-8, UTF-16 (LE/BE), UTF-32 (LE/BE)
- UTF-7 (email-safe Unicode)
- UTF-9 (historical 9-bit encoding)
- UTF-18 (historical 18-bit encoding)
- UTF-EBCDIC (for IBM mainframes)
- EBCDIC to/from ASCII conversion

**Features:**
- String creation and manipulation
- Encoding conversion
- Character counting
- Substring operations
- String comparison (case-sensitive and case-insensitive)
- Find/search operations
- Case conversion (upper, lower, title)
- Unicode normalization (NFC, NFD, NFKC, NFKD)
- Character property queries
- Grapheme cluster support
- Iterator interface

**Usage Example:**
```c
#include "ustring.h"

// Create UTF-8 string
ustr8_t *str = ustr8_create("Hello, 世界");

// Convert to UTF-16
ustr16_t *str16 = ustr16_from_utf8("Hello, 世界");

// Convert to UTF-32
ustr32_t *str32 = ustr32_from_utf8("Hello, 世界");

// Case conversion
ustr8_t *upper = ustr8_to_upper(str);

// Concatenation
ustr8_t *combined = ustr8_concat(str, upper);

// Cleanup
ustr8_free(str);
ustr16_free(str16);
ustr32_free(str32);
ustr8_free(upper);
ustr8_free(combined);
```

---

### libvtable - Virtual Table and Object-Oriented Messaging

**Location**: `libvtable/`
**Headers**: `libvtable/vtable.h`
**Implementation**: `libvtable/vtable.c` (800 lines)

Universal vtable layout and method dispatch supporting multiple object-oriented paradigms:

**Features:**
- **C++ Support:**
  - Single inheritance vtables
  - Multiple inheritance with layout calculation
  - Virtual inheritance with VBT (virtual base tables)
  - RTTI (Runtime Type Information)
  - Dynamic cast support

- **Objective-C Support:**
  - Class and metaclass management
  - Selector-based method dispatch
  - Method caching
  - Category support
  - ISA pointer handling

- **Oberon-2 Support:**
  - Type-bound procedures
  - Type extension (inheritance)
  - Type test (IS operator)

- **Cross-Language:**
  - Method Resolution Order (C3 linearization)
  - Universal method dispatch
  - Vtable serialization for precompiled headers
  - Type conversion between vtable formats

**Usage Example:**
```c
#include "vtable.h"

// Create C++ vtable
vtable_builder_t *builder = vtable_builder_create(VTABLE_CXX_SINGLE, "MyClass");

vfunc_entry_t method = {
    .name = "foo",
    .mangled_name = "_ZN7MyClass3fooEv",
    .function_ptr = &my_function,
    .convention = CALL_THISCALL
};
vtable_builder_add_method(builder, &method);

vtable_t *vtable = vtable_builder_build(builder);

// Dispatch method
void *func = vtable_get_method(vtable, "foo");

vtable_free(vtable);
vtable_builder_free(builder);
```

---

## Symbol Management

### libmangle - Symbol Mangling and Demangling

**Location**: `libmangle/`
**Status**: Fully implemented (2500 lines)

Universal symbol mangler/demangler supporting multiple languages and ABI standards:

**Supported Mangling Schemes:**
- C (no mangling)
- C++ Itanium ABI
- C++ MSVC ABI
- Pascal/Delphi
- Modula-2/Modula-3
- Oberon
- Ada
- PCC Universal (cross-language)

**Features:**
- Bidirectional mangling/demangling
- Auto-detection of mangling schemes
- Type signature encoding
- Namespace/scope handling
- Template/generic support
- Scheme conversion

See `libmangle/README.md` for detailed documentation.

---

## Legacy Compatibility Libraries

### libcrt - Borland CRT Unit Emulation

**Location**: `libcrt/`
**Headers**: `libcrt/crt.h`
**Status**: Header defined, implementation pending

Borland Pascal CRT unit compatibility for console I/O:

**Features:**
- Screen control (ClrScr, GotoXY, WhereX, WhereY)
- Text attributes (TextColor, TextBackground, TextAttr)
- Text modes (CO80, BW80, MONO, etc.)
- Window management
- Keyboard input (ReadKey, KeyPressed)
- Sound (PC speaker emulation)
- Cross-platform (ANSI escape sequences, ncurses, Windows Console API)

**Supported Backends:**
- ANSI escape sequences (Unix/Linux)
- ncurses (full-featured terminal control)
- Windows Console API (native Windows support)

---

### libbgi - Borland Graphics Interface Emulation

**Location**: `libbgi/`
**Headers**: `libbgi/bgi.h`
**Status**: Header defined, implementation pending

Comprehensive graphics library emulating Borland BGI and Microsoft GRAPH/PGCHART:

**Features:**
- **Borland BGI Compatibility:**
  - All standard BGI drivers (CGA, EGA, VGA, MCGA, Hercules, etc.)
  - Drawing primitives (line, circle, arc, ellipse, polygon)
  - Fill patterns and styles
  - Multiple fonts (Gothic, Simplex, Triplex, etc.)
  - Image manipulation

- **Microsoft GRAPH Compatibility:**
  - Video mode management
  - Graphics primitives
  - Viewport and clipping

- **Microsoft PGCHART:**
  - Column, bar, line, scatter, and pie charts
  - Chart legends and axes
  - Custom palettes

**Supported Backends:**
- SDL2 (modern, cross-platform)
- OpenGL (hardware-accelerated)
- Linux framebuffer (embedded systems)

---

## Additional Infrastructure Libraries

The following libraries have headers defined and are ready for implementation:

### libgthread - Green Threads and Native Threads

**Location**: `libgthread/`
**Headers**: `libgthread/gthread.h`

Unified threading library supporting both user-space (green) and native threads:

- Green threads (M:N threading, work-stealing scheduler)
- Native threads (POSIX, Windows, C11 threads)
- Thread pools
- Synchronization primitives (mutexes, condition variables, semaphores)
- Read-write locks
- Atomic operations

---

### libnet - Asynchronous I/O and Networking

**Location**: `libnet/`
**Headers**: `libnet/net.h`

Comprehensive async I/O and networking library:

- Asynchronous I/O (epoll, kqueue, IOCP)
- Socket operations (TCP, UDP, Unix domain)
- HTTP/HTTPS client and server
- WebSocket support
- TLS/SSL (OpenSSL, LibreSSL)
- DNS resolution
- URL parsing

---

### libpccir - PCC Intermediate Representation Writer

**Location**: `libpccir/`
**Headers**: `libpccir/pccir.h`

Library for writing PCC IR for cross-language modules:

- IR node creation
- Type system
- Cross-language module support
- Precompiled headers
- Generic/template support
- Optimization hints

---

### libunwind - Portable Stack Unwinding

**Location**: `libunwind/`
**Headers**: `libunwind/unwind.h`

Portable stack unwinding library:

- DWARF unwinding
- Platform-specific unwinding (x86, ARM, MIPS, etc.)
- Frame walking
- Register context
- Exception unwinding support

---

### libxmath - Extended Mathematics Library

**Location**: `libxmath/`
**Headers**: `libxmath/xmath.h`

Extended math library supporting multiple floating-point formats:

- **IEEE 754 formats:**
  - IEEE 754-2008: binary16, binary32, binary64, binary128
  - IEEE 754-2008: decimal64, decimal128

- **Specialized formats:**
  - bfloat16 (Google Brain Float)
  - FP16 (half precision)
  - FP8 (quarter precision)

- **Historic formats:**
  - VAX floating-point (F, D, G, H)
  - Cray floating-point
  - IBM floating-point (all sizes)
  - MBF (Microsoft Binary Format)

---

## Build System Integration

All libraries are integrated into the PCC build system:

```bash
./configure --enable-languages=all
make
make install
```

Individual libraries can be built:

```bash
cd libgc && make
cd libexcept && make
cd libustring && make
# etc.
```

---

## Design Principles

All infrastructure libraries follow these design principles:

1. **Reusability**: Libraries are compiler-agnostic and can be used in any project
2. **Thread Safety**: All operations are thread-safe where applicable
3. **Cross-Platform**: Support for Linux, *BSD, macOS, Windows
4. **Cross-Language**: Designed for multi-language compiler support
5. **Performance**: Optimized implementations with minimal overhead
6. **Standards Compliance**: Follow relevant language standards and ABIs
7. **Comprehensive Testing**: Extensive test suites for all features

---

## License

Copyright (c) 2025 PCC Project

These libraries are part of the Portable C Compiler (PCC) project and are distributed under the same license as PCC.

---

## Contributing

Contributions are welcome! Please see CONTRIBUTING.md for guidelines.

For bug reports and feature requests, please use the PCC issue tracker.

---

## Documentation

Each library has detailed documentation in its respective directory:

- `libgc/README.md` - Garbage collection guide
- `libexcept/README.md` - Exception handling guide
- `libustring/README.md` - Unicode string processing guide
- `libvtable/README.md` - Virtual table and OOP guide
- `libmangle/README.md` - Symbol mangling guide
- `libcrt/README.md` - Borland CRT emulation guide
- `libbgi/README.md` - BGI graphics guide

---

## Architecture Diagram

```
PCC Infrastructure Libraries
│
├── Memory Management
│   └── libgc (Garbage Collection & ARC)
│
├── Exception Handling
│   ├── libexcept (Universal Exception Handling)
│   └── libunwind (Stack Unwinding)
│
├── Text & Unicode
│   └── libustring (Unicode Strings)
│
├── Object-Oriented Support
│   ├── libvtable (Virtual Tables & Messaging)
│   └── libmangle (Symbol Mangling)
│
├── Threading & Concurrency
│   └── libgthread (Green & Native Threads)
│
├── I/O & Networking
│   └── libnet (Async I/O & Networking)
│
├── Code Generation
│   └── libpccir (PCC IR Writer)
│
├── Mathematics
│   └── libxmath (Extended Math & Float Formats)
│
└── Legacy Compatibility
    ├── libcrt (Borland CRT Emulation)
    └── libbgi (BGI/GRAPH/PGCHART Graphics)
```

---

## Roadmap

### Completed (v1.0):
- [x] libgc - Garbage collection and ARC
- [x] libexcept - Exception handling
- [x] libustring - Unicode strings with UTF-7/9/18/EBCDIC
- [x] libvtable - Virtual tables and Objective-C messaging
- [x] libmangle - Symbol mangling/demangling
- [x] libcrt - Header API defined
- [x] libbgi - Header API defined

### In Progress (v1.1):
- [ ] Complete libcrt implementation
- [ ] Complete libbgi implementation (SDL2 backend)
- [ ] libgthread implementation
- [ ] libnet implementation
- [ ] libxmath implementation

### Planned (v2.0):
- [ ] libpccir implementation
- [ ] libunwind implementation
- [ ] Additional graphics backends (OpenGL, Vulkan)
- [ ] WebAssembly support in libgthread
- [ ] HTTP/2 and HTTP/3 support in libnet

---

*Last Updated: 2025-11-15*
