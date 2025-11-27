# i386 and x86-64 Extensions for PCC

This document describes the memory models and compiler extensions implemented for PCC's i386 and x86-64 (amd64) backends.

## Overview

PCC supports several memory models and vendor-specific extensions for i386 and x86-64 architectures:

1. **Memory Models** - Control pointer size and addressing modes (i386 only)
2. **`__far16`** - Watcom 16-bit far pointer support (i386 large model)
3. **`__fs` / `__gs`** - Segment register access (i386 and x86-64)
4. **`__near32`** - 32-bit near pointer for macOS (x86-64 only)

These extensions enable compatibility with DOS/Windows code bases and provide low-level access to x86-specific features.

---

## Memory Models (i386)

The i386 backend supports two memory models:

| Memory Model | Code Pointers | Data Pointers | Common Usage |
|--------------|---------------|---------------|--------------|
| **small** (default) | 32-bit near  | 32-bit near   | Flat 32-bit addressing (modern protected mode) |
| **large**           | 32-bit near  | 48-bit far    | Segmented addressing (16-bit selector + 32-bit offset) |

### Usage

Specify the memory model using the `-mcmodel` flag:

```bash
pcc -mcmodel=small -c file.c    # Default: 32-bit flat addressing
pcc -mcmodel=large -c file.c    # 48-bit segmented addressing
```

### Small Memory Model (Default)

- **Pointer Size**: 32 bits
- **Addressing**: Flat memory model
- **Use Cases**:
  - Modern protected mode applications
  - Linux/BSD/Windows 32-bit executables
  - Default for most i386 code

**Example:**
```c
int *ptr;                // 32-bit near pointer
void *data = malloc(100); // Returns 32-bit pointer
```

### Large Memory Model

- **Pointer Size**: 48 bits (16-bit selector + 32-bit offset)
- **Addressing**: Segmented memory model
- **Use Cases**:
  - Legacy segmented applications
  - Code that needs to access multiple segments
  - Compatibility with DOS extender code

**Example:**
```c
// In large model, pointers are 48-bit by default
int *ptr;                // 48-bit far pointer
char *str = "hello";     // 48-bit far pointer to constant
```

---

## `__far16` - Watcom 16-bit Far Pointer

### Description

The `__far16` keyword provides compatibility with Watcom C/C++ for specifying 16-bit far pointers in i386 large memory model. This is used when mixing 16-bit and 32-bit code.

**Availability**: i386 only (also available on i86 for forward compatibility)

### Syntax

```c
type __far16 *pointer_name;
__attribute__((far16)) type *pointer_name;  // Equivalent attribute form
```

### Examples

```c
// 16-bit far pointer to video memory
unsigned char __far16 *video_mem = (unsigned char __far16 *)0xB8000000L;

// Function taking 16-bit far pointer
void write_video(char __far16 *dest, const char *src, int len);

// Struct with 16-bit far pointer member
struct legacy_descriptor {
    unsigned short selector;
    unsigned long offset;
    void __far16 *legacy_ptr;  // 16-bit far pointer for compatibility
};
```

### Use Cases

1. **DOS Extender Compatibility**: Interfacing with 16-bit real mode from 32-bit protected mode
2. **BIOS Calls**: Calling 16-bit BIOS routines from 32-bit code
3. **Legacy Code**: Maintaining compatibility with Watcom C/C++ code bases
4. **Mixed Mode**: Applications using both 16-bit and 32-bit addressing

---

## `__fs` and `__gs` - Segment Register Access

### Description

The `__fs` and `__fs` keywords provide access to the FS and GS segment registers, which are available on all x86 architectures from the 80386 onward.

**Availability**: i386, x86-64 (also available on i86 for forward compatibility)

### Syntax

```c
type __fs *pointer_name;
type __gs *pointer_name;
__attribute__((fs)) type *pointer_name;  // Equivalent attribute form
__attribute__((gs)) type *pointer_name;  // Equivalent attribute form
```

### x86-64 Usage

On x86-64, FS and GS are commonly used for:
- **Thread-Local Storage (TLS)**: Accessing per-thread data
- **Kernel/User Separation**: Kernel uses GS for per-CPU data
- **Security Features**: Stack canary storage

### Examples

#### i386 Examples

```c
// Access thread-local storage via FS
int __fs *tls_data;

// Custom segment-based addressing
struct segment_descriptor {
    unsigned short selector;
    unsigned long base;
    unsigned long limit;
};

void setup_custom_segment(void) {
    // FS-based pointer for custom segment
    unsigned char __fs *custom_data;
    // ... segment setup code ...
}

// Reading from specific segment
int read_fs_value(unsigned int offset) {
    int __fs *ptr = (int __fs *)offset;
    return *ptr;
}
```

#### x86-64 Examples

```c
// Access thread-local variable via FS (Linux/BSD convention)
// FS typically points to TLS block
struct thread_info {
    int thread_id;
    void *stack_base;
    unsigned long stack_canary;
};

// Read thread-local data
int get_thread_id(void) {
    struct thread_info __fs *ti = (struct thread_info __fs *)0;
    return ti->thread_id;
}

// Windows x64 uses GS for TLS
void *get_teb_pointer(void) {
    void __gs **teb_ptr = (void __gs **)0x30;
    return *teb_ptr;
}

// Stack canary check (GCC-style)
unsigned long __gs *get_stack_canary_ptr(void) {
    // GS:0x28 contains stack canary on Linux x86-64
    return (unsigned long __gs *)0x28;
}
```

### Platform-Specific Usage

| Platform | FS Usage | GS Usage |
|----------|----------|----------|
| **Linux x86-64** | Thread-local storage (TLS) | Unused (available for apps) |
| **Windows x64** | Unused | Thread Environment Block (TEB) |
| **FreeBSD x86-64** | Thread-local storage | Unused |
| **Linux Kernel x86-64** | Unused | Per-CPU data |

### Use Cases

1. **Thread-Local Storage**: Fast access to per-thread variables
2. **OS Kernel Development**: Per-CPU data structures
3. **Security**: Stack canary storage
4. **Performance**: Zero-overhead thread-local access
5. **Legacy Compatibility**: DOS/Windows segmented code

---

## `__near32` - 32-bit Near Pointer (x86-64)

### Description

The `__near32` keyword allows x86-64 code to export 32-bit routines for compatibility with 32-bit callers. This is primarily used on macOS for creating universal binaries that support both 32-bit and 64-bit execution.

**Availability**: x86-64 (amd64) only

**Inspiration**: CrossOver/LLVM implementation for macOS 32-bit compatibility layer

### Syntax

```c
type __near32 function_name(parameters);
__attribute__((near32)) type function_name(parameters);  // Equivalent attribute form
```

### Examples

#### macOS Universal Binary Support

```c
// Export 32-bit routine from 64-bit library
int __near32 legacy_api_function(int x, int y) {
    // Function uses 32-bit calling convention
    // Pointers are 32-bit within this function
    return x + y;
}

// 32-bit callback for legacy APIs
typedef void (__near32 *legacy_callback_t)(int event, void *data);

void register_legacy_callback(legacy_callback_t callback) {
    // Callback will be called with 32-bit pointers
}
```

#### CrossOver/Wine Compatibility

```c
// Wine/CrossOver thunk layer for 32-bit Windows apps on 64-bit macOS
void __near32 *wine_get_dos_file_name(const char *unix_path) {
    // Returns 32-bit pointer for 32-bit Windows application
    static char dos_path[260];
    convert_unix_to_dos_path(unix_path, dos_path);
    return (void __near32 *)dos_path;
}

// WoW64-style thunk (Windows-on-Windows 64-bit)
int __near32 NtCreateFile32(
    void __near32 *FileHandle,
    unsigned int DesiredAccess,
    void __near32 *ObjectAttributes
) {
    // 32-bit version of 64-bit NtCreateFile
    // All pointers are 32-bit
}
```

#### macOS Transition APIs

```c
// Support for 32-bit legacy frameworks
void __near32 *CFBundleGetFunctionPointerForName32(
    void *bundle,
    const char *functionName
) {
    // Returns 32-bit function pointer for legacy Carbon/Cocoa APIs
}

// QuickTime legacy support
int __near32 QTMoviePlayer_Load32(const char __near32 *filename) {
    // 32-bit QuickTime player interface
}
```

### Use Cases

1. **macOS Universal Binaries**: Supporting both 32-bit and 64-bit execution
2. **Wine/CrossOver**: Running 32-bit Windows applications on 64-bit Unix
3. **Legacy API Compatibility**: Calling old 32-bit libraries from 64-bit code
4. **Thunk Layers**: Creating compatibility shims between 32-bit and 64-bit code
5. **Rosetta-style Translation**: Emulation/translation layer support

### Notes

- On x86-64, pointers marked `__near32` are 32 bits wide
- Useful for compatibility layers and emulation
- Inspired by Apple's macOS transition technologies and CrossOver/Wine thunk layers
- Allows 64-bit applications to provide 32-bit interfaces

---

## Platform Support

| Feature | i86 | i386 | x86-64 | Notes |
|---------|-----|------|--------|-------|
| Memory models (small/large) | 6 models | 2 models | - | i86 has 6, i386 has 2 |
| `__far16` | ✓ | ✓ | ✗ | Forward compat on i86 |
| `__fs` / `__gs` | ✓ | ✓ | ✓ | 386+ hardware feature |
| `__near32` | ✗ | ✗ | ✓ | x86-64 only |

## Compiler Compatibility

| Compiler | `__far16` | `__fs`/`__gs` | `__near32` |
|----------|-----------|---------------|------------|
| Watcom C/C++ 10.x-11.x | ✓ | ✓ | ✗ |
| DJGPP (DOS) | ✓ | ✓ | ✗ |
| Clang/LLVM (CrossOver) | ✗ | ✓ | ✓ |
| PCC (this impl) | ✓ | ✓ | ✓ |

## See Also

- `arch/i86/DOS_KEYWORDS.md` - DOS-era i86 keywords
- `arch/i86/BORLAND_MSVC_EXTENSIONS.md` - Borland and MSVC extensions
- `arch/i386/macdefs.h` - i386 memory model definitions
- `arch/i386/local2.c` - i386 backend implementation

## References

- Watcom C/C++ 10.6 Programmer's Guide
- Intel 80386 Programmer's Reference Manual
- Intel 64 and IA-32 Architectures Software Developer's Manual
- System V Application Binary Interface - Intel386 Architecture Processor Supplement
- System V Application Binary Interface - AMD64 Architecture Processor Supplement
- Apple macOS ABI Mach-O File Format Reference
- Wine/CrossOver source code (dlls/ntdll/unix/virtual.c, dlls/wow64/*.c)
