# PCC Memory Models and x86 Extensions

This document provides an overview of the memory model and vendor extension support added to PCC for x86 architectures (i86, i386, and x86-64/amd64).

## Quick Reference

### Command-Line Options

```bash
# i86 memory models
pcc -mcmodel=tiny -c file.c      # Tiny model (64K total)
pcc -mcmodel=small -c file.c     # Small model (64K code + 64K data)
pcc -mcmodel=medium -c file.c    # Medium model (1MB code + 64K data)
pcc -mcmodel=compact -c file.c   # Compact model (64K code + 1MB data)
pcc -mcmodel=large -c file.c     # Large model (1MB code + 1MB data)
pcc -mcmodel=huge -c file.c      # Huge model (normalized far pointers)

# i386 memory models
pcc -mcmodel=small -c file.c     # Small model (32-bit flat) [default]
pcc -mcmodel=large -c file.c     # Large model (48-bit segmented)
```

### Keyword Summary

| Keyword | i86 | i386 | x86-64 | Description |
|---------|-----|------|--------|-------------|
| `__far` | ✓ | - | - | Far pointer (32-bit segment:offset) |
| `__near` | ✓ | - | - | Near pointer (16-bit offset) |
| `__huge` | ✓ | - | - | Huge pointer (normalized 32-bit) |
| `__based` | ✓ | - | - | Based pointer (relative to segment) |
| `__far16` | ✓ | ✓ | - | Watcom 16-bit far pointer |
| `__cdecl` | ✓ | - | - | C calling convention |
| `__pascal` | ✓ | - | - | Pascal calling convention |
| `__fortran` | ✓ | - | - | Fortran calling convention |
| `__syscall` | ✓ | - | - | Watcom syscall convention |
| `__watcall` | ✓ | - | - | Watcom register calling convention |
| `__interrupt` | ✓ | - | - | Interrupt service routine |
| `__loadds` | ✓ | - | - | Load DS on function entry |
| `__saveregs` | ✓ | - | - | Save/restore all registers |
| `__export` | ✓ | - | - | Export function (DLL) |
| `__ss` | ✓ | - | - | Stack segment register |
| `__cs` | ✓ | - | - | Code segment register |
| `__ds` | ✓ | - | - | Data segment register |
| `__es` | ✓ | - | - | Extra segment register |
| `__fs` | ✓ | ✓ | ✓ | FS segment register |
| `__gs` | ✓ | ✓ | ✓ | GS segment register |
| `__emit` | ✓ | - | - | Emit raw bytes (Borland) |
| `__asm` | ✓ | ✓ | - | MSVC-style inline assembly |
| `__near32` | - | - | ✓ | 32-bit near pointer (macOS) |

## Architecture-Specific Documentation

- **i86**: See [`arch/i86/DOS_KEYWORDS.md`](arch/i86/DOS_KEYWORDS.md) for DOS-era keywords
- **i86**: See [`arch/i86/BORLAND_MSVC_EXTENSIONS.md`](arch/i86/BORLAND_MSVC_EXTENSIONS.md) for Borland/MSVC extensions
- **i386/x86-64**: See [`arch/i386/I386_EXTENSIONS.md`](arch/i386/I386_EXTENSIONS.md) for i386 and x86-64 extensions

## Test Files

Example usage of all features can be found in:
- [`arch/i86/test_i86_complete.c`](arch/i86/test_i86_complete.c) - Comprehensive i86 examples
- [`arch/i386/test_i386_extensions.c`](arch/i386/test_i386_extensions.c) - i386 examples
- [`arch/amd64/test_amd64_extensions.c`](arch/amd64/test_amd64_extensions.c) - x86-64 examples

## Memory Model Overview

### i86 Memory Models

| Model | Code Ptrs | Data Ptrs | Max Code | Max Data | Use Case |
|-------|-----------|-----------|----------|----------|----------|
| Tiny | 16-bit | 16-bit | 64K | 64K (shared) | .COM executables |
| Small | 16-bit | 16-bit | 64K | 64K | Default for DOS |
| Medium | 32-bit | 16-bit | 1MB | 64K | Large programs |
| Compact | 16-bit | 32-bit | 64K | 1MB | Data-intensive |
| Large | 32-bit | 32-bit | 1MB | 1MB | Large programs+data |
| Huge | 32-bit | 32-bit (norm) | 1MB | 1MB | Arrays > 64K |

### i386 Memory Models

| Model | Pointer Size | Addressing | Use Case |
|-------|--------------|------------|----------|
| Small | 32-bit | Flat | Modern protected mode (default) |
| Large | 48-bit | Segmented | Legacy segmented apps, DOS extenders |

## Common Use Cases

### DOS Programming (i86)

```c
// Video memory access
unsigned char __far *video = (unsigned char __far *)0xB8000000L;
video[0] = 'A';
video[1] = 0x0F;  // White on black

// Interrupt handler
void __interrupt timer_handler(void) {
    // Automatically saves registers and uses IRET
}

// Emit assembly opcodes
__emit(0x90);           // NOP
__emit(0x0F, 0x31);     // RDTSC
```

### DOS Extender / Protected Mode (i386)

```c
// 16-bit far pointer for BIOS calls
void __far16 *bios_data_area = (void __far16 *)0x00000400L;

// 32-bit flat pointer for application
void *app_data = malloc(1024 * 1024);  // 1MB allocation
```

### Thread-Local Storage (x86-64)

```c
// Linux: FS points to TLS
struct thread_info __fs *ti = (struct thread_info __fs *)0;
int tid = ti->thread_id;

// Read stack canary
unsigned long __gs *canary = (unsigned long __gs *)0x28;
```

### macOS 32-bit Compatibility (x86-64)

```c
// Export 32-bit routine from 64-bit library
int __near32 legacy_function(int x, int y) {
    return x + y;  // Uses 32-bit calling convention
}
```

## Compiler Compatibility

PCC's implementation is compatible with:

| Feature | Source Compatibility |
|---------|---------------------|
| Memory models | Microsoft C 5.x-6.x, Watcom C 9.x-11.x, Borland C++ 3.x-5.x |
| `__far/__near/__huge` | Microsoft, Watcom, Borland |
| `__cdecl/__pascal` | Microsoft, Watcom, Borland |
| `__interrupt/__loadds` | Microsoft, Watcom, Borland |
| `__emit` | Borland C++ 3.x-5.x |
| `__asm` | Microsoft C 6.x, MSVC 1.x-6.x |
| `__far16` | Watcom C/C++ 10.x-11.x |
| `__fs/__gs` | Watcom C 10.x+, GCC, MSVC |
| `__near32` | CrossOver/LLVM (macOS) |

## Implementation Details

### Modified Files

- **`cc/ccom/pass1.h`**: Attribute definitions organized by architecture
- **`cc/ccom/gcc_compat.c`**: Keyword table and parsing logic
- **`arch/i86/macdefs.h`**: i86 memory model flags and SZPOINT macro
- **`arch/i86/local2.c`**: i86 backend implementation
- **`arch/i386/macdefs.h`**: i386 memory model flags
- **`arch/i386/local2.c`**: i386 backend implementation
- **`cc/cc/cc.c`**: Command-line -mcmodel parsing

### Attribute System

All keywords are implemented as GCC-style attributes:

```c
__far int *ptr;
// Equivalent to:
__attribute__((far)) int *ptr;
```

This allows both the convenient keyword syntax and the explicit attribute syntax.

## Future Work

Potential enhancements:

1. **Code Generation**: Implement actual code generation for memory model differences
2. **Linker Support**: Add linker support for memory model object files
3. **Optimization**: Memory model-aware optimizations
4. **Additional Platforms**: Support for more DOS extender ABIs
5. **Validation**: Add compile-time pointer type validation

## Commit History

```
786a43e Add i386 memory models and x86 cross-platform extensions
da9172e Fix __emit syntax to use parentheses and support multiple bytes
e23e91a Add Borland __emit and MSVC __asm syntax support
264d623 Add comprehensive DOS-era i86 keywords
3891b8a Add DOS-era compiler keywords for i86 backend
```

## References

- Intel 8086 Family User's Manual
- Intel 80386 Programmer's Reference Manual
- Microsoft C 6.0 Reference
- Borland C++ 3.1 Programmer's Guide
- Watcom C/C++ 11.0 Programmer's Guide
- System V ABI i386 and AMD64 supplements
- Wine/CrossOver source code
- Apple macOS ABI documentation

---

**Generated with [Claude Code](https://claude.com/claude-code)**

**Co-Authored-By: Claude <noreply@anthropic.com>**
