# Borland and MSVC Extensions for i86/i386

This document describes Borland C/C++ and Microsoft Visual C++ compiler extensions implemented in PCC for the i86 and i386 backends.

## Overview

PCC supports several vendor-specific extensions that were common in DOS and early Windows development:

1. **`__emit`** - Emit raw bytes into the code stream (Borland/MSVC)
2. **MSVC `__asm`** - Microsoft-style inline assembly syntax
3. **Borland Register Pseudo-Variables** - Direct access to CPU registers (planned)

These features are available for both **i86** and **i386** architectures.

---

## `__emit` - Raw Byte Emission

### Description

The `__emit` keyword allows you to insert raw byte values directly into the generated code. This is useful for:
- Emitting specific machine instructions not supported by the compiler
- Creating custom opcodes
- Inserting padding or alignment bytes
- Implementing low-level optimizations

### Syntax

**Borland/MSVC syntax:**
```c
__emit(byte_value)                    // Single byte
__emit(byte1, byte2, byte3, ...)     // Multiple bytes (Borland)
__emit__(byte_value)                  // Alternative form
```

**GCC-style attribute form:**
```c
__attribute__((emit(byte_value)))
__attribute__((emit(byte1, byte2, ...)))
```

**PCC Extension:** Unlike the original Borland/MSVC implementations which required literal constants, PCC also accepts expressions and variables as arguments, allowing for more flexible code generation.

### Examples

```c
// Emit NOP instruction (0x90)
void insert_nop(void)
{
    __emit(0x90);
}

// Emit INT 3 instruction (debugger breakpoint: 0xCC)
void debug_break(void)
{
    __emit(0xCC);
}

// Emit multiple bytes in one statement (Borland-style)
void custom_instruction(void)
{
    __emit(0x0F, 0x31);  // RDTSC instruction (Read Time-Stamp Counter)
}

// Multiple separate calls
void rdtsc_alt(void)
{
    __emit(0x0F);  // Two-byte opcode prefix
    __emit(0x31);  // RDTSC second byte
}

// Using attribute form
void nop_sled(void)
{
    __attribute__((emit(0x90, 0x90, 0x90)));  // Three NOPs at once
}

// PCC extension: Using variables (not available in original Borland/MSVC)
void emit_dynamic(unsigned char opcode)
{
    __emit(opcode);  // Emit byte from variable
}

// PCC extension: Complex expressions
void emit_calculated(void)
{
    __emit(0x90 + 0);  // Expression evaluation
}
```

### i86 Examples

```c
// Emit far return (RETF)
void far_return_stub(void)
{
    __emit(0xCB);  // RETF instruction
}

// Emit segment override prefix
void es_override(void)
{
    __emit(0x26, 0xA1);  // ES: MOV AX, [...]
    // ... rest of instruction
}

// Lock prefix for atomic operations
void atomic_inc(void)
{
    __emit(0xF0, 0xFF, 0x06);  // LOCK INC [mem]
    // ... address follows
}

// Multiple instructions
void int_sequence(void)
{
    __emit(0xCD, 0x21);  // INT 21h (DOS function call)
}

// Segment register manipulation
void set_es_to_video(void)
{
    __emit(0xB8, 0x00, 0xB8);  // MOV AX, 0B800h (VGA text mode)
    __emit(0x8E, 0xC0);         // MOV ES, AX
}
```

### i386 Examples

```c
// Emit CPUID instruction
void emit_cpuid(void)
{
    __emit(0x0F, 0xA2);  // CPUID
}

// Emit RDTSC (Read Time-Stamp Counter)
void emit_rdtsc(void)
{
    __emit(0x0F, 0x31);  // RDTSC
}

// Emit PREFETCH instruction (Pentium III+)
void emit_prefetch(void)
{
    __emit(0x0F, 0x18, 0x00);  // PREFETCH [EAX]
}

// WBINVD instruction (486+)
void writeback_invalidate(void)
{
    __emit(0x0F, 0x09);  // WBINVD
}

// INVD instruction (486+)
void invalidate_cache(void)
{
    __emit(0x0F, 0x08);  // INVD
}
```

### Use Cases

1. **Debugging**: Insert breakpoint instructions (`__emit(0xCC)`)
2. **Optimization**: Insert specific CPU instructions for performance
3. **Compatibility**: Work around compiler code generation issues
4. **Assembly Tricks**: Implement specific instruction sequences
5. **Anti-debugging**: Insert unusual instruction sequences
6. **Instruction prefixes**: LOCK, REP, segment overrides
7. **Privileged instructions**: System-level operations (I/O, cache control)
8. **Dynamic code generation**: Emit bytes based on runtime calculations (PCC extension)

### Compatibility

**Original Borland/MSVC Syntax:**
- **Borland C/C++ 3.x-5.x**:
  - `__emit(byte)` - single byte (required literal)
  - `__emit(b1, b2, ...)` - multiple bytes (required literals)
- **Microsoft C/C++ 6.x-7.x**:
  - `__emit byte` - single byte (required literal)

**PCC Implementation:**
- ✓ `__emit(byte)` - Borland single byte syntax
- ✓ `__emit(b1, b2, ...)` - Borland multiple byte syntax
- ✓ `__emit__(byte)` - Alternative form
- ✓ `__attribute__((emit(byte, ...)))` - GCC-style attribute
- ✓ **PCC Extension**: Accepts variables and expressions (not just literals)

### Notes

- The emitted bytes are inserted directly into the code stream at the current position
- No instruction validation is performed - you are responsible for valid opcodes
- Multi-byte instructions can be emitted in a single `__emit()` call
- Be careful with instruction alignment and CPU compatibility
- **PCC Extension**: Unlike Borland/MSVC, PCC allows non-constant expressions as arguments, enabling dynamic opcode generation

---

## MSVC-Style `__asm` - Inline Assembly

### Description

Microsoft Visual C++ uses a different inline assembly syntax compared to GCC. PCC supports the MSVC `__asm` keyword for compatibility with MSVC and Borland codebases.

### Syntax

**Single instruction:**
```c
__asm instruction
```

**Multiple instructions (block form):**
```c
__asm {
    instruction1
    instruction2
    ...
}
```

**Compared to GCC syntax:**
```c
// GCC style (also supported)
asm("instruction");
asm volatile("instruction");

// MSVC style (this implementation)
__asm instruction
__asm { instruction }
```

### Examples

#### Basic Usage

```c
// Single instruction
void set_interrupt_flag(void)
{
    __asm sti
}

void clear_interrupt_flag(void)
{
    __asm cli
}

// Multiple instructions
void save_context(void)
{
    __asm {
        pushad      ; Push all general-purpose registers
        pushfd      ; Push flags
    }
}
```

#### i86 Examples

```c
// Read/write I/O ports
unsigned char read_port(unsigned int port)
{
    unsigned char value;
    __asm {
        mov dx, port
        in al, dx
        mov value, al
    }
    return value;
}

void write_port(unsigned int port, unsigned char value)
{
    __asm {
        mov dx, port
        mov al, value
        out dx, al
    }
}

// Disable interrupts and halt
void halt_cpu(void)
{
    __asm {
        cli         ; Clear interrupt flag
        hlt         ; Halt processor
    }
}

// Load segment registers
void set_data_segment(unsigned int seg)
{
    __asm {
        mov ax, seg
        mov ds, ax
        mov es, ax
    }
}
```

#### i386 Examples

```c
// Read CR0 register (protected mode)
unsigned long read_cr0(void)
{
    unsigned long value;
    __asm {
        mov eax, cr0
        mov value, eax
    }
    return value;
}

// Write CR3 register (page directory base)
void write_cr3(unsigned long value)
{
    __asm {
        mov eax, value
        mov cr3, eax
    }
}

// Atomic compare-and-swap
int atomic_cas(int *ptr, int old_val, int new_val)
{
    int result;
    __asm {
        mov esi, ptr
        mov eax, old_val
        mov ebx, new_val
        lock cmpxchg [esi], ebx
        mov result, eax
    }
    return result;
}

// CPUID instruction
void get_cpu_info(unsigned int *eax, unsigned int *ebx,
                  unsigned int *ecx, unsigned int *edx)
{
    __asm {
        mov eax, [eax]
        cpuid
        mov esi, eax
        mov [esi], eax
        mov esi, ebx
        mov [esi], ebx
        mov esi, ecx
        mov [esi], ecx
        mov esi, edx
        mov [esi], edx
    }
}
```

### Features

- **Direct register access**: Use register names directly (AX, BX, EAX, etc.)
- **Memory references**: Access C variables by name
- **No constraints needed**: Unlike GCC asm, no input/output/clobber specifications
- **Natural syntax**: Looks like standalone assembly language
- **Comments**: Use `;` for assembly comments

### Differences from GCC `asm`

| Feature | GCC `asm` | MSVC `__asm` |
|---------|-----------|--------------|
| Syntax | String-based | Direct assembly |
| Register constraints | Required | Not needed |
| Memory constraints | Required | Not needed |
| Clobber list | Required | Automatic |
| Comments | None (in string) | `;` supported |
| Multiple instructions | `\n` in string | Newlines or `;` |
| Block form | Not available | `{ }` braces |

### Example Comparison

```c
// GCC style
int gcc_atomic_inc(int *ptr)
{
    int result;
    asm volatile(
        "lock incl %1\n\t"
        "movl %1, %0"
        : "=r" (result)
        : "m" (*ptr)
        : "memory"
    );
    return result;
}

// MSVC style
int msvc_atomic_inc(int *ptr)
{
    int result;
    __asm {
        mov esi, ptr
        lock inc dword ptr [esi]
        mov eax, [esi]
        mov result, eax
    }
    return result;
}
```

### Limitations

- Function calls within `__asm` blocks may not be portable
- Floating-point operations require explicit FPU instructions
- Stack manipulation must preserve frame pointer
- Some optimizations may be disabled around `__asm` blocks

### Compatibility

- **Microsoft Visual C++**: Full `__asm` support (16-bit and 32-bit)
- **Borland C/C++**: Full `__asm` support
- **Watcom C/C++**: Supports `__asm` with slight variations
- **PCC**: Supports `__asm` keyword, block form planned

---

## Borland Register Pseudo-Variables (Planned)

### Description

Borland C/C++ provided direct access to CPU registers through pseudo-variables. These allow reading and writing registers as if they were regular C variables.

**Note**: This feature is planned but not yet fully implemented in PCC. The infrastructure is in place for future support.

### i86 Register Pseudo-Variables

#### 16-bit General Purpose Registers
```c
unsigned int _AX;    // AX register
unsigned int _BX;    // BX register
unsigned int _CX;    // CX register
unsigned int _DX;    // DX register
unsigned int _SI;    // Source Index
unsigned int _DI;    // Destination Index
unsigned int _BP;    // Base Pointer
unsigned int _SP;    // Stack Pointer
```

#### 8-bit Register Halves
```c
unsigned char _AL;   // AL (low byte of AX)
unsigned char _AH;   // AH (high byte of AX)
unsigned char _BL;   // BL (low byte of BX)
unsigned char _BH;   // BH (high byte of BX)
unsigned char _CL;   // CL (low byte of CX)
unsigned char _CH;   // CH (high byte of CX)
unsigned char _DL;   // DL (low byte of DX)
unsigned char _DH;   // DH (high byte of DX)
```

#### Segment Registers
```c
unsigned int _CS;    // Code Segment
unsigned int _DS;    // Data Segment
unsigned int _ES;    // Extra Segment
unsigned int _SS;    // Stack Segment
```

#### Flags Register
```c
unsigned int _FLAGS; // Flags register
```

### i386 Register Pseudo-Variables

#### 32-bit General Purpose Registers
```c
unsigned long _EAX;  // EAX register
unsigned long _EBX;  // EBX register
unsigned long _ECX;  // ECX register
unsigned long _EDX;  // EDX register
unsigned long _ESI;  // Source Index
unsigned long _EDI;  // Destination Index
unsigned long _EBP;  // Base Pointer
unsigned long _ESP;  // Stack Pointer
```

#### Additional Segment Registers (386+)
```c
unsigned int _FS;    // Additional segment register
unsigned int _GS;    // Additional segment register
```

### Planned Usage Examples

```c
// i86 examples
void set_ax_to_zero(void)
{
    _AX = 0;  // Set AX register to 0
}

unsigned int get_flags(void)
{
    return _FLAGS;  // Read flags register
}

void set_data_segment(unsigned int seg)
{
    _DS = seg;  // Set DS register
}

// i386 examples
void clear_eax(void)
{
    _EAX = 0;  // Clear EAX register
}

unsigned long get_stack_pointer(void)
{
    return _ESP;  // Read current stack pointer
}

void save_all_regs(unsigned long *buf)
{
    buf[0] = _EAX;
    buf[1] = _EBX;
    buf[2] = _ECX;
    buf[3] = _EDX;
    buf[4] = _ESI;
    buf[5] = _EDI;
    buf[6] = _EBP;
    buf[7] = _ESP;
}
```

### Implementation Status

- **Attributes defined**: Yes (infrastructure ready)
- **Keywords defined**: Not yet
- **Parser support**: Not yet
- **Code generation**: Not yet
- **Target**: Future enhancement

### Historical Context

Register pseudo-variables were popular in:
- **Borland Turbo C/C++**: Primary feature for low-level programming
- **MS-DOS development**: Quick register access without assembly
- **Embedded systems**: Direct hardware control
- **Interrupt handlers**: Fast context save/restore

---

## Platform Support

| Feature | i86 | i386 | Notes |
|---------|-----|------|-------|
| `__emit(...)` | ✓ | ✓ | Multiple bytes, PCC extension for non-literals |
| `__asm` (MSVC-style) | ✓ | ✓ | Block form `{ }` supported |
| Register pseudo-vars | Planned | Planned | Future enhancement |

## Compiler Compatibility Matrix

| Compiler | `__emit` | `__asm` | Register Vars |
|----------|----------|---------|---------------|
| Borland C/C++ 3.x-5.x | ✓ | ✓ | ✓ |
| Microsoft C/C++ 6.x-7.x | ✓ | ✓ | ✗ |
| Watcom C/C++ 10.x-11.x | ✓ | ✓ | ✗ |
| PCC (this impl) | ✓ | ✓ | Planned |

## See Also

- `DOS_KEYWORDS.md` - DOS-era compiler keywords
- `arch/i86/macdefs.h` - i86 architecture definitions
- `arch/i386/macdefs.h` - i386 architecture definitions
- `cc/ccom/gcc_compat.c` - GCC compatibility layer

## References

- Borland C++ 3.1 Programmer's Guide
- Microsoft C/C++ 7.0 Language Reference
- Watcom C/C++ 10.6 User's Guide
- Intel 8086 Family User's Manual
- Intel 80386 Programmer's Reference Manual
