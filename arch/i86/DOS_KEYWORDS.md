# DOS-Era Compiler Keywords for i86

This document describes the DOS-era compiler keywords implemented for PCC's i86 backend, providing compatibility with Microsoft C, Watcom C, Borland C/C++, and Zortech C++ compilers from the DOS era.

## Overview

When compiling for the i86 architecture, PCC supports traditional DOS-era keywords for pointer types, calling conventions, function modifiers, and segment operations. These keywords work as aliases for `__attribute__((keyword))`.

## Pointer Type Modifiers

### `__far`
- **Usage**: `type __far *pointer`
- **Equivalent**: `__attribute__((far))`
- **Description**: Far pointer (segment:offset, 32-bit). Used in medium, compact, large, and huge memory models for pointers that can access data across segment boundaries.
- **Example**:
  ```c
  int __far *ptr;  // far pointer to int
  char __far *str; // far pointer to char
  ```

### `__near`
- **Usage**: `type __near *pointer`
- **Equivalent**: `__attribute__((near))`
- **Description**: Near pointer (offset only, 16-bit). Used for pointers within the current segment.
- **Example**:
  ```c
  int __near *ptr;  // near pointer to int (16-bit)
  ```

### `__huge`
- **Usage**: `type __huge *pointer`
- **Equivalent**: `__attribute__((huge))`
- **Description**: Huge pointer (normalized far pointer, 32-bit). Similar to far pointers but with normalized segment:offset representation for proper pointer arithmetic across segments.
- **Example**:
  ```c
  long __huge *bigarray;  // huge pointer for large arrays
  ```

### `__based`
- **Usage**: `type __based(segment) *pointer`
- **Equivalent**: `__attribute__((based))`
- **Description**: Based pointer - pointer relative to a specific segment register.
- **Example**:
  ```c
  int __based(__segname("_DATA")) *ptr;
  ```

### `__far16`
- **Usage**: `type __far16 *pointer` (Watcom C/C++)
- **Equivalent**: `__attribute__((far16))`
- **Description**: Watcom-specific 16-bit far pointer. Explicitly specifies 16-bit far addressing.
- **Example**:
  ```c
  void __far16 *video_mem;  // Far pointer to video memory
  ```

### `__segment`
- **Usage**: `__segment seg_type`
- **Equivalent**: `__attribute__((segment))`
- **Description**: Segment type specifier. Used to declare segment variables.
- **Example**:
  ```c
  __segment seg;
  seg = _segname("_CODE");
  ```

### `__self`
- **Usage**: `type __self *pointer`
- **Equivalent**: `__attribute__((self))`
- **Description**: Self-relative addressing. Pointer contains offset relative to itself.
- **Example**:
  ```c
  int __self *relptr;  // Self-relative pointer
  ```

## Calling Convention Modifiers

### `__cdecl`
- **Usage**: `type __cdecl function(...)`
- **Equivalent**: `__attribute__((cdecl))`
- **Description**: C calling convention (caller cleans stack). This is the default for C functions.
- **Example**:
  ```c
  int __cdecl myfunction(int a, int b);
  void __cdecl printf(const char *fmt, ...);
  ```

### `__pascal`
- **Usage**: `type __pascal function(...)`
- **Equivalent**: `__attribute__((pascal))`
- **Description**: Pascal calling convention (callee cleans stack, parameters pushed left-to-right).
- **Example**:
  ```c
  int __pascal PascalFunc(int a, int b);
  ```

### `__fortran`
- **Usage**: `type __fortran function(...)`
- **Equivalent**: `__attribute__((fortran))`
- **Description**: Fortran calling convention (similar to Pascal).
- **Example**:
  ```c
  void __fortran FortranSubroutine(int *a, int *b);
  ```

### `__syscall`
- **Usage**: `type __syscall function(...)` (Watcom C/C++)
- **Equivalent**: `__attribute__((syscall))`
- **Description**: Watcom system call convention. Used for OS/2 and Windows system calls.
- **Example**:
  ```c
  int __syscall DosRead(int handle, void *buf, int count);
  ```

### `__watcall`
- **Usage**: `type __watcall function(...)` (Watcom C/C++)
- **Equivalent**: `__attribute__((watcall))`
- **Description**: Watcom optimized register-based calling convention. Passes arguments in registers (EAX, EDX, EBX, ECX).
- **Example**:
  ```c
  int __watcall FastFunction(int a, int b, int c);
  ```

## Function Modifiers

### `__interrupt`
- **Usage**: `void __interrupt function(void)` or `void __interrupt(n) function(void)`
- **Equivalent**: `__attribute__((interrupt))` or `__attribute__((interrupt(n)))`
- **Description**: Declares an interrupt service routine (ISR). The function will save all registers, set up segment registers, and use IRET instead of RET. Optional interrupt vector number.
- **Example**:
  ```c
  void __interrupt timer_handler(void);
  void __interrupt(0x09) keyboard_handler(void);  // INT 09h handler
  ```
- **Behavior**:
  - Saves all registers on entry
  - Restores all registers on exit
  - Uses IRET (interrupt return) instruction
  - Sets up DS to DGROUP for small/medium models

### `__loadds`
- **Usage**: `type __loadds function(...)`
- **Equivalent**: `__attribute__((loadds))`
- **Description**: Forces the function to load DS register with DGROUP on entry. Used when calling from different data segments.
- **Example**:
  ```c
  int __far __loadds CallbackFunction(int param);
  ```
- **Use Cases**:
  - Windows callback functions (WndProc, DlgProc)
  - DLL exported functions
  - Functions called from different data segments

### `__saveregs`
- **Usage**: `type __saveregs function(...)`
- **Equivalent**: `__attribute__((saveregs))`
- **Description**: Save and restore all registers (AX, BX, CX, DX, SI, DI) in function prologue/epilogue.
- **Example**:
  ```c
  void __saveregs CriticalFunction(void);
  ```
- **Use Cases**:
  - Functions called from assembly
  - Signal handlers
  - Callback functions

### `__export`
- **Usage**: `type __export function(...)`
- **Equivalent**: `__attribute__((export))`
- **Description**: Marks function for export from a DLL. Equivalent to including in module definition file (.DEF).
- **Example**:
  ```c
  int __far __pascal __export LibraryFunction(int param);
  ```
- **Note**: Commonly combined with __far and __loadds for Windows DLLs

## Segment Register Keywords

### `__ss`, `__cs`, `__ds`, `__es`, `__fs`, `__gs`
- **Usage**: `__segment_reg variable` or in expressions
- **Equivalent**: `__attribute__((ss))`, `__attribute__((cs))`, etc.
- **Description**: Segment register specifiers for based pointers or segment operations.
- **Registers**:
  - `__ss` - Stack Segment
  - `__cs` - Code Segment
  - `__ds` - Data Segment
  - `__es` - Extra Segment
  - `__fs` - Additional Segment (80386+)
  - `__gs` - Additional Segment (80386+)
- **Example**:
  ```c
  // Access variable relative to ES segment
  int __es *ptr;

  // Based pointer using DS
  char __based(__ds) *str;
  ```

## Memory Models

These keywords work in conjunction with the memory model selected via `-mcmodel`:

| Memory Model | Code Pointers | Data Pointers | Common Usage |
|--------------|---------------|---------------|--------------|
| tiny         | near (16-bit) | near (16-bit) | .COM programs |
| small        | near (16-bit) | near (16-bit) | Default, < 64KB code + data |
| medium       | far (32-bit)  | near (16-bit) | Large code, small data |
| compact      | near (16-bit) | far (32-bit)  | Small code, large data |
| large        | far (32-bit)  | far (32-bit)  | Large code and data |
| huge         | far (32-bit)  | huge (32-bit) | Very large arrays |

## Compatibility

These keywords are enabled **only** when compiling for the i86 architecture (`#ifdef mach_i86`). They are not available for other architectures.

### Supported Syntax Forms

All keywords can be used in two equivalent ways:

1. **Direct keyword**: `__far`, `__near`, `__interrupt`, etc.
2. **Attribute form**: `__attribute__((far))`, `__attribute__((interrupt))`, etc.

### Complete Keyword List

**Pointer Type Modifiers:**
- `__far`, `__near`, `__huge`, `__based`, `__far16`, `__segment`, `__self`

**Calling Conventions:**
- `__cdecl`, `__pascal`, `__fortran`, `__syscall`, `__watcall`
- Also available: `__stdcall`, `__fastcall` (GCC-compatible)

**Function Modifiers:**
- `__interrupt`, `__loadds`, `__saveregs`, `__export`

**Segment Registers:**
- `__ss`, `__cs`, `__ds`, `__es`, `__fs`, `__gs`

### Example: Mixed Usage

```c
// DOS-style declarations
int __far *get_far_pointer(void);
void __pascal WindowProc(int msg, int param);
void __interrupt timer_handler(void);
int __far __loadds __export DllFunction(int x);

// Equivalent attribute-style declarations
int __attribute__((far)) *get_far_pointer(void);
void __attribute__((pascal)) WindowProc(int msg, int param);
void __attribute__((interrupt)) timer_handler(void);
int __attribute__((far)) __attribute__((loadds))
    __attribute__((export)) DllFunction(int x);

// Watcom-specific
int __watcall FastFunc(int a, int b);
void __far16 *GetVideoMem(void);
```

## Implementation Notes

1. These keywords are implemented as extensions to GCC's `__attribute__` mechanism
2. They are processed during the lexical analysis phase and converted to attributes
3. The actual behavior (pointer size, calling convention) is implemented in the i86 backend
4. Conditional compilation ensures they only exist for i86 targets

## Historical Context

These keywords were essential for DOS and early Windows programming when dealing with:

### Memory Management
- **Segmented memory architecture**: 8086/80286 CPUs used segment:offset addressing
- **Memory constraints**: 64KB segment limits required careful pointer management
- **TSR programs**: Terminate-and-stay-resident programs needed far pointers
- **Expanded/Extended memory**: EMS/XMS drivers required far pointers

### Calling Conventions
- **Inter-language calls**: Pascal and Fortran libraries required different calling conventions
- **Windows API**: Windows 3.x used __pascal convention for most APIs
- **DLL development**: __export and __loadds were required for DLL functions
- **Assembly integration**: __cdecl vs __pascal determined stack cleanup responsibility

### Interrupt Handling
- **Hardware interrupts**: Keyboard (INT 09h), timer (INT 08h), mouse (INT 33h)
- **Software interrupts**: DOS services (INT 21h), BIOS services (INT 10h, 13h, 16h)
- **Custom ISRs**: Game development, real-time systems, device drivers
- **Protected mode**: OS/2 and Windows 3.x interrupt handlers

### Platform-Specific Use
- **Microsoft C/C++**: Used for MS-DOS, Windows 3.x development
- **Borland C/C++**: Turbo C, Turbo C++, Borland C++ - very popular for games
- **Watcom C/C++**: Known for optimizing compiler, used for games (DOOM, Duke Nukem 3D)
- **Zortech C++**: Early C++ compiler, later became Symantec C++

### Real-World Examples
```c
// Windows 3.x callback function
LRESULT __far __pascal __export WndProc(HWND hwnd, UINT msg,
                                         WPARAM wParam, LPARAM lParam);

// DOS TSR keyboard interrupt handler
void __interrupt __far keyboard_isr(void);

// Watcom game engine optimized function
int __watcall RenderSprite(int x, int y, sprite_t *spr);

// Direct video memory access (CGA/EGA/VGA)
unsigned char __far *video = (unsigned char __far *)0xB8000000L;

// DLL export for Windows 3.x
int __far __pascal __loadds __export MessageBox(HWND hwnd,
                                                  LPCSTR text,
                                                  LPCSTR caption,
                                                  UINT type);
```

## See Also

- `BORLAND_MSVC_EXTENSIONS.md` - Borland and MSVC extensions (__emit, __asm, register pseudo-variables)
- `arch/i86/macdefs.h` - Memory model definitions
- `arch/i86/local2.c` - Memory model implementation
- `cc/ccom/pass1.h` - Attribute definitions
- `cc/ccom/gcc_compat.c` - Keyword and attribute handling
