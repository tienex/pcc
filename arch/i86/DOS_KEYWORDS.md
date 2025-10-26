# DOS-Era Compiler Keywords for i86

This document describes the DOS-era compiler keywords implemented for PCC's i86 backend, providing compatibility with Microsoft C, Watcom C, and Borland C/C++ compilers from the DOS era.

## Overview

When compiling for the i86 architecture, PCC supports traditional DOS-era keywords for pointer types and calling conventions. These keywords work as aliases for `__attribute__((keyword))`.

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

1. **Direct keyword**: `__far`, `__near`, `__huge`, etc.
2. **Attribute form**: `__attribute__((far))`, `__attribute__((near))`, etc.

### Example: Mixed Usage

```c
// DOS-style declaration
int __far *get_far_pointer(void);

// Equivalent attribute-style declaration
int __attribute__((far)) *get_far_pointer(void);

// Function with calling convention
void __pascal WindowProc(int msg, int param);

// Equivalent
void __attribute__((pascal)) WindowProc(int msg, int param);
```

## Implementation Notes

1. These keywords are implemented as extensions to GCC's `__attribute__` mechanism
2. They are processed during the lexical analysis phase and converted to attributes
3. The actual behavior (pointer size, calling convention) is implemented in the i86 backend
4. Conditional compilation ensures they only exist for i86 targets

## Historical Context

These keywords were essential for DOS programming when dealing with:
- **Segmented memory architecture**: 8086/80286 CPUs used segment:offset addressing
- **Memory constraints**: 64KB segment limits required careful pointer management
- **Inter-language calls**: Pascal and Fortran libraries required different calling conventions
- **TSR programs**: Terminate-and-stay-resident programs needed far pointers
- **Windows 3.x programming**: Required far pointers for cross-segment access

## See Also

- `arch/i86/macdefs.h` - Memory model definitions
- `arch/i86/local2.c` - Memory model implementation
- `cc/ccom/pass1.h` - Attribute definitions
- `cc/ccom/gcc_compat.c` - Keyword and attribute handling
