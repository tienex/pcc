# Watcom Pragma Support in PCC

This document describes the Watcom-compatible pragma directives supported by PCC.

## Overview

PCC now supports a comprehensive set of Watcom C/C++ pragma directives, enhancing compatibility with code originally written for the Watcom compiler. Most pragmas are accepted and parsed correctly, though some may not have full implementation depending on the target architecture.

## Supported Pragmas

### 1. `#pragma aux` - Function Calling Conventions

Defines custom calling conventions and register usage for functions. This is one of the most powerful features, allowing you to create reusable calling conventions and apply them to functions.

**Syntax:**
```c
// Define and apply inline:
#pragma aux symbol [parm [reg...]] [value [reg]] [modify [reg...]] [caller|routine] [loadds] [export]

// Define a named calling convention:
#pragma aux __ConventionName parm [...] value [...] modify [...] caller|routine

// Apply a calling convention to a function:
#pragma aux (__ConventionName) function_name
```

**Keywords:**
- `parm [reg1] [reg2] ...` - Specifies registers for parameters
- `value [reg]` - Specifies register for return value
- `modify [reg1 reg2 ...]` - Lists registers modified by function
- `caller` - Caller cleans up the stack (like `__cdecl`)
- `routine` - Callee cleans up the stack (like `__stdcall`)
- `loadds` - Load DS register (16-bit compatibility)
- `export` - Mark function for export

**Predefined Calling Conventions (x86/x64):**
- `__cdecl` - C declaration (caller cleanup, stack parameters)
- `__stdcall` - Standard call (callee cleanup, stack parameters)
- `__fastcall` - Fast call (first two params in ECX/EDX, callee cleanup)
- `__thiscall` - This call (this in ECX, callee cleanup, C++ methods)
- `__vectorcall` - Vector call (uses vector registers, x64)
- `_System` - System calling convention
- `_Optlink` - IBM Optlink convention
- `_Syscall` - System call convention

**Examples:**

**Basic inline convention:**
```c
// Fast calling convention using registers
#pragma aux fast_add parm [eax] [ebx] value [eax]
int fast_add(int a, int b);

// Specify modified registers
#pragma aux multiply parm [ecx] [edx] value [eax] modify [eax ebx]
int multiply(int x, int y);

// Caller cleanup
#pragma aux caller_func parm [eax] caller
void caller_func(int arg);

// DLL export
#pragma aux dll_export loadds export parm [eax] value [eax]
int dll_export(int param);
```

**Defining and using custom calling conventions:**
```c
// Define a custom calling convention
#pragma aux __MyFast parm [eax] [ebx] [ecx] value [eax] modify [eax ebx ecx edx] caller

// Apply it to multiple functions
#pragma aux (__MyFast) my_function1
#pragma aux (__MyFast) my_function2
#pragma aux (__MyFast) my_function3

int my_function1(int a, int b, int c);
int my_function2(int x, int y, int z);
int my_function3(int p, int q, int r);
```

**Using predefined conventions:**
```c
// Use __cdecl convention
#pragma aux (__cdecl) standard_function
int standard_function(int a, int b);

// Use __fastcall convention
#pragma aux (__fastcall) fast_function
int fast_function(int a, int b);

// Use __stdcall convention
#pragma aux (__stdcall) api_function
void api_function(int param);
```

**Complex example:**
```c
// Define a high-performance math convention
#pragma aux __MathOp parm [eax] [ebx] [ecx] value [eax] modify [eax ebx ecx edx] routine

// Apply to math functions
#pragma aux (__MathOp) add_fast
#pragma aux (__MathOp) sub_fast
#pragma aux (__MathOp) mul_fast

int add_fast(int a, int b, int c);
int sub_fast(int a, int b, int c);
int mul_fast(int a, int b, int c);
```

**Architecture Support:**
- **i86**: Maps caller/callee cleanup to stdcall flag
- **i386/amd64**: Accepts pragma, can be extended for register allocation
- **Other architectures**: Accepts pragma gracefully

**Notes:**
- Calling conventions starting with `__` or `_[A-Z]` are automatically registered as reusable conventions
- You can define unlimited custom calling conventions (up to 64)
- Conventions are looked up at pragma processing time
- Invalid convention names produce a warning

---

### 2. `#pragma comment` - Object File Comments

Embeds comments or directives into the object file.

**Syntax:**
```c
#pragma comment(type, "string")
```

**Types:**
- `compiler` - Compiler identification
- `lib` - Library to link
- `linker` - Linker directives
- `user` - User-defined comment

**Examples:**
```c
#pragma comment(compiler, "PCC with Watcom support")
#pragma comment(lib, "advapi32")
#pragma comment(linker, "/SUBSYSTEM:CONSOLE")
```

---

### 3. `#pragma message` - Compilation Messages

Prints a message during compilation.

**Syntax:**
```c
#pragma message("text")
```

**Example:**
```c
#pragma message("Compiling module XYZ")
```

**Output:**
```
#pragma message: Compiling module XYZ
```

---

### 4. `#pragma warning` - Warning Control

Controls compiler warning messages.

**Syntax:**
```c
#pragma warning(push)
#pragma warning(pop)
#pragma warning(disable : num1 num2 ...)
#pragma warning(enable : num1 num2 ...)
```

**Examples:**
```c
#pragma warning(disable : 123 456)
int code_with_warnings(void);
#pragma warning(enable : 123)
```

---

### 5. `#pragma once` - Include Guard

Ensures the header file is included only once.

**Syntax:**
```c
#pragma once
```

**Example:**
```c
// myheader.h
#pragma once

struct MyStruct {
    int value;
};
```

**Note:** This is a preprocessor directive and requires preprocessor integration.

---

### 6. `#pragma intrinsic` - Intrinsic Functions

Instructs the compiler to use intrinsic implementations for specified functions.

**Syntax:**
```c
#pragma intrinsic(func1, func2, ...)
```

**Example:**
```c
#pragma intrinsic(memcpy, memset, strlen)
```

---

### 7. `#pragma function` - Disable Intrinsics

Disables intrinsic implementations for specified functions.

**Syntax:**
```c
#pragma function(func1, func2, ...)
```

**Example:**
```c
#pragma function(strcpy, strcmp)
```

---

### 8. `#pragma code_seg` / `#pragma data_seg` - Segment Control

Controls code and data segment placement.

**Syntax:**
```c
#pragma code_seg("segment_name")
#pragma data_seg("segment_name")
#pragma code_seg()  // Reset to default
#pragma data_seg()  // Reset to default
```

**Examples:**
```c
#pragma code_seg("INIT_CODE")
void init_function(void) {
    // Placed in INIT_CODE segment
}
#pragma code_seg()

#pragma data_seg("SHARED_DATA")
int shared_var = 0;
#pragma data_seg()
```

---

### 9. `#pragma alloc_text` - Function Segment Allocation

Allocates a specific function to a named text segment.

**Syntax:**
```c
#pragma alloc_text(segment, function)
```

**Example:**
```c
#pragma alloc_text(INIT, early_init)
void early_init(void);
```

---

### 10. `#pragma inline_depth` - Inline Expansion Depth

Sets the maximum depth for inline function expansion.

**Syntax:**
```c
#pragma inline_depth(n)
```

**Example:**
```c
#pragma inline_depth(8)
inline int compute(int x) { return x * 2; }
```

---

### 11. `#pragma inline_recursion` - Recursive Inlining

Enables or disables recursive function inlining.

**Syntax:**
```c
#pragma inline_recursion(on|off)
```

**Example:**
```c
#pragma inline_recursion(on)
```

---

### 12. `#pragma auto_inline` - Automatic Inlining

Controls automatic inlining of functions.

**Syntax:**
```c
#pragma auto_inline(on|off)
```

**Example:**
```c
#pragma auto_inline(on)
```

---

### 13. `#pragma enum` - Enum Size Control

Controls the size of enumeration types.

**Syntax:**
```c
#pragma enum int|minimum|original|pop
```

**Options:**
- `int` - Force enums to int size
- `minimum` - Use minimum required size
- `original` - Restore original setting
- `pop` - Pop from enum setting stack

**Example:**
```c
#pragma enum int
enum Status {
    OK,
    ERROR,
    PENDING
};
```

---

### 14. `#pragma read_only_file` - Read-Only File Marker

Marks the current file as read-only for dependency tracking.

**Syntax:**
```c
#pragma read_only_file
```

---

### 15. `#pragma library` - Library Specification

Specifies a library to link with.

**Syntax:**
```c
#pragma library("libname")
```

**Example:**
```c
#pragma library("advapi32")
#pragma library("user32")
```

---

### 16. `#pragma include_alias` - Include File Aliasing

Creates an alias for include files.

**Syntax:**
```c
#pragma include_alias("alias", "realfile")
```

**Example:**
```c
#pragma include_alias("myheader.h", "real_header.h")
```

**Note:** Requires preprocessor integration.

---

### 17. `#pragma error` - Error Messages

Emits a custom error message and stops compilation.

**Syntax:**
```c
#pragma error "message"
```

**Example:**
```c
#ifndef REQUIRED_DEFINE
#pragma error "REQUIRED_DEFINE must be defined"
#endif
```

---

### 18. `#pragma disable_message` / `#pragma enable_message`

Disables or enables specific compiler messages.

**Syntax:**
```c
#pragma disable_message(num1, num2, ...)
#pragma enable_message(num1, num2, ...)
```

**Example:**
```c
#pragma disable_message(201, 202, 203)
int test_code(void);
#pragma enable_message(201)
```

---

### 19. `#pragma extref` - External Reference

Creates an external reference to a symbol.

**Syntax:**
```c
#pragma extref symbol
```

**Example:**
```c
#pragma extref external_symbol
```

---

## Previously Existing Pragmas

### `#pragma pack` - Structure Packing

Controls structure member alignment and packing.

**Syntax:**
```c
#pragma pack(n)
#pragma pack(push, n)
#pragma pack(pop)
#pragma pack()
```

**Example:**
```c
#pragma pack(push, 1)
struct PackedStruct {
    char c;
    int i;
    short s;
};
#pragma pack(pop)
```

---

### `#pragma packed` / `#pragma aligned`

Controls packing and alignment for declarations.

**Syntax:**
```c
#pragma packed n
#pragma aligned n
```

**Example:**
```c
#pragma aligned 16
struct AlignedStruct {
    double d;
    int i;
};
```

---

### `#pragma weak` - Weak Symbols

Declares weak symbols for linking.

**Syntax:**
```c
#pragma weak symbol
#pragma weak alias = target
```

**Example:**
```c
#pragma weak weak_function
void weak_function(void);
```

---

## Implementation Status

| Pragma | Parser Support | Full Implementation | Notes |
|--------|---------------|---------------------|-------|
| aux | ✅ | Partial | i86 maps to calling conventions |
| comment | ✅ | Partial | Accepted, not embedded in object |
| message | ✅ | ✅ | Prints to stderr |
| warning | ✅ | Partial | Accepted, no warning stack |
| once | ✅ | No | Needs preprocessor integration |
| intrinsic | ✅ | No | Framework in place |
| function | ✅ | No | Framework in place |
| code_seg | ✅ | Partial | Accepted, needs backend support |
| data_seg | ✅ | Partial | Accepted, needs backend support |
| alloc_text | ✅ | Partial | Accepted, needs backend support |
| inline_depth | ✅ | No | Framework in place |
| inline_recursion | ✅ | No | Framework in place |
| auto_inline | ✅ | No | Framework in place |
| enum | ✅ | Partial | Accepted, needs implementation |
| read_only_file | ✅ | No | Framework in place |
| library | ✅ | Partial | Accepted, needs linker integration |
| include_alias | ✅ | No | Needs preprocessor integration |
| error | ✅ | ✅ | Emits error and continues |
| disable_message | ✅ | No | Framework in place |
| enable_message | ✅ | No | Framework in place |
| extref | ✅ | Partial | Accepted, needs backend support |
| pack | ✅ | ✅ | Fully implemented |
| packed | ✅ | ✅ | Fully implemented |
| aligned | ✅ | ✅ | Fully implemented |
| weak | ✅ | ✅ | Fully implemented |

---

## Usage Notes

1. **Compatibility**: These pragmas enhance source-level compatibility with Watcom C/C++ code.

2. **Architecture-Specific**: Some pragmas (especially `#pragma aux`) have architecture-specific behavior.

3. **Graceful Degradation**: Pragmas without full implementation are accepted without errors, preventing compilation failures on Watcom code.

4. **Extensions**: The framework is in place to extend implementation as needed.

5. **Standards Compliance**: These pragmas are compiler extensions and do not affect standards compliance when not used.

---

## Future Enhancements

Potential areas for future development:

- Full intrinsic function support
- Complete segment control implementation
- Preprocessor integration for `once` and `include_alias`
- Warning message control stack
- Library directive integration with linker

---

## Examples

See `test_all_pragmas.c` for comprehensive examples of all supported pragmas.

---

## Contributing

When adding new pragma support:

1. Add handler function in `cc/ccom/scan.l`
2. Add entry to `pragmas[]` table
3. For architecture-specific behavior, update `arch/*/local.c`
4. Update this documentation
5. Add test cases

---

## References

- Open Watcom C/C++ Compiler Documentation
- PCC Compiler Documentation
- Architecture-Specific ABI Documentation
