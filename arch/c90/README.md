# C90 Code Generator Backend

This is a code generator backend for PCC that generates portable C90-compliant C code instead of architecture-specific assembly language.

## Overview

The C90 backend translates PCC's intermediate representation (IR) into standard C code that can be compiled by any C90-compliant C compiler. This provides:

- **Portability**: Generated code can run on any platform with a C90 compiler
- **Readability**: Output is human-readable C code rather than assembly
- **Debugging**: Easier to understand and debug than assembly output
- **Cross-compilation**: Can target any platform without architecture-specific code generation
- **Memory Model Support**: Configurable for 16-bit, 32-bit, 64-bit, and segmented architectures
- **Modern Features**: Follows patterns from recent PCC improvements (i386/i86 backends)

## Architecture

The C90 backend follows the same structure as other PCC architecture backends:

- **macdefs.h**: Architecture-specific definitions (type sizes, alignment, register definitions)
- **code.c**: High-level code generation (segments, declarations, file structure)
- **local.c**: Architecture-specific local optimizations
- **local2.c**: Low-level code generation (function prologue/epilogue, instruction emission)
- **order.c**: Instruction ordering and optimization decisions
- **table.c**: Pattern matching table mapping IR operations to C code templates

## New Features (2025 Update)

### Memory Model Support

Following the i386/i86 backend patterns, the C90 backend now supports multiple memory models:

- **16-bit mode** (`-mcmodel=16bit`): For embedded systems, 8/16-bit CPUs
  - 16-bit integers and pointers
  - 64-bit long double
  - Generates `int16_t`, `uint16_t` typedefs

- **32-bit mode** (`-mcmodel=32bit` or `-mcmodel=small`, default): For most systems
  - 32-bit integers and pointers
  - 80-bit long double (x87 compatible)
  - Standard C90 types

- **64-bit mode** (`-mcmodel=64bit` or `-mcmodel=medium`): For modern 64-bit systems
  - 32-bit int, 64-bit long
  - 64-bit pointers
  - Generates `int64_t`, `uint64_t` typedefs

- **Segmented mode** (`-mcmodel=segmented`): For segmented architectures
  - 32-bit segmented pointers (16:16 or 16:32)
  - Useful for targeting 16-bit protected mode or x86 real mode

### Directive Abstraction Layer

Inspired by the new `libx86asm` abstraction in x86 backends, C90 now has clean directive functions:

```c
void c90_declare_external(char *name, TWORD type);
void c90_declare_static(char *name, TWORD type);
void c90_declare_weak(char *name);
void c90_create_alias(char *from, char *to);
```

This makes the code more maintainable and allows future dialect switching (C89/C90/C99).

### Enhanced Type System

- **Full type support**: char, short, int, long, long long, float, double, long double
- **Pointer types**: Properly handles multi-level pointers
- **Unsigned types**: Correctly emits `unsigned` qualifier
- **Function return types**: Generates proper return type signatures

### Builtin Function Mapping

Maps GCC/Clang builtins to C90-compatible equivalents:

- `__builtin_constant_p(x)` → `1` (always true after compilation)
- `__builtin_expect(x, y)` → `x` (hint ignored)
- `__builtin_alloca(n)` → `alloca(n)` (requires `<alloca.h>`)

## Implementation Details

### Virtual Registers

The C90 backend uses "virtual registers" that map to C variables:
- `r0` through `r15`: Integer/pointer registers (long type)
- `f0` through `f7`: Floating-point registers (double type)
- `CC`: Condition code pseudo-register (int type)

These are declared as local variables in generated functions.

### Code Generation

IR operations are mapped to C equivalents:
- `PLUS` → `A1 = AL + AR;`
- `ASSIGN` → `AL = AR;`
- `CALL` → `A1 = function();`
- `UMUL` (dereference) → `A1 = *AL;`
- etc.

### Type System

The backend uses standard C90 types (sizes depend on memory model):

**16-bit mode:**
- 8-bit: `char` / `unsigned char`
- 16-bit: `short` / `unsigned short` / `int` / `unsigned int`
- 32-bit: `long` / `unsigned long`
- 64-bit: `long long` / `unsigned long long` / `double` / `long double`
- 32-bit: `float`

**32-bit mode (default):**
- 8-bit: `char` / `unsigned char`
- 16-bit: `short` / `unsigned short`
- 32-bit: `int` / `unsigned int` / `long` / `unsigned long` / `float`
- 64-bit: `long long` / `unsigned long long` / `double`
- 80-bit: `long double`

**64-bit mode:**
- 8-bit: `char` / `unsigned char`
- 16-bit: `short` / `unsigned short`
- 32-bit: `int` / `unsigned int` / `float`
- 64-bit: `long` / `unsigned long` / `double` / `long double` / pointer types
- 64-bit: `long long` / `unsigned long long`

## Limitations

While significantly improved, some limitations remain:

1. **Function parameters**: Currently generates `void func(void)` signatures
   - Tracking actual parameter types requires extending the interpass protocol
   - Planned for future enhancement

2. **Type conversions**: Some implicit conversions may need explicit casts
   - Most conversions are handled correctly
   - Edge cases with complex pointer casts may need work

3. **Inline assembly**: Not supported (C90 has no standard inline assembly)
   - `__asm` blocks are commented out with warnings

4. **ABI compatibility**: Generated code uses C calling conventions
   - May differ from native ABI (not usually an issue for C-to-C compilation)

5. **Optimization**: No high-level optimizations yet
   - Generated code is straightforward translation of IR
   - C compiler optimizations will be applied to output

## Usage

To use the C90 backend, configure PCC with:

```bash
./configure --target=c90
make
```

Then compile C source:

```bash
# Default 32-bit mode
pcc -target c90 input.c -o output.c

# 16-bit mode (for embedded systems)
pcc -target c90 -mcmodel=16bit input.c -o output.c

# 64-bit mode (for modern systems)
pcc -target c90 -mcmodel=64bit input.c -o output.c

# Segmented mode (for real mode / protected mode DOS)
pcc -target c90 -mcmodel=segmented input.c -o output.c
```

The output will be C90-compliant C code that can be compiled with any C compiler:

```bash
gcc output.c -o final_executable
clang output.c -o final_executable
tcc output.c -o final_executable
```

### Generated Code Structure

The C90 backend generates:

```c
/* Generated by PCC C90 backend (32-bit mode) */

#include <stddef.h>  /* size_t, ptrdiff_t, NULL */
#include <limits.h>  /* INT_MAX, LONG_MAX, etc. */
#include <float.h>   /* FLT_MAX, DBL_MAX, etc. */

/* .text section */

void function_name(void)
{
	/* Virtual register variables */
	long r0;
	long r1;
	/* ... */

	/* Temporary variables */
	int CC;  /* Condition code pseudo-register */

	/* Function body */
	r0 = 42;
	r1 = r0 + 10;
	/* ... */
}
```

## Recent Improvements (2025)

Based on analysis of recent PCC enhancements:

✅ **Implemented:**
- Memory model support (`-mcmodel=`) following i386/i86 patterns
- Dynamic type sizes based on memory model
- Directive abstraction layer (like `libx86asm`)
- Enhanced type name generation for all C types
- Builtin function mapping
- Proper return type generation
- Improved header generation with model-specific typedefs
- Floating-point format support (IEEE 754)

🚧 **In Progress:**
- Full function parameter type tracking
- Extended pointer type annotations (for `__far`, `__near`, etc.)
- Structure/union field generation

📋 **Future Enhancements:**
- C99 output mode (`-std=c99`)
- Better optimization of generated expressions
- Support for C99 features (VLAs, inline, designated initializers)
- Integration with code formatters (indent, clang-format)
- Symbol visibility attributes (`__attribute__((visibility(...)))`)
- Thread-local storage (`__thread` / `_Thread_local`)
- Full interpass protocol extension for parameter types

## Compatibility

The C90 backend follows modern PCC conventions:

- **APX support**: Recognizes extended x86-64 registers (for future expansion)
- **Multi-stage bootstrap**: Compatible with PCC's bootstrap process
- **Universal assembly patterns**: Follows patterns from universal x86 emitter
- **ABI awareness**: Understands Itanium, MSVC, Watcom ABIs (for future use)
- **Pragma support**: Processes architecture hints (though not all apply to C90)

## Contributing

When extending the C90 backend:

1. Follow the directive abstraction pattern (use `c90_*` functions, not raw `printf`)
2. Update memory model macros in `macdefs.h`
3. Add flag handling in `local.c:mflags()`
4. Update type mapping in `code.c:c90_typename()` and `local2.c:print_c_type()`
5. Document new features in this README

## References

- PCC main repository: http://pcc.ludd.ltu.se/
- i386 backend: `/arch/i386/` (memory model patterns)
- i86 backend: `/arch/i86/` (16-bit support patterns)
- libx86asm: Universal x86 assembly emitter (directive abstraction pattern)

## License

Same license as PCC (BSD-style). See the file headers for details.
