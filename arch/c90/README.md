# C90 Code Generator Backend

This is a code generator backend for PCC that generates portable C90-compliant C code instead of architecture-specific assembly language.

## Overview

The C90 backend translates PCC's intermediate representation (IR) into standard C code that can be compiled by any C90-compliant C compiler. This provides:

- **Portability**: Generated code can run on any platform with a C90 compiler
- **Readability**: Output is human-readable C code rather than assembly
- **Debugging**: Easier to understand and debug than assembly output
- **Cross-compilation**: Can target any platform without architecture-specific code generation

## Architecture

The C90 backend follows the same structure as other PCC architecture backends:

- **macdefs.h**: Architecture-specific definitions (type sizes, alignment, register definitions)
- **code.c**: High-level code generation (segments, declarations, file structure)
- **local.c**: Architecture-specific local optimizations
- **local2.c**: Low-level code generation (function prologue/epilogue, instruction emission)
- **order.c**: Instruction ordering and optimization decisions
- **table.c**: Pattern matching table mapping IR operations to C code templates

## Implementation Details

### Virtual Registers

The C90 backend uses "virtual registers" that map to C variables:
- `r0` through `r15`: Integer/pointer registers
- `f0` through `f7`: Floating-point registers

These are declared as local variables in generated functions.

### Code Generation

IR operations are mapped to C equivalents:
- `PLUS` → `A1 = AL + AR;`
- `ASSIGN` → `AL = AR;`
- `CALL` → `A1 = function();`
- `UMUL` (dereference) → `A1 = *AL;`
- etc.

### Type System

The backend uses standard C90 types:
- 8-bit: `char` / `unsigned char`
- 16-bit: `short` / `unsigned short`
- 32-bit: `int` / `unsigned int` / `long` / `unsigned long`
- 64-bit: `long long` / `unsigned long long`
- Floating: `float`, `double`

## Limitations

This is a basic implementation with some limitations:

1. **Function signatures**: Currently generates `void func(void)` signatures; full implementation would need parameter type tracking
2. **Type conversions**: Some implicit conversions may need explicit casts
3. **Complex expressions**: Very complex expressions may need temporary variables
4. **Inline assembly**: Not supported (C90 has no standard inline assembly)
5. **ABI compatibility**: Generated code uses C calling conventions, may differ from native ABI

## Usage

To use the C90 backend, configure PCC with:

```bash
./configure --target=c90
make
```

Then compile C source:

```bash
pcc -target c90 input.c -o output.c
```

The output will be C90-compliant C code that can be compiled with any C compiler:

```bash
gcc output.c -o final_executable
```

## Future Enhancements

Potential improvements for this backend:

- Full function signature generation with proper parameter types
- Better handling of struct/union types
- Optimization of generated C code (expression simplification, dead code elimination)
- Support for C99 features (when targeting C99 instead of C90)
- Better pretty-printing of generated code
- Integration with C code formatters (indent, clang-format)

## License

Same license as PCC (BSD-style). See the file headers for details.
