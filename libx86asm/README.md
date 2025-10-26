# libx86asm - Universal x86 Assembly Emitter Library

A comprehensive C library for generating x86 assembly code in multiple assembler formats.

## Features

- **Multi-Format Support**: Generate assembly for 9 different assemblers:
  - GNU AS (AT&T syntax)
  - Apple AS (Darwin/Mach-O)
  - MASM 1.x-6.11 (Microsoft Macro Assembler)
  - ML 6.x+ (Microsoft Macro Assembler)
  - TASM (Borland Turbo Assembler)
  - WASM (Watcom Assembler)
  - OWASM (Open Watcom Assembler)
  - NASM (Netwide Assembler)
  - YASM (Yet Another Assembler)

- **Comprehensive x86 Support**:
  - Full register set (8-bit through 64-bit GPRs)
  - Segment registers
  - FPU registers (ST0-ST7)
  - MMX registers
  - SSE/AVX registers (XMM/YMM)
  - Complex addressing modes
  - Instruction prefixes (LOCK, REP, etc.)

- **Clean API**: Simple, intuitive interface for code generation
- **Format Abstraction**: Write code once, emit to any assembler format
- **Portable**: Pure C implementation with no external dependencies

## Building

```bash
cd libx86asm
make
```

This will create:
- `libx86asm.a` - Static library
- `libx86asm.so` - Shared library
- `test_x86asm` - Test program

## Running Tests

```bash
make test
```

This will generate sample assembly files in all supported formats.

## Installation

```bash
sudo make install
```

This installs the library to `/usr/local/lib` and headers to `/usr/local/include`.

## Quick Start

```c
#include <x86asm.h>

int main(void) {
    FILE *fp = fopen("output.asm", "w");
    x86asm_ctx_t *ctx = x86asm_create(ASM_FMT_NASM, fp, 64);

    /* Emit a simple function */
    x86asm_segment(ctx, SEG_TEXT, NULL);
    x86asm_label(ctx, "my_function", 1);

    /* mov rax, rdi */
    X86ASM_INSN2(ctx, "mov",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_reg(REG_RDI, 64));

    /* ret */
    X86ASM_INSN0(ctx, "ret");

    x86asm_destroy(ctx);
    fclose(fp);
    return 0;
}
```

Compile with:
```bash
gcc -o myprogram myprogram.c -lx86asm
```

## API Reference

### Context Management

#### `x86asm_ctx_t *x86asm_create(x86asm_format_t format, FILE *output, int bits)`
Create a new emitter context.

**Parameters:**
- `format`: Target assembler format (ASM_FMT_GNU_AS, ASM_FMT_NASM, etc.)
- `output`: Output file stream
- `bits`: Target architecture (16, 32, or 64)

**Returns:** Emitter context or NULL on error

#### `void x86asm_destroy(x86asm_ctx_t *ctx)`
Destroy emitter context and free resources.

### Instruction Emission

#### `void x86asm_insn(x86asm_ctx_t *ctx, const char *mnemonic, x86asm_operand_t *ops, int nops, x86asm_prefix_t prefix)`
Emit an instruction with operands.

**Parameters:**
- `ctx`: Emitter context
- `mnemonic`: Instruction mnemonic (e.g., "mov", "add")
- `ops`: Array of operands
- `nops`: Number of operands (0-3)
- `prefix`: Instruction prefix flags

**Convenience Macros:**
- `X86ASM_INSN0(ctx, mnemonic)` - No operands (e.g., ret)
- `X86ASM_INSN1(ctx, mnemonic, op1)` - One operand (e.g., push)
- `X86ASM_INSN2(ctx, mnemonic, op1, op2)` - Two operands (e.g., mov)
- `X86ASM_INSN3(ctx, mnemonic, op1, op2, op3)` - Three operands (e.g., AVX)

### Operand Creation

#### Register Operand
```c
x86asm_operand_t x86asm_op_reg(x86asm_reg_t reg, int size);
```

Example:
```c
x86asm_op_reg(REG_RAX, 64)  /* 64-bit RAX register */
x86asm_op_reg(REG_EAX, 32)  /* 32-bit EAX register */
x86asm_op_reg(REG_AL, 8)    /* 8-bit AL register */
```

#### Immediate Operand
```c
x86asm_operand_t x86asm_op_imm(int64_t value, int size);
```

Example:
```c
x86asm_op_imm(42, 32)       /* 32-bit immediate value 42 */
x86asm_op_imm(0x1234, 16)   /* 16-bit immediate value 0x1234 */
```

#### Memory Operand
```c
x86asm_operand_t x86asm_op_mem(x86asm_reg_t base, x86asm_reg_t index,
                               int scale, int64_t disp, int size);
```

Example:
```c
/* [rbp - 16] */
x86asm_op_mem(REG_RBP, REG_NONE, 0, -16, 64)

/* [rax + rbx*4 + 8] */
x86asm_op_mem(REG_RAX, REG_RBX, 4, 8, 64)
```

#### Symbol Memory Operand
```c
x86asm_operand_t x86asm_op_mem_symbol(const char *symbol, int64_t disp, int size);
```

Example:
```c
x86asm_op_mem_symbol("global_var", 0, 64)     /* [global_var] */
x86asm_op_mem_symbol("array", 16, 32)         /* [array + 16] */
```

#### Label Operand
```c
x86asm_operand_t x86asm_op_label(const char *label);
```

### Labels and Segments

#### `void x86asm_label(x86asm_ctx_t *ctx, const char *name, int global)`
Emit a label.

**Parameters:**
- `name`: Label name
- `global`: 1 for global/exported labels, 0 for local

#### `void x86asm_segment(x86asm_ctx_t *ctx, x86asm_segment_t seg, const char *name)`
Switch to a segment/section.

**Segments:**
- `SEG_TEXT`: Code section
- `SEG_DATA`: Initialized data
- `SEG_BSS`: Uninitialized data
- `SEG_RODATA`: Read-only data
- `SEG_CONST`: Constants

### Data Emission

#### `void x86asm_data(x86asm_ctx_t *ctx, x86asm_datasize_t size, const void *data, size_t count)`
Emit data values.

**Data Sizes:**
- `DATA_BYTE`: 8-bit bytes
- `DATA_WORD`: 16-bit words
- `DATA_DWORD`: 32-bit doublewords
- `DATA_QWORD`: 64-bit quadwords
- `DATA_ASCIZ`: Null-terminated string

Example:
```c
/* Emit bytes */
uint8_t bytes[] = {0x01, 0x02, 0x03};
x86asm_data(ctx, DATA_BYTE, bytes, 3);

/* Emit string */
const char *msg = "Hello, World!";
x86asm_data(ctx, DATA_ASCIZ, msg, strlen(msg));

/* Emit 32-bit integers */
uint32_t values[] = {100, 200, 300};
x86asm_data(ctx, DATA_DWORD, values, 3);
```

### Directives

#### `void x86asm_comment(x86asm_ctx_t *ctx, const char *text)`
Emit a comment.

#### `void x86asm_align(x86asm_ctx_t *ctx, int alignment)`
Emit an alignment directive.

#### `void x86asm_directive(x86asm_ctx_t *ctx, const char *name, const char *value)`
Emit a raw assembler directive.

## Complete Example

```c
#include <x86asm.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    FILE *fp = fopen("example.asm", "w");
    x86asm_ctx_t *ctx = x86asm_create(ASM_FMT_NASM, fp, 64);

    /* Code section */
    x86asm_segment(ctx, SEG_TEXT, NULL);

    /* Function: int add(int a, int b) */
    x86asm_comment(ctx, "Function: int add(int a, int b)");
    x86asm_label(ctx, "add", 1);

    /* Arguments in RDI and RSI (System V AMD64 ABI) */
    X86ASM_INSN2(ctx, "mov",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_reg(REG_RDI, 64));
    X86ASM_INSN2(ctx, "add",
                 x86asm_op_reg(REG_RAX, 64),
                 x86asm_op_reg(REG_RSI, 64));
    X86ASM_INSN0(ctx, "ret");

    /* Data section */
    x86asm_segment(ctx, SEG_DATA, NULL);
    x86asm_label(ctx, "message", 0);
    const char *msg = "Result: ";
    x86asm_data(ctx, DATA_ASCIZ, msg, strlen(msg));

    x86asm_destroy(ctx);
    fclose(fp);
    return 0;
}
```

## Format Differences

### AT&T vs Intel Syntax

**AT&T Syntax (GNU AS, Apple AS):**
- Operand order: source, destination
- Register prefix: %
- Immediate prefix: $
- Memory syntax: `disp(%base, %index, scale)`

**Intel Syntax (MASM, NASM, TASM, WASM):**
- Operand order: destination, source
- No register prefix
- No immediate prefix
- Memory syntax: `[base + index*scale + disp]`

The library handles these differences automatically based on the selected format.

### Format-Specific Features

**GNU AS:**
- Standard ELF sections (.text, .data, .bss)
- `.globl` for global symbols

**Apple AS:**
- Mach-O sections
- Underscore prefix for symbols (_name)
- `.globl` for exports

**MASM/ML:**
- SEGMENT/ENDS for sections
- PUBLIC for global symbols
- Uppercase directives (DB, DW, DD, DQ)

**NASM/YASM:**
- `section` directive
- `global` for exports
- Lowercase directives (db, dw, dd, dq)

**TASM:**
- Similar to MASM
- Additional Borland-specific features

**WASM/OWASM:**
- Watcom-specific directives
- `.code`, `.data`, `.data?` for sections

## Register Reference

### General Purpose Registers

**64-bit:** RAX, RBX, RCX, RDX, RSI, RDI, RBP, RSP, R8-R15
**32-bit:** EAX, EBX, ECX, EDX, ESI, EDI, EBP, ESP, R8D-R15D
**16-bit:** AX, BX, CX, DX, SI, DI, BP, SP, R8W-R15W
**8-bit:** AL, BL, CL, DL, AH, BH, CH, DH, SPL, BPL, SIL, DIL, R8B-R15B

### APX Extended Registers (Intel Advanced Performance Extensions)

**64-bit:** R16-R31
**32-bit:** R16D-R31D
**16-bit:** R16W-R31W
**8-bit:** R16B-R31B

The library can emit code using APX extended registers even on assemblers that don't natively support APX yet. The register names will be emitted as-is (e.g., r16, r17, etc.).

### Special Registers

**Segment:** ES, CS, SS, DS, FS, GS
**FPU:** ST0-ST7
**MMX:** MM0-MM7

### SIMD Registers

**SSE (XMM):** XMM0-XMM15
**AVX (YMM):** YMM0-YMM15
**AVX-512 (ZMM):** ZMM0-ZMM31
**Extended XMM (APX/AVX-512):** XMM16-XMM31
**Extended YMM (APX/AVX-512):** YMM16-YMM31
**Mask (AVX-512):** K0-K7

## Instruction Support

The library can emit **any x86/x86-64 instruction** from Intel, AMD, or VIA:
- All legacy instructions (8086 through Pentium)
- Modern extensions (SSE, SSE2, SSE3, SSSE3, SSE4.1, SSE4.2)
- AVX, AVX2, AVX-512 (all variants)
- APX (Advanced Performance Extensions)
- AMD-specific instructions (3DNow!, XOP, FMA4, TBM, etc.)
- VIA-specific instructions (PadLock, etc.)
- Specialized instructions (BMI, BMI2, ADX, SGX, etc.)

Since instruction mnemonics are strings, you can emit any instruction regardless of whether it's in the official ISA documentation - useful for experimental or undocumented opcodes.

## License

BSD Licensed - See COPYING file for details.

## Contributing

This library is part of the Portable C Compiler (PCC) project.

## Contact

For issues and questions, please refer to the PCC project documentation.
