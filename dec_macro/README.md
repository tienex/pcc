# DEC MACRO Assembly Language Frontend for PCC

## Overview

This directory contains a complete frontend compiler for **DEC MACRO** assembly language, integrated with the Portable C Compiler (PCC) framework.

DEC MACRO (also known as MACRO-11, MACRO-32, etc.) is the assembly language used on Digital Equipment Corporation (DEC) systems, including:

- **PDP-10** (36-bit architecture)
- **PDP-11** (16-bit architecture)
- **VAX** (32-bit architecture)

## Features

### Supported DEC MACRO Features

- **Labels**: Symbolic labels for code and data addresses
- **Instructions**: Full instruction set support (mnemonic-based)
- **Directives**: Assembly directives for program structure
- **Macros**: Macro definition and expansion (.MACRO/.ENDM)
- **Conditional Assembly**: .IF/.IFDEF/.IFNDEF/.ENDC
- **Data Directives**: .BYTE, .WORD, .LONG, .ASCII, .ASCIZ
- **Storage Allocation**: .BLKB, .BLKW, .BLKL
- **Symbol Control**: .GLOBL, .EXTERN, .ENTRY
- **Multiple Number Formats**: Decimal, octal, hexadecimal, binary
- **Addressing Modes**:
  - Register direct (R0-R15)
  - Immediate (#value)
  - Direct (address)
  - Indirect (@address, @register)
  - Indexed (offset(register))
  - Autoincrement ((Rn)+)
  - Autodecrement (-(Rn))

### Compiler Architecture

The DEC MACRO frontend follows the standard PCC architecture:

```
Source (.mac) → Lexer (scan.l) → Parser (mgram.y) → IR → Backend → Assembly
```

**Components:**

- `main.c` - Entry point and command-line processing
- `scan.l` - Flex lexical analyzer for tokenization
- `mgram.y` - Yacc/Bison parser grammar
- `pass1.h` - Data structure definitions
- `error.c` - Error handling and diagnostics
- `symtab.c` - Symbol table management
- `macro.c` - Macro definition and expansion
- `codegen.c` - Code generation and instruction emission

## Building

### Prerequisites

- C compiler (gcc or clang)
- Flex (lexical analyzer generator)
- Bison or Yacc (parser generator)

### Build Steps

```bash
cd dec_macro/mcom
./configure    # Configure build (if running from PCC root)
make           # Build the compiler
```

Or manually:

```bash
yacc -d mgram.y         # Generate parser
flex scan.l             # Generate lexer
gcc -c main.c error.c symtab.c macro.c codegen.c y.tab.c lex.yy.c
gcc -o mcom main.o error.o symtab.o macro.o codegen.o y.tab.o lex.yy.o
```

## Usage

```bash
mcom [options] [input.mac]

Options:
  -o file       Write output to file
  -l            Generate listing output
  -v            Verbose output
  --dump-symtab Dump symbol table
  -h, --help    Show help
```

### Examples

```bash
# Assemble a DEC MACRO file
mcom program.mac

# Assemble to a specific output file
mcom -o program.s program.mac

# Generate listing with symbol table
mcom -l --dump-symtab program.mac

# Verbose assembly
mcom -v program.mac
```

## DEC MACRO Language Examples

### Example 1: Simple PDP-11 Program

```asm
	.TITLE	HELLO - Hello World Program
	.IDENT	/V1.0/

	.PSECT	CODE, RO, I, LCL

START:	.ENTRY	START
	MOV	#MSG, R1	; Load message address
	MOV	#13, R2		; Message length
	MOV	#1, R0		; Write system call
	TRAP	#0		; Call OS

	MOV	#0, R0		; Exit code
	TRAP	#1		; Exit

MSG:	.ASCII	/Hello, World!/

	.END	START
```

### Example 2: Macro Definition

```asm
	.TITLE	MACROS - Macro Examples

	; Define a PUSH macro
	.MACRO	PUSH REG
	MOV	REG, -(SP)
	.ENDM

	; Define a POP macro
	.MACRO	POP REG
	MOV	(SP)+, REG
	.ENDM

	; Use the macros
START:	MOV	#100, R0
	PUSH	R0		; Expands to: MOV R0, -(SP)
	PUSH	R1
	POP	R1		; Expands to: MOV (SP)+, R1
	POP	R0

	.END	START
```

### Example 3: VAX Instructions

```asm
	.TITLE	VAX - VAX Assembly Example
	.PSECT	DATA, WRT, NOEXE

BUFFER:	.BLKL	10		; Reserve 10 longwords

	.PSECT	CODE, RO, EXE

MAIN:	.ENTRY	MAIN, ^M<R2,R3,R4>
	CLRL	R0		; Clear R0
	MOVL	#100, R1	; Load immediate
	ADDL2	R1, R0		; R0 = R0 + R1
	MULL2	#5, R0		; R0 = R0 * 5

	MOVL	R0, BUFFER	; Store to memory
	RET			; Return

	.END	MAIN
```

## Target Backends

The DEC MACRO frontend can generate code for multiple PCC backends:

- **arch/pdp10** - PDP-10 (36-bit)
- **arch/pdp11** - PDP-11 (16-bit)
- **arch/vax** - VAX (32-bit)

## Integration with PCC

To integrate this frontend into the main PCC build system:

1. Add `dec_macro` to the top-level `configure.ac`
2. Add `dec_macro/mcom/Makefile` to AC_CONFIG_FILES
3. Update top-level `Makefile.am` to include dec_macro subdirectory
4. Run `autoreconf` to regenerate build scripts

## Addressing Modes Reference

| Mode | Syntax | Description |
|------|--------|-------------|
| Register | `R0` | Register direct |
| Immediate | `#100` | Immediate value |
| Direct | `LABEL` | Direct address |
| Indirect | `@LABEL` | Indirect through address |
| Register Indirect | `@R0` | Indirect through register |
| Autoincrement | `(R0)+` | Post-increment |
| Autodecrement | `-(R0)` | Pre-decrement |
| Indexed | `100(R0)` | Base + displacement |

## Number Formats

| Format | Syntax | Example |
|--------|--------|---------|
| Decimal | `123` | 123 |
| Octal | `0177` or `^O177` | 127 |
| Hexadecimal | `0xFF` or `^XFF` | 255 |
| Binary | `^B1010` | 10 |

## Directives Reference

### Program Structure
- `.TITLE` - Module title
- `.IDENT` - Identification string
- `.PSECT` - Program section
- `.ENTRY` - Entry point
- `.END` - End of module

### Symbol Control
- `.GLOBL` - Declare global symbol
- `.EXTERN` - External reference

### Data Definition
- `.BYTE` - Byte data
- `.WORD` - Word data (16-bit)
- `.LONG` - Longword data (32-bit)
- `.ASCII` - ASCII string
- `.ASCIZ` - Null-terminated string

### Storage Allocation
- `.BLKB` - Block of bytes
- `.BLKW` - Block of words
- `.BLKL` - Block of longs

### Alignment
- `.ALIGN` - Align to boundary
- `.EVEN` - Word align

### Macros
- `.MACRO` - Begin macro definition
- `.ENDM` - End macro definition
- `.MEXIT` - Exit macro

### Conditional Assembly
- `.IF` - Conditional assembly
- `.IFDEF` - If defined
- `.IFNDEF` - If not defined
- `.ENDC` - End conditional

## Testing

Test files are provided in the `tests/` directory:

```bash
cd tests
../mcom/mcom test_basic.mac
../mcom/mcom test_macros.mac
../mcom/mcom test_vax.mac
```

## References

- DEC MACRO-11 Language Reference Manual
- DEC VAX MACRO and Instruction Set Reference Manual
- PDP-10 MACRO Assembler Reference Manual
- PCC (Portable C Compiler) Architecture Documentation

## License

Copyright (c) 2025 PCC DEC MACRO Compiler

This code follows the same BSD license as the PCC project.

## Author

Developed as part of the PCC compiler infrastructure project.

## Contributing

Contributions welcome! Areas for improvement:

- Additional macro features (.IRP, .IRPC, .REPT)
- Advanced conditional assembly
- Listing file generation
- Cross-reference generation
- Integration with PCC linker
- Extended instruction sets for specific DEC models
