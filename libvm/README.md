# libvm - Universal Bootstrapping Virtual Machine

A minimal register-based virtual machine designed for bootstrapping compilers on systems without native compilers. Simple enough to implement in shell scripts, batch files, or any scripting language.

## Features

- **16 general-purpose registers** (r0-r15, including sp and fp)
- **Register-based architecture** for efficiency
- **Simple instruction set** (50+ instructions)
- **Text and binary bytecode formats**
- **Multiple implementations:**
  - Full C library (`libvm.a`)
  - POSIX shell script (`vmsh.sh`)
  - Windows batch file (`vmbat.bat`)
- **Cross-platform** (Unix, Linux, Windows, DOS)
- **Self-contained** - no external dependencies

## Instruction Set

### Data Movement
- `nop` - No operation
- `mov rd, rs` - Move register
- `ldi rd, imm` - Load immediate
- `ld rd, [rs+off]` - Load from memory
- `st [rd+off], rs` - Store to memory
- `ldb/ldh` - Load byte/halfword
- `stb/sth` - Store byte/halfword

### Arithmetic
- `add rd, rs1, rs2` - Add
- `sub rd, rs1, rs2` - Subtract
- `mul rd, rs1, rs2` - Multiply
- `div rd, rs1, rs2` - Divide
- `mod rd, rs1, rs2` - Modulo
- `neg rd, rs` - Negate
- `addi/subi rd, rs, imm` - Add/subtract immediate

### Logical
- `and/or/xor rd, rs1, rs2` - Bitwise operations
- `not rd, rs` - Bitwise NOT
- `shl/shr/sar rd, rs, bits` - Shift operations
- `andi/ori/xori rd, rs, imm` - Immediate bitwise

### Comparison
- `cmp rs1, rs2` - Compare (sets flags)
- `tst rs` - Test (sets flags)
- `cmpi rs, imm` - Compare immediate

### Control Flow
- `jmp addr` - Unconditional jump
- `jz/jnz addr` - Jump if zero/not zero
- `jl/jle/jg/jge addr` - Conditional jumps
- `call addr` - Call subroutine
- `ret` - Return from subroutine

### Stack
- `push rs` - Push register
- `pop rd` - Pop register

### I/O
- `print rs` - Print register value
- `printc rs` - Print character
- `input rd` - Read input
- `syscall n` - System call

### System
- `halt` - Stop execution

## Usage

### C Library

```c
#include <vm.h>

// Create VM
vm_state_t *vm = vm_create();

// Load program from text assembly
vm_load_text(vm,
    "ldi r0, 42\n"
    "print r0\n"
    "halt\n"
);

// Run
vm_run(vm);

// Cleanup
vm_destroy(vm);
```

### Command-Line Tools

**Assemble:**
```bash
vmasm -o program.vmo program.vms
```

**Disassemble:**
```bash
vmdis program.vmo
```

**Run:**
```bash
vmrun program.vmo
vmrun -t program.vmo  # Trace execution
vmrun -s program.vmo  # Show statistics
```

### Shell Script Executor

For systems without a C compiler:

```bash
./vmsh.sh examples/hello.vms
```

### Batch File Executor

For DOS/Windows systems:

```batch
vmbat.bat examples\hello.vms
```

## Examples

### Hello World

```asm
# hello.vms
ldi r0, 72      # 'H'
printc r0
ldi r0, 101     # 'e'
printc r0
# ... etc ...
halt
```

Run:
```bash
./vmsh.sh examples/hello.vms
```

### Fibonacci Sequence

```asm
ldi r0, 0
ldi r1, 1
ldi r2, 10

print r0
print r1

loop:
cmp r2, r0
jz done
add r3, r0, r1
print r3
mov r0, r1
mov r1, r3
subi r2, r2, 1
jmp loop

done:
halt
```

### Factorial

```asm
main:
ldi r0, 5
call factorial
print r0
halt

factorial:
push r1
push r2
ldi r1, 1
cmp r0, r1
jle base_case
mov r2, r0
subi r0, r0, 1
call factorial
mul r0, r0, r2
pop r2
pop r1
ret

base_case:
ldi r0, 1
pop r2
pop r1
ret
```

## Bytecode Format

### Text Format (`.vms`)

Human-readable assembly language:
```
ldi r0, 10
ldi r1, 20
add r2, r0, r1
print r2
halt
```

### Binary Format (`.vmo`)

8 bytes per instruction:
```
[opcode:8][rd:8][rs1:8][rs2:8][immediate:32]
```

## Bootstrapping Strategy

The VM enables a multi-stage bootstrap:

1. **Stage 0**: Shell script VM (`vmsh.sh` or `vmbat.bat`)
   - Runs on any system with sh/cmd.exe
   - No compiler needed
   - Slow but functional

2. **Stage 1**: Minimal compiler written in VM bytecode
   - Compiles simple C subset
   - Generates native code or faster VM bytecode

3. **Stage 2**: Full compiler compiled with Stage 1
   - Complete PCC compiler
   - Optimized native code generation

## Design Philosophy

- **Simplicity**: Easy to understand and implement
- **Portability**: Runs anywhere
- **Minimalism**: Only essential instructions
- **Bootstrappable**: Can build itself
- **Text-first**: Human-readable bytecode format

## Memory Model

- **Linear address space**
- **Byte-addressable**
- **Stack grows downward** from top of memory
- **Default size**: 1MB (configurable)

## Registers

- `r0-r13`: General purpose
- `r14 (sp)`: Stack pointer
- `r15 (fp)`: Frame pointer

## Flags

- `Z`: Zero
- `N`: Negative
- `C`: Carry
- `V`: Overflow

## Building

```bash
./configure
make
make install
```

## Testing

```bash
# Run examples
./vmsh.sh examples/hello.vms
./vmsh.sh examples/fibonacci.vms
./vmsh.sh examples/factorial.vms

# Or with C implementation
make check
./vmrun examples/hello.vms
```

## Performance

The VM implementations have different performance characteristics:

- **C library**: ~10M instructions/second
- **Shell script**: ~1K instructions/second
- **Batch file**: ~500 instructions/second

The shell/batch implementations are for bootstrapping only. Once a C compiler is available, use the C library version.

## Contributing

The VM is part of the Portable C Compiler (PCC) project.

## License

Copyright (c) 2025 PCC Project
