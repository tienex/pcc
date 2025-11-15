/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Bootstrapping Virtual Machine
 *
 * A minimal register-based VM designed for bootstrapping compilers
 * on systems without native compilers. Simple enough to implement
 * in shell scripts, batch files, or any scripting language.
 *
 * Features:
 * - 16 general-purpose registers
 * - Simple linear memory model
 * - Text and binary bytecode formats
 * - Minimal instruction set (30 instructions)
 * - System call interface
 * - Can be implemented in sh, bash, cmd.exe, etc.
 */

#ifndef _PCC_VM_H_
#define _PCC_VM_H_

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * VM Configuration
 */
#define VM_NUM_REGISTERS    16
#define VM_DEFAULT_MEM_SIZE (1024 * 1024)  /* 1 MB */
#define VM_STACK_SIZE       (64 * 1024)    /* 64 KB */
#define VM_MAX_INSTRUCTION_LEN 256

/*
 * Register Names
 */
typedef enum {
	VM_R0 = 0,   /* General purpose */
	VM_R1,
	VM_R2,
	VM_R3,
	VM_R4,
	VM_R5,
	VM_R6,
	VM_R7,
	VM_R8,
	VM_R9,
	VM_R10,
	VM_R11,
	VM_R12,
	VM_R13,
	VM_SP = 14,  /* Stack pointer */
	VM_FP = 15   /* Frame pointer */
} vm_register_t;

/*
 * Instruction Opcodes
 */
typedef enum {
	/* Data Movement */
	VM_OP_NOP = 0x00,
	VM_OP_MOV,          /* MOV rd, rs */
	VM_OP_LDI,          /* LDI rd, imm */
	VM_OP_LD,           /* LD rd, [rs+off] */
	VM_OP_ST,           /* ST [rd+off], rs */
	VM_OP_LDB,          /* LD byte */
	VM_OP_STB,          /* ST byte */
	VM_OP_LDH,          /* LD halfword (16-bit) */
	VM_OP_STH,          /* ST halfword */

	/* Arithmetic */
	VM_OP_ADD = 0x10,   /* ADD rd, rs1, rs2 */
	VM_OP_SUB,          /* SUB rd, rs1, rs2 */
	VM_OP_MUL,          /* MUL rd, rs1, rs2 */
	VM_OP_DIV,          /* DIV rd, rs1, rs2 */
	VM_OP_MOD,          /* MOD rd, rs1, rs2 */
	VM_OP_NEG,          /* NEG rd, rs */
	VM_OP_ADDI,         /* ADDI rd, rs, imm */
	VM_OP_SUBI,         /* SUBI rd, rs, imm */

	/* Logical */
	VM_OP_AND = 0x20,   /* AND rd, rs1, rs2 */
	VM_OP_OR,           /* OR rd, rs1, rs2 */
	VM_OP_XOR,          /* XOR rd, rs1, rs2 */
	VM_OP_NOT,          /* NOT rd, rs */
	VM_OP_SHL,          /* SHL rd, rs, bits */
	VM_OP_SHR,          /* SHR rd, rs, bits */
	VM_OP_SAR,          /* SAR rd, rs, bits (arithmetic shift) */
	VM_OP_ANDI,         /* ANDI rd, rs, imm */
	VM_OP_ORI,          /* ORI rd, rs, imm */
	VM_OP_XORI,         /* XORI rd, rs, imm */

	/* Comparison */
	VM_OP_CMP = 0x30,   /* CMP rs1, rs2 */
	VM_OP_TST,          /* TST rs */
	VM_OP_CMPI,         /* CMPI rs, imm */

	/* Control Flow */
	VM_OP_JMP = 0x40,   /* JMP addr */
	VM_OP_JZ,           /* JZ addr (jump if zero) */
	VM_OP_JNZ,          /* JNZ addr (jump if not zero) */
	VM_OP_JL,           /* JL addr (jump if less) */
	VM_OP_JLE,          /* JLE addr (jump if less or equal) */
	VM_OP_JG,           /* JG addr (jump if greater) */
	VM_OP_JGE,          /* JGE addr (jump if greater or equal) */
	VM_OP_CALL,         /* CALL addr */
	VM_OP_RET,          /* RET */

	/* Stack */
	VM_OP_PUSH = 0x50,  /* PUSH rs */
	VM_OP_POP,          /* POP rd */

	/* System */
	VM_OP_SYSCALL = 0x60, /* SYSCALL n */
	VM_OP_HALT,         /* HALT */
	VM_OP_PRINT,        /* PRINT rs (debug) */
	VM_OP_PRINTC,       /* PRINTC rs (print as char) */
	VM_OP_INPUT,        /* INPUT rd */

	/* Extended */
	VM_OP_LEA = 0x70,   /* LEA rd, [rs+off] (load effective address) */
	VM_OP_SWAP,         /* SWAP rd, rs */
	VM_OP_SEXT,         /* SEXT rd, rs (sign extend) */
	VM_OP_ZEXT,         /* ZEXT rd, rs (zero extend) */

	VM_OP_MAX = 0xFF
} vm_opcode_t;

/*
 * Flags Register Bits
 */
#define VM_FLAG_ZERO     0x01  /* Z - Zero */
#define VM_FLAG_NEGATIVE 0x02  /* N - Negative */
#define VM_FLAG_CARRY    0x04  /* C - Carry */
#define VM_FLAG_OVERFLOW 0x08  /* V - Overflow */

/*
 * System Call Numbers
 */
typedef enum {
	VM_SYSCALL_EXIT = 0,
	VM_SYSCALL_READ,
	VM_SYSCALL_WRITE,
	VM_SYSCALL_OPEN,
	VM_SYSCALL_CLOSE,
	VM_SYSCALL_GETCHAR,
	VM_SYSCALL_PUTCHAR,
	VM_SYSCALL_MALLOC,
	VM_SYSCALL_FREE,
	VM_SYSCALL_TIME
} vm_syscall_t;

/*
 * Instruction Structure (Binary Format)
 */
typedef struct {
	uint8_t opcode;
	uint8_t rd;       /* Destination register */
	uint8_t rs1;      /* Source register 1 */
	uint8_t rs2;      /* Source register 2 */
	int32_t imm;      /* Immediate value */
} vm_instruction_t;

/*
 * VM State
 */
typedef struct {
	int32_t regs[VM_NUM_REGISTERS];  /* Registers */
	uint32_t pc;                      /* Program counter */
	uint32_t flags;                   /* Flags register */
	uint8_t *memory;                  /* Memory */
	size_t mem_size;                  /* Memory size */
	uint32_t stack_base;              /* Stack base address */
	int running;                      /* Execution state */
	int debug;                        /* Debug mode */
	FILE *debug_output;               /* Debug output stream */
} vm_state_t;

/*
 * VM Statistics
 */
typedef struct {
	uint64_t instructions_executed;
	uint64_t memory_reads;
	uint64_t memory_writes;
	uint64_t syscalls;
} vm_stats_t;

/*
 * Bytecode Format Types
 */
typedef enum {
	VM_FORMAT_BINARY,   /* Binary bytecode */
	VM_FORMAT_TEXT,     /* Text assembly */
	VM_FORMAT_HEX       /* Hex dump */
} vm_format_t;

/*
 * VM Creation and Destruction
 */

/* Create VM with default memory size */
vm_state_t *vm_create(void);

/* Create VM with specific memory size */
vm_state_t *vm_create_with_memory(size_t mem_size);

/* Destroy VM */
void vm_destroy(vm_state_t *vm);

/* Reset VM state */
void vm_reset(vm_state_t *vm);

/*
 * Program Loading
 */

/* Load program from binary bytecode */
int vm_load_binary(vm_state_t *vm, const uint8_t *bytecode, size_t size);

/* Load program from text assembly */
int vm_load_text(vm_state_t *vm, const char *assembly);

/* Load program from file */
int vm_load_file(vm_state_t *vm, const char *filename, vm_format_t format);

/* Load program from string */
int vm_load_string(vm_state_t *vm, const char *program);

/*
 * Execution
 */

/* Execute one instruction */
int vm_step(vm_state_t *vm);

/* Execute until halt or error */
int vm_run(vm_state_t *vm);

/* Execute with instruction limit */
int vm_run_limited(vm_state_t *vm, uint64_t max_instructions);

/* Continue execution from current PC */
int vm_continue(vm_state_t *vm);

/*
 * Register Access
 */

/* Get register value */
int32_t vm_get_register(const vm_state_t *vm, vm_register_t reg);

/* Set register value */
void vm_set_register(vm_state_t *vm, vm_register_t reg, int32_t value);

/* Get register name */
const char *vm_register_name(vm_register_t reg);

/* Parse register name */
int vm_parse_register(const char *name, vm_register_t *reg);

/*
 * Memory Access
 */

/* Read word (32-bit) from memory */
int32_t vm_mem_read(const vm_state_t *vm, uint32_t addr);

/* Write word to memory */
int vm_mem_write(vm_state_t *vm, uint32_t addr, int32_t value);

/* Read byte from memory */
uint8_t vm_mem_read_byte(const vm_state_t *vm, uint32_t addr);

/* Write byte to memory */
int vm_mem_write_byte(vm_state_t *vm, uint32_t addr, uint8_t value);

/* Read halfword (16-bit) from memory */
uint16_t vm_mem_read_half(const vm_state_t *vm, uint32_t addr);

/* Write halfword to memory */
int vm_mem_write_half(vm_state_t *vm, uint32_t addr, uint16_t value);

/* Get memory pointer (for bulk operations) */
uint8_t *vm_mem_ptr(vm_state_t *vm, uint32_t addr);

/*
 * Stack Operations
 */

/* Push value onto stack */
int vm_stack_push(vm_state_t *vm, int32_t value);

/* Pop value from stack */
int32_t vm_stack_pop(vm_state_t *vm);

/* Get stack pointer */
uint32_t vm_stack_pointer(const vm_state_t *vm);

/*
 * Flags
 */

/* Get flags register */
uint32_t vm_get_flags(const vm_state_t *vm);

/* Set flags register */
void vm_set_flags(vm_state_t *vm, uint32_t flags);

/* Test flag */
int vm_test_flag(const vm_state_t *vm, uint32_t flag);

/* Update flags based on result */
void vm_update_flags(vm_state_t *vm, int32_t result, int carry, int overflow);

/*
 * Instruction Assembly/Disassembly
 */

/* Assemble text instruction to binary */
int vm_assemble_instruction(const char *text, vm_instruction_t *instr);

/* Disassemble binary instruction to text */
int vm_disassemble_instruction(const vm_instruction_t *instr, char *buffer, size_t size);

/* Get instruction name */
const char *vm_opcode_name(vm_opcode_t opcode);

/* Parse opcode name */
int vm_parse_opcode(const char *name, vm_opcode_t *opcode);

/*
 * Assembler
 */

/* Assemble text program to binary */
uint8_t *vm_assemble(const char *text, size_t *out_size);

/* Disassemble binary program to text */
char *vm_disassemble(const uint8_t *bytecode, size_t size);

/* Assemble file */
int vm_assemble_file(const char *input_file, const char *output_file);

/* Disassemble file */
int vm_disassemble_file(const char *input_file, const char *output_file);

/*
 * Labels and Symbols
 */

/* Symbol table for assembler */
typedef struct vm_symbol vm_symbol_t;

/* Create symbol table */
vm_symbol_t *vm_symbol_table_create(void);

/* Add symbol */
int vm_symbol_add(vm_symbol_t *table, const char *name, uint32_t address);

/* Lookup symbol */
int vm_symbol_lookup(const vm_symbol_t *table, const char *name, uint32_t *address);

/* Free symbol table */
void vm_symbol_table_free(vm_symbol_t *table);

/*
 * Debugging
 */

/* Enable debug mode */
void vm_enable_debug(vm_state_t *vm, FILE *output);

/* Disable debug mode */
void vm_disable_debug(vm_state_t *vm);

/* Print VM state */
void vm_print_state(const vm_state_t *vm, FILE *output);

/* Print registers */
void vm_print_registers(const vm_state_t *vm, FILE *output);

/* Print memory region */
void vm_print_memory(const vm_state_t *vm, uint32_t start, uint32_t end, FILE *output);

/* Print disassembly around PC */
void vm_print_disassembly(const vm_state_t *vm, int before, int after, FILE *output);

/* Trace execution */
void vm_trace(vm_state_t *vm, int enable);

/*
 * Statistics
 */

/* Get execution statistics */
void vm_get_stats(const vm_state_t *vm, vm_stats_t *stats);

/* Reset statistics */
void vm_reset_stats(vm_state_t *vm);

/* Print statistics */
void vm_print_stats(const vm_state_t *vm, FILE *output);

/*
 * System Calls
 */

/* Register syscall handler */
typedef int32_t (*vm_syscall_handler_t)(vm_state_t *vm, uint32_t syscall_num);
void vm_register_syscall_handler(vm_state_t *vm, vm_syscall_handler_t handler);

/* Default syscall handler */
int32_t vm_default_syscall_handler(vm_state_t *vm, uint32_t syscall_num);

/*
 * Error Handling
 */

typedef enum {
	VM_OK = 0,
	VM_ERROR_INVALID_OPCODE,
	VM_ERROR_INVALID_REGISTER,
	VM_ERROR_MEMORY_OUT_OF_BOUNDS,
	VM_ERROR_STACK_OVERFLOW,
	VM_ERROR_STACK_UNDERFLOW,
	VM_ERROR_DIVISION_BY_ZERO,
	VM_ERROR_INVALID_INSTRUCTION,
	VM_ERROR_ASSEMBLY_ERROR,
	VM_ERROR_FILE_ERROR,
	VM_ERROR_HALT
} vm_error_t;

/* Get last error */
vm_error_t vm_get_error(const vm_state_t *vm);

/* Get error message */
const char *vm_error_message(vm_error_t error);

/* Set error handler */
typedef void (*vm_error_handler_t)(vm_state_t *vm, vm_error_t error);
void vm_set_error_handler(vm_state_t *vm, vm_error_handler_t handler);

/*
 * Utilities
 */

/* Sign extend value */
int32_t vm_sign_extend(int32_t value, int bits);

/* Zero extend value */
uint32_t vm_zero_extend(uint32_t value, int bits);

/* Check if value overflows */
int vm_check_overflow(int32_t a, int32_t b, int32_t result);

/* Check if operation produces carry */
int vm_check_carry(uint32_t a, uint32_t b, uint32_t result);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_VM_H_ */
