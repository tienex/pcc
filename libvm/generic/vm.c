/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Bootstrapping Virtual Machine - Implementation
 */

#include "vm.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

/*
 * Symbol Table (for assembler)
 */
typedef struct symbol_entry {
	char *name;
	uint32_t address;
	struct symbol_entry *next;
} symbol_entry_t;

struct vm_symbol {
	symbol_entry_t *head;
	size_t count;
};

/*
 * Extended VM State (internal)
 */
typedef struct {
	vm_state_t base;
	vm_stats_t stats;
	vm_error_t last_error;
	vm_syscall_handler_t syscall_handler;
	vm_error_handler_t error_handler;
	int trace_enabled;
} vm_state_internal_t;

/*
 * Instruction Mnemonics
 */
static const char *opcode_names[] = {
	[VM_OP_NOP] = "nop",
	[VM_OP_MOV] = "mov",
	[VM_OP_LDI] = "ldi",
	[VM_OP_LD] = "ld",
	[VM_OP_ST] = "st",
	[VM_OP_LDB] = "ldb",
	[VM_OP_STB] = "stb",
	[VM_OP_LDH] = "ldh",
	[VM_OP_STH] = "sth",
	[VM_OP_ADD] = "add",
	[VM_OP_SUB] = "sub",
	[VM_OP_MUL] = "mul",
	[VM_OP_DIV] = "div",
	[VM_OP_MOD] = "mod",
	[VM_OP_NEG] = "neg",
	[VM_OP_ADDI] = "addi",
	[VM_OP_SUBI] = "subi",
	[VM_OP_AND] = "and",
	[VM_OP_OR] = "or",
	[VM_OP_XOR] = "xor",
	[VM_OP_NOT] = "not",
	[VM_OP_SHL] = "shl",
	[VM_OP_SHR] = "shr",
	[VM_OP_SAR] = "sar",
	[VM_OP_ANDI] = "andi",
	[VM_OP_ORI] = "ori",
	[VM_OP_XORI] = "xori",
	[VM_OP_CMP] = "cmp",
	[VM_OP_TST] = "tst",
	[VM_OP_CMPI] = "cmpi",
	[VM_OP_JMP] = "jmp",
	[VM_OP_JZ] = "jz",
	[VM_OP_JNZ] = "jnz",
	[VM_OP_JL] = "jl",
	[VM_OP_JLE] = "jle",
	[VM_OP_JG] = "jg",
	[VM_OP_JGE] = "jge",
	[VM_OP_CALL] = "call",
	[VM_OP_RET] = "ret",
	[VM_OP_PUSH] = "push",
	[VM_OP_POP] = "pop",
	[VM_OP_SYSCALL] = "syscall",
	[VM_OP_HALT] = "halt",
	[VM_OP_PRINT] = "print",
	[VM_OP_PRINTC] = "printc",
	[VM_OP_INPUT] = "input",
	[VM_OP_LEA] = "lea",
	[VM_OP_SWAP] = "swap",
	[VM_OP_SEXT] = "sext",
	[VM_OP_ZEXT] = "zext",
};

static const char *register_names[] = {
	"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
	"r8", "r9", "r10", "r11", "r12", "r13", "sp", "fp"
};

/*
 * Helper Functions
 */

static vm_state_internal_t *vm_to_internal(vm_state_t *vm) {
	return (vm_state_internal_t *)vm;
}

static const vm_state_internal_t *vm_to_internal_const(const vm_state_t *vm) {
	return (const vm_state_internal_t *)vm;
}

static void vm_set_error(vm_state_t *vm, vm_error_t error) {
	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->last_error = error;
	if (vmi->error_handler) {
		vmi->error_handler(vm, error);
	}
}

static int vm_check_mem_bounds(const vm_state_t *vm, uint32_t addr, size_t size) {
	return (addr + size <= vm->mem_size);
}

/*
 * VM Creation and Destruction
 */

vm_state_t *vm_create(void) {
	return vm_create_with_memory(VM_DEFAULT_MEM_SIZE);
}

vm_state_t *vm_create_with_memory(size_t mem_size) {
	vm_state_internal_t *vmi = calloc(1, sizeof(vm_state_internal_t));
	if (!vmi) return NULL;

	vmi->base.memory = calloc(1, mem_size);
	if (!vmi->base.memory) {
		free(vmi);
		return NULL;
	}

	vmi->base.mem_size = mem_size;
	vmi->base.stack_base = mem_size - VM_STACK_SIZE;
	vmi->base.regs[VM_SP] = vmi->base.stack_base;
	vmi->base.regs[VM_FP] = vmi->base.stack_base;
	vmi->base.running = 1;
	vmi->syscall_handler = vm_default_syscall_handler;

	return &vmi->base;
}

void vm_destroy(vm_state_t *vm) {
	if (!vm) return;
	free(vm->memory);
	free(vm);
}

void vm_reset(vm_state_t *vm) {
	if (!vm) return;

	memset(vm->regs, 0, sizeof(vm->regs));
	vm->pc = 0;
	vm->flags = 0;
	vm->running = 1;
	vm->regs[VM_SP] = vm->stack_base;
	vm->regs[VM_FP] = vm->stack_base;

	vm_state_internal_t *vmi = vm_to_internal(vm);
	memset(&vmi->stats, 0, sizeof(vm_stats_t));
	vmi->last_error = VM_OK;
}

/*
 * Register Access
 */

int32_t vm_get_register(const vm_state_t *vm, vm_register_t reg) {
	if (reg >= VM_NUM_REGISTERS) return 0;
	return vm->regs[reg];
}

void vm_set_register(vm_state_t *vm, vm_register_t reg, int32_t value) {
	if (reg >= VM_NUM_REGISTERS) return;
	vm->regs[reg] = value;
}

const char *vm_register_name(vm_register_t reg) {
	if (reg >= VM_NUM_REGISTERS) return "???";
	return register_names[reg];
}

int vm_parse_register(const char *name, vm_register_t *reg) {
	if (!name || !reg) return -1;

	/* Handle 'r' prefix */
	if (name[0] == 'r' && isdigit(name[1])) {
		int num = atoi(name + 1);
		if (num >= 0 && num < VM_NUM_REGISTERS) {
			*reg = num;
			return 0;
		}
	}

	/* Handle special names */
	if (strcmp(name, "sp") == 0) { *reg = VM_SP; return 0; }
	if (strcmp(name, "fp") == 0) { *reg = VM_FP; return 0; }

	return -1;
}

/*
 * Memory Access
 */

int32_t vm_mem_read(const vm_state_t *vm, uint32_t addr) {
	if (!vm_check_mem_bounds(vm, addr, 4)) return 0;
	vm_state_internal_t *vmi = vm_to_internal((vm_state_t *)vm);
	vmi->stats.memory_reads++;
	return *(int32_t *)(vm->memory + addr);
}

int vm_mem_write(vm_state_t *vm, uint32_t addr, int32_t value) {
	if (!vm_check_mem_bounds(vm, addr, 4)) {
		vm_set_error(vm, VM_ERROR_MEMORY_OUT_OF_BOUNDS);
		return -1;
	}
	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->stats.memory_writes++;
	*(int32_t *)(vm->memory + addr) = value;
	return 0;
}

uint8_t vm_mem_read_byte(const vm_state_t *vm, uint32_t addr) {
	if (!vm_check_mem_bounds(vm, addr, 1)) return 0;
	return vm->memory[addr];
}

int vm_mem_write_byte(vm_state_t *vm, uint32_t addr, uint8_t value) {
	if (!vm_check_mem_bounds(vm, addr, 1)) {
		vm_set_error(vm, VM_ERROR_MEMORY_OUT_OF_BOUNDS);
		return -1;
	}
	vm->memory[addr] = value;
	return 0;
}

uint16_t vm_mem_read_half(const vm_state_t *vm, uint32_t addr) {
	if (!vm_check_mem_bounds(vm, addr, 2)) return 0;
	return *(uint16_t *)(vm->memory + addr);
}

int vm_mem_write_half(vm_state_t *vm, uint32_t addr, uint16_t value) {
	if (!vm_check_mem_bounds(vm, addr, 2)) {
		vm_set_error(vm, VM_ERROR_MEMORY_OUT_OF_BOUNDS);
		return -1;
	}
	*(uint16_t *)(vm->memory + addr) = value;
	return 0;
}

uint8_t *vm_mem_ptr(vm_state_t *vm, uint32_t addr) {
	if (!vm_check_mem_bounds(vm, addr, 0)) return NULL;
	return vm->memory + addr;
}

/*
 * Stack Operations
 */

int vm_stack_push(vm_state_t *vm, int32_t value) {
	vm->regs[VM_SP] -= 4;
	if (vm->regs[VM_SP] < 0) {
		vm_set_error(vm, VM_ERROR_STACK_OVERFLOW);
		return -1;
	}
	return vm_mem_write(vm, vm->regs[VM_SP], value);
}

int32_t vm_stack_pop(vm_state_t *vm) {
	if (vm->regs[VM_SP] >= (int32_t)vm->stack_base) {
		vm_set_error(vm, VM_ERROR_STACK_UNDERFLOW);
		return 0;
	}
	int32_t value = vm_mem_read(vm, vm->regs[VM_SP]);
	vm->regs[VM_SP] += 4;
	return value;
}

uint32_t vm_stack_pointer(const vm_state_t *vm) {
	return vm->regs[VM_SP];
}

/*
 * Flags
 */

uint32_t vm_get_flags(const vm_state_t *vm) {
	return vm->flags;
}

void vm_set_flags(vm_state_t *vm, uint32_t flags) {
	vm->flags = flags;
}

int vm_test_flag(const vm_state_t *vm, uint32_t flag) {
	return (vm->flags & flag) != 0;
}

void vm_update_flags(vm_state_t *vm, int32_t result, int carry, int overflow) {
	vm->flags = 0;
	if (result == 0) vm->flags |= VM_FLAG_ZERO;
	if (result < 0) vm->flags |= VM_FLAG_NEGATIVE;
	if (carry) vm->flags |= VM_FLAG_CARRY;
	if (overflow) vm->flags |= VM_FLAG_OVERFLOW;
}

/*
 * Utilities
 */

int32_t vm_sign_extend(int32_t value, int bits) {
	int32_t sign_bit = 1 << (bits - 1);
	if (value & sign_bit) {
		return value | (~((1 << bits) - 1));
	}
	return value & ((1 << bits) - 1);
}

uint32_t vm_zero_extend(uint32_t value, int bits) {
	return value & ((1 << bits) - 1);
}

int vm_check_overflow(int32_t a, int32_t b, int32_t result) {
	/* Overflow if signs of operands are same, but result sign differs */
	return ((a ^ result) & (b ^ result)) < 0;
}

int vm_check_carry(uint32_t a, uint32_t b, uint32_t result) {
	return result < a;
}

/*
 * Instruction Execution
 */

int vm_step(vm_state_t *vm) {
	if (!vm || !vm->running) return -1;

	/* Fetch instruction */
	if (!vm_check_mem_bounds(vm, vm->pc, 8)) {
		vm_set_error(vm, VM_ERROR_MEMORY_OUT_OF_BOUNDS);
		return -1;
	}

	vm_instruction_t instr;
	instr.opcode = vm->memory[vm->pc];
	instr.rd = vm->memory[vm->pc + 1];
	instr.rs1 = vm->memory[vm->pc + 2];
	instr.rs2 = vm->memory[vm->pc + 3];
	instr.imm = *(int32_t *)(vm->memory + vm->pc + 4);

	uint32_t next_pc = vm->pc + 8;

	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->stats.instructions_executed++;

	/* Trace if enabled */
	if (vmi->trace_enabled && vm->debug_output) {
		char buf[256];
		vm_disassemble_instruction(&instr, buf, sizeof(buf));
		fprintf(vm->debug_output, "[%08X] %s\n", vm->pc, buf);
	}

	/* Decode and execute */
	switch (instr.opcode) {
	case VM_OP_NOP:
		break;

	case VM_OP_MOV:
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = vm->regs[instr.rs1];
		break;

	case VM_OP_LDI:
		if (instr.rd >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = instr.imm;
		break;

	case VM_OP_LD: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rs1] + instr.imm;
		vm->regs[instr.rd] = vm_mem_read(vm, addr);
		break;
	}

	case VM_OP_ST: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rd] + instr.imm;
		vm_mem_write(vm, addr, vm->regs[instr.rs1]);
		break;
	}

	case VM_OP_LDB: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rs1] + instr.imm;
		vm->regs[instr.rd] = (int32_t)(int8_t)vm_mem_read_byte(vm, addr);
		break;
	}

	case VM_OP_STB: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rd] + instr.imm;
		vm_mem_write_byte(vm, addr, (uint8_t)vm->regs[instr.rs1]);
		break;
	}

	case VM_OP_LDH: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rs1] + instr.imm;
		vm->regs[instr.rd] = (int32_t)(int16_t)vm_mem_read_half(vm, addr);
		break;
	}

	case VM_OP_STH: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		uint32_t addr = vm->regs[instr.rd] + instr.imm;
		vm_mem_write_half(vm, addr, (uint16_t)vm->regs[instr.rs1]);
		break;
	}

	case VM_OP_ADD: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = vm->regs[instr.rs2];
		int32_t result = a + b;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, vm_check_carry(a, b, result), vm_check_overflow(a, b, result));
		break;
	}

	case VM_OP_SUB: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = vm->regs[instr.rs2];
		int32_t result = a - b;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, vm_check_carry(a, -b, result), vm_check_overflow(a, -b, result));
		break;
	}

	case VM_OP_MUL: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] * vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_DIV: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		if (vm->regs[instr.rs2] == 0) {
			vm_set_error(vm, VM_ERROR_DIVISION_BY_ZERO);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] / vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_MOD: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		if (vm->regs[instr.rs2] == 0) {
			vm_set_error(vm, VM_ERROR_DIVISION_BY_ZERO);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] % vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_NEG: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = -vm->regs[instr.rs1];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_ADDI: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = instr.imm;
		int32_t result = a + b;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, vm_check_carry(a, b, result), vm_check_overflow(a, b, result));
		break;
	}

	case VM_OP_SUBI: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = instr.imm;
		int32_t result = a - b;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, vm_check_carry(a, -b, result), vm_check_overflow(a, -b, result));
		break;
	}

	case VM_OP_AND: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] & vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_OR: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] | vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_XOR: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] ^ vm->regs[instr.rs2];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_NOT: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = ~vm->regs[instr.rs1];
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_SHL: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] << instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_SHR: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = (uint32_t)vm->regs[instr.rs1] >> instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_SAR: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] >> instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_ANDI: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] & instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_ORI: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] | instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_XORI: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1] ^ instr.imm;
		vm->regs[instr.rd] = result;
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_CMP: {
		if (instr.rs1 >= VM_NUM_REGISTERS || instr.rs2 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = vm->regs[instr.rs2];
		int32_t result = a - b;
		vm_update_flags(vm, result, vm_check_carry(a, -b, result), vm_check_overflow(a, -b, result));
		break;
	}

	case VM_OP_TST: {
		if (instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t result = vm->regs[instr.rs1];
		vm_update_flags(vm, result, 0, 0);
		break;
	}

	case VM_OP_CMPI: {
		if (instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t a = vm->regs[instr.rs1];
		int32_t b = instr.imm;
		int32_t result = a - b;
		vm_update_flags(vm, result, vm_check_carry(a, -b, result), vm_check_overflow(a, -b, result));
		break;
	}

	case VM_OP_JMP:
		next_pc = instr.imm;
		break;

	case VM_OP_JZ:
		if (vm->flags & VM_FLAG_ZERO)
			next_pc = instr.imm;
		break;

	case VM_OP_JNZ:
		if (!(vm->flags & VM_FLAG_ZERO))
			next_pc = instr.imm;
		break;

	case VM_OP_JL:
		/* Jump if less (signed): N != V */
		if (!!(vm->flags & VM_FLAG_NEGATIVE) != !!(vm->flags & VM_FLAG_OVERFLOW))
			next_pc = instr.imm;
		break;

	case VM_OP_JLE:
		/* Jump if less or equal (signed): Z || (N != V) */
		if ((vm->flags & VM_FLAG_ZERO) ||
		    (!!(vm->flags & VM_FLAG_NEGATIVE) != !!(vm->flags & VM_FLAG_OVERFLOW)))
			next_pc = instr.imm;
		break;

	case VM_OP_JG:
		/* Jump if greater (signed): !Z && (N == V) */
		if (!(vm->flags & VM_FLAG_ZERO) &&
		    (!!(vm->flags & VM_FLAG_NEGATIVE) == !!(vm->flags & VM_FLAG_OVERFLOW)))
			next_pc = instr.imm;
		break;

	case VM_OP_JGE:
		/* Jump if greater or equal (signed): N == V */
		if (!!(vm->flags & VM_FLAG_NEGATIVE) == !!(vm->flags & VM_FLAG_OVERFLOW))
			next_pc = instr.imm;
		break;

	case VM_OP_CALL:
		if (vm_stack_push(vm, next_pc) < 0)
			return -1;
		next_pc = instr.imm;
		break;

	case VM_OP_RET:
		next_pc = vm_stack_pop(vm);
		break;

	case VM_OP_PUSH:
		if (instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		if (vm_stack_push(vm, vm->regs[instr.rs1]) < 0)
			return -1;
		break;

	case VM_OP_POP:
		if (instr.rd >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = vm_stack_pop(vm);
		break;

	case VM_OP_SYSCALL: {
		vmi->stats.syscalls++;
		if (vmi->syscall_handler) {
			int32_t result = vmi->syscall_handler(vm, instr.imm);
			vm->regs[VM_R0] = result;
		}
		break;
	}

	case VM_OP_HALT:
		vm->running = 0;
		vm_set_error(vm, VM_ERROR_HALT);
		return 0;

	case VM_OP_PRINT:
		if (instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		printf("%d\n", vm->regs[instr.rs1]);
		break;

	case VM_OP_PRINTC:
		if (instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		putchar(vm->regs[instr.rs1]);
		break;

	case VM_OP_INPUT:
		if (instr.rd >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		scanf("%d", &vm->regs[instr.rd]);
		break;

	case VM_OP_LEA:
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = vm->regs[instr.rs1] + instr.imm;
		break;

	case VM_OP_SWAP: {
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		int32_t temp = vm->regs[instr.rd];
		vm->regs[instr.rd] = vm->regs[instr.rs1];
		vm->regs[instr.rs1] = temp;
		break;
	}

	case VM_OP_SEXT:
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = vm_sign_extend(vm->regs[instr.rs1], instr.imm);
		break;

	case VM_OP_ZEXT:
		if (instr.rd >= VM_NUM_REGISTERS || instr.rs1 >= VM_NUM_REGISTERS) {
			vm_set_error(vm, VM_ERROR_INVALID_REGISTER);
			return -1;
		}
		vm->regs[instr.rd] = vm_zero_extend(vm->regs[instr.rs1], instr.imm);
		break;

	default:
		vm_set_error(vm, VM_ERROR_INVALID_OPCODE);
		return -1;
	}

	vm->pc = next_pc;
	return 0;
}

int vm_run(vm_state_t *vm) {
	if (!vm) return -1;

	while (vm->running) {
		if (vm_step(vm) < 0) {
			if (vm_to_internal_const(vm)->last_error == VM_ERROR_HALT)
				return 0;
			return -1;
		}
	}
	return 0;
}

int vm_run_limited(vm_state_t *vm, uint64_t max_instructions) {
	if (!vm) return -1;

	uint64_t count = 0;
	while (vm->running && count < max_instructions) {
		if (vm_step(vm) < 0) {
			if (vm_to_internal_const(vm)->last_error == VM_ERROR_HALT)
				return 0;
			return -1;
		}
		count++;
	}
	return (count >= max_instructions) ? 1 : 0;
}

int vm_continue(vm_state_t *vm) {
	if (!vm) return -1;
	vm->running = 1;
	return vm_run(vm);
}

/*
 * Opcodes
 */

const char *vm_opcode_name(vm_opcode_t opcode) {
	if (opcode >= sizeof(opcode_names) / sizeof(opcode_names[0]) || !opcode_names[opcode])
		return "???";
	return opcode_names[opcode];
}

int vm_parse_opcode(const char *name, vm_opcode_t *opcode) {
	if (!name || !opcode) return -1;

	for (size_t i = 0; i < sizeof(opcode_names) / sizeof(opcode_names[0]); i++) {
		if (opcode_names[i] && strcmp(name, opcode_names[i]) == 0) {
			*opcode = i;
			return 0;
		}
	}
	return -1;
}

/*
 * Symbol Table
 */

vm_symbol_t *vm_symbol_table_create(void) {
	vm_symbol_t *table = calloc(1, sizeof(vm_symbol_t));
	return table;
}

int vm_symbol_add(vm_symbol_t *table, const char *name, uint32_t address) {
	if (!table || !name) return -1;

	symbol_entry_t *entry = malloc(sizeof(symbol_entry_t));
	if (!entry) return -1;

	entry->name = strdup(name);
	entry->address = address;
	entry->next = table->head;
	table->head = entry;
	table->count++;
	return 0;
}

int vm_symbol_lookup(const vm_symbol_t *table, const char *name, uint32_t *address) {
	if (!table || !name || !address) return -1;

	for (symbol_entry_t *entry = table->head; entry; entry = entry->next) {
		if (strcmp(entry->name, name) == 0) {
			*address = entry->address;
			return 0;
		}
	}
	return -1;
}

void vm_symbol_table_free(vm_symbol_t *table) {
	if (!table) return;

	symbol_entry_t *entry = table->head;
	while (entry) {
		symbol_entry_t *next = entry->next;
		free(entry->name);
		free(entry);
		entry = next;
	}
	free(table);
}

/*
 * Instruction Assembly/Disassembly
 */

int vm_assemble_instruction(const char *text, vm_instruction_t *instr) {
	if (!text || !instr) return -1;

	char opcode_str[32] = {0};
	char rd_str[32] = {0}, rs1_str[32] = {0}, rs2_str[32] = {0};
	int32_t imm = 0;

	/* Skip leading whitespace and comments */
	while (*text && isspace(*text)) text++;
	if (*text == '#' || *text == ';' || *text == '\0') {
		instr->opcode = VM_OP_NOP;
		return 0;
	}

	/* Parse opcode */
	sscanf(text, "%31s", opcode_str);

	vm_opcode_t opcode;
	if (vm_parse_opcode(opcode_str, &opcode) < 0)
		return -1;

	instr->opcode = opcode;
	instr->rd = 0;
	instr->rs1 = 0;
	instr->rs2 = 0;
	instr->imm = 0;

	/* Move past opcode */
	text += strlen(opcode_str);
	while (*text && isspace(*text)) text++;

	/* Parse operands based on instruction type */
	switch (opcode) {
	case VM_OP_NOP:
	case VM_OP_HALT:
	case VM_OP_RET:
		/* No operands */
		break;

	case VM_OP_LDI:
	case VM_OP_CMPI:
	case VM_OP_ADDI:
	case VM_OP_SUBI:
	case VM_OP_ANDI:
	case VM_OP_ORI:
	case VM_OP_XORI:
	case VM_OP_SHL:
	case VM_OP_SHR:
	case VM_OP_SAR:
		/* rd, imm */
		sscanf(text, " %31[^,], %d", rd_str, &imm);
		if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
			return -1;
		instr->imm = imm;
		break;

	case VM_OP_MOV:
	case VM_OP_NEG:
	case VM_OP_NOT:
	case VM_OP_SWAP:
		/* rd, rs1 */
		sscanf(text, " %31[^,], %31s", rd_str, rs1_str);
		if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
			return -1;
		if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
			return -1;
		break;

	case VM_OP_ADD:
	case VM_OP_SUB:
	case VM_OP_MUL:
	case VM_OP_DIV:
	case VM_OP_MOD:
	case VM_OP_AND:
	case VM_OP_OR:
	case VM_OP_XOR:
		/* rd, rs1, rs2 */
		sscanf(text, " %31[^,], %31[^,], %31s", rd_str, rs1_str, rs2_str);
		if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
			return -1;
		if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
			return -1;
		if (vm_parse_register(rs2_str, (vm_register_t *)&instr->rs2) < 0)
			return -1;
		break;

	case VM_OP_CMP:
	case VM_OP_TST:
		/* rs1, rs2 */
		sscanf(text, " %31[^,], %31s", rs1_str, rs2_str);
		if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
			return -1;
		if (opcode == VM_OP_CMP) {
			if (vm_parse_register(rs2_str, (vm_register_t *)&instr->rs2) < 0)
				return -1;
		}
		break;

	case VM_OP_JMP:
	case VM_OP_JZ:
	case VM_OP_JNZ:
	case VM_OP_JL:
	case VM_OP_JLE:
	case VM_OP_JG:
	case VM_OP_JGE:
	case VM_OP_CALL:
		/* addr (immediate) */
		sscanf(text, " %d", &imm);
		instr->imm = imm;
		break;

	case VM_OP_PUSH:
	case VM_OP_PRINT:
	case VM_OP_PRINTC:
		/* rs1 */
		sscanf(text, " %31s", rs1_str);
		if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
			return -1;
		break;

	case VM_OP_POP:
	case VM_OP_INPUT:
		/* rd */
		sscanf(text, " %31s", rd_str);
		if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
			return -1;
		break;

	case VM_OP_LD:
	case VM_OP_LDB:
	case VM_OP_LDH:
	case VM_OP_LEA:
		/* rd, [rs1+imm] or rd, [rs1] */
		if (sscanf(text, " %31[^,], [ %31[^]+-] + %d ]", rd_str, rs1_str, &imm) >= 2 ||
		    sscanf(text, " %31[^,], [ %31[^]] ]", rd_str, rs1_str) >= 2) {
			if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
				return -1;
			if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
				return -1;
			instr->imm = imm;
		} else {
			return -1;
		}
		break;

	case VM_OP_ST:
	case VM_OP_STB:
	case VM_OP_STH:
		/* [rd+imm], rs1 or [rd], rs1 */
		if (sscanf(text, " [ %31[^]+-] + %d ], %31s", rd_str, &imm, rs1_str) >= 2 ||
		    sscanf(text, " [ %31[^]] ], %31s", rd_str, rs1_str) >= 2) {
			if (vm_parse_register(rd_str, (vm_register_t *)&instr->rd) < 0)
				return -1;
			if (vm_parse_register(rs1_str, (vm_register_t *)&instr->rs1) < 0)
				return -1;
			instr->imm = imm;
		} else {
			return -1;
		}
		break;

	case VM_OP_SYSCALL:
		/* syscall number */
		sscanf(text, " %d", &imm);
		instr->imm = imm;
		break;

	default:
		break;
	}

	return 0;
}

int vm_disassemble_instruction(const vm_instruction_t *instr, char *buffer, size_t size) {
	if (!instr || !buffer || size == 0) return -1;

	const char *opcode = vm_opcode_name(instr->opcode);

	switch (instr->opcode) {
	case VM_OP_NOP:
	case VM_OP_HALT:
	case VM_OP_RET:
		snprintf(buffer, size, "%s", opcode);
		break;

	case VM_OP_LDI:
	case VM_OP_CMPI:
	case VM_OP_ADDI:
	case VM_OP_SUBI:
	case VM_OP_ANDI:
	case VM_OP_ORI:
	case VM_OP_XORI:
	case VM_OP_SHL:
	case VM_OP_SHR:
	case VM_OP_SAR:
		snprintf(buffer, size, "%s %s, %d", opcode,
		         vm_register_name(instr->rd), instr->imm);
		break;

	case VM_OP_MOV:
	case VM_OP_NEG:
	case VM_OP_NOT:
	case VM_OP_SWAP:
		snprintf(buffer, size, "%s %s, %s", opcode,
		         vm_register_name(instr->rd), vm_register_name(instr->rs1));
		break;

	case VM_OP_ADD:
	case VM_OP_SUB:
	case VM_OP_MUL:
	case VM_OP_DIV:
	case VM_OP_MOD:
	case VM_OP_AND:
	case VM_OP_OR:
	case VM_OP_XOR:
		snprintf(buffer, size, "%s %s, %s, %s", opcode,
		         vm_register_name(instr->rd), vm_register_name(instr->rs1),
		         vm_register_name(instr->rs2));
		break;

	case VM_OP_CMP:
		snprintf(buffer, size, "%s %s, %s", opcode,
		         vm_register_name(instr->rs1), vm_register_name(instr->rs2));
		break;

	case VM_OP_TST:
		snprintf(buffer, size, "%s %s", opcode, vm_register_name(instr->rs1));
		break;

	case VM_OP_JMP:
	case VM_OP_JZ:
	case VM_OP_JNZ:
	case VM_OP_JL:
	case VM_OP_JLE:
	case VM_OP_JG:
	case VM_OP_JGE:
	case VM_OP_CALL:
		snprintf(buffer, size, "%s 0x%08X", opcode, instr->imm);
		break;

	case VM_OP_PUSH:
	case VM_OP_PRINT:
	case VM_OP_PRINTC:
		snprintf(buffer, size, "%s %s", opcode, vm_register_name(instr->rs1));
		break;

	case VM_OP_POP:
	case VM_OP_INPUT:
		snprintf(buffer, size, "%s %s", opcode, vm_register_name(instr->rd));
		break;

	case VM_OP_LD:
	case VM_OP_LDB:
	case VM_OP_LDH:
	case VM_OP_LEA:
		if (instr->imm == 0)
			snprintf(buffer, size, "%s %s, [%s]", opcode,
			         vm_register_name(instr->rd), vm_register_name(instr->rs1));
		else
			snprintf(buffer, size, "%s %s, [%s+%d]", opcode,
			         vm_register_name(instr->rd), vm_register_name(instr->rs1),
			         instr->imm);
		break;

	case VM_OP_ST:
	case VM_OP_STB:
	case VM_OP_STH:
		if (instr->imm == 0)
			snprintf(buffer, size, "%s [%s], %s", opcode,
			         vm_register_name(instr->rd), vm_register_name(instr->rs1));
		else
			snprintf(buffer, size, "%s [%s+%d], %s", opcode,
			         vm_register_name(instr->rd), instr->imm,
			         vm_register_name(instr->rs1));
		break;

	case VM_OP_SYSCALL:
		snprintf(buffer, size, "%s %d", opcode, instr->imm);
		break;

	default:
		snprintf(buffer, size, "??? 0x%02X", instr->opcode);
		break;
	}

	return 0;
}

/*
 * Program Loading (continued in next part...)
 */

/*
 * Program Loading
 */

int vm_load_binary(vm_state_t *vm, const uint8_t *bytecode, size_t size) {
	if (!vm || !bytecode) return -1;
	if (size > vm->mem_size) return -1;

	memcpy(vm->memory, bytecode, size);
	vm->pc = 0;
	vm->running = 1;
	return 0;
}

int vm_load_text(vm_state_t *vm, const char *assembly) {
	if (!vm || !assembly) return -1;

	uint8_t *binary = NULL;
	size_t binary_size = 0;

	binary = vm_assemble(assembly, &binary_size);
	if (!binary) return -1;

	int result = vm_load_binary(vm, binary, binary_size);
	free(binary);
	return result;
}

int vm_load_file(vm_state_t *vm, const char *filename, vm_format_t format) {
	if (!vm || !filename) return -1;

	FILE *f = fopen(filename, format == VM_FORMAT_TEXT ? "r" : "rb");
	if (!f) return -1;

	fseek(f, 0, SEEK_END);
	long size = ftell(f);
	fseek(f, 0, SEEK_SET);

	if (format == VM_FORMAT_TEXT) {
		char *text = malloc(size + 1);
		if (!text) {
			fclose(f);
			return -1;
		}
		fread(text, 1, size, f);
		text[size] = '\0';
		fclose(f);

		int result = vm_load_text(vm, text);
		free(text);
		return result;
	} else {
		uint8_t *binary = malloc(size);
		if (!binary) {
			fclose(f);
			return -1;
		}
		fread(binary, 1, size, f);
		fclose(f);

		int result = vm_load_binary(vm, binary, size);
		free(binary);
		return result;
	}
}

int vm_load_string(vm_state_t *vm, const char *program) {
	return vm_load_text(vm, program);
}

/*
 * Assembler
 */

uint8_t *vm_assemble(const char *text, size_t *out_size) {
	if (!text || !out_size) return NULL;

	/* First pass: count instructions and build symbol table */
	vm_symbol_t *symbols = vm_symbol_table_create();
	size_t num_instructions = 0;
	uint32_t address = 0;

	const char *line = text;
	while (*line) {
		/* Skip whitespace */
		while (*line && isspace(*line) && *line != '\n') line++;

		/* End of line */
		if (*line == '\n') {
			line++;
			continue;
		}
		if (*line == '\0') break;

		/* Comment */
		if (*line == '#' || *line == ';') {
			while (*line && *line != '\n') line++;
			continue;
		}

		/* Label */
		if (isalpha(*line) || *line == '_') {
			const char *label_start = line;
			while (*line && (isalnum(*line) || *line == '_')) line++;

			if (*line == ':') {
				/* This is a label */
				char label[256];
				size_t label_len = line - label_start;
				if (label_len >= sizeof(label)) label_len = sizeof(label) - 1;
				memcpy(label, label_start, label_len);
				label[label_len] = '\0';

				vm_symbol_add(symbols, label, address);
				line++; /* Skip ':' */
				continue;
			} else {
				/* This is an instruction, rewind */
				line = label_start;
			}
		}

		/* Instruction */
		while (*line && *line != '\n') line++;
		num_instructions++;
		address += 8;  /* Each instruction is 8 bytes */
	}

	/* Allocate output buffer */
	size_t binary_size = num_instructions * 8;
	uint8_t *binary = calloc(1, binary_size);
	if (!binary) {
		vm_symbol_table_free(symbols);
		return NULL;
	}

	/* Second pass: assemble instructions */
	line = text;
	address = 0;
	size_t instr_count = 0;

	while (*line && instr_count < num_instructions) {
		/* Skip whitespace */
		while (*line && isspace(*line) && *line != '\n') line++;

		/* End of line */
		if (*line == '\n') {
			line++;
			continue;
		}
		if (*line == '\0') break;

		/* Comment */
		if (*line == '#' || *line == ';') {
			while (*line && *line != '\n') line++;
			continue;
		}

		/* Label */
		if (isalpha(*line) || *line == '_') {
			const char *check = line;
			while (*check && (isalnum(*check) || *check == '_')) check++;
			if (*check == ':') {
				line = check + 1;
				continue;
			}
		}

		/* Get line */
		char line_buf[VM_MAX_INSTRUCTION_LEN];
		const char *line_end = line;
		while (*line_end && *line_end != '\n') line_end++;
		size_t line_len = line_end - line;
		if (line_len >= sizeof(line_buf)) line_len = sizeof(line_buf) - 1;
		memcpy(line_buf, line, line_len);
		line_buf[line_len] = '\0';

		/* Resolve label references in line */
		char resolved_line[VM_MAX_INSTRUCTION_LEN];
		strcpy(resolved_line, line_buf);

		/* Check if line contains a label reference (simple heuristic) */
		for (symbol_entry_t *sym = symbols->head; sym; sym = sym->next) {
			char *pos = strstr(resolved_line, sym->name);
			if (pos) {
				/* Check if it's a whole word */
				int is_word = 1;
				if (pos > resolved_line && (isalnum(pos[-1]) || pos[-1] == '_'))
					is_word = 0;
				if (pos[strlen(sym->name)] && (isalnum(pos[strlen(sym->name)]) || pos[strlen(sym->name)] == '_'))
					is_word = 0;

				if (is_word) {
					/* Replace label with address */
					char temp[VM_MAX_INSTRUCTION_LEN];
					*pos = '\0';
					snprintf(temp, sizeof(temp), "%s%u%s",
					         resolved_line, sym->address,
					         pos + strlen(sym->name));
					strcpy(resolved_line, temp);
				}
			}
		}

		/* Assemble instruction */
		vm_instruction_t instr;
		if (vm_assemble_instruction(resolved_line, &instr) < 0) {
			free(binary);
			vm_symbol_table_free(symbols);
			return NULL;
		}

		/* Write to binary */
		binary[address] = instr.opcode;
		binary[address + 1] = instr.rd;
		binary[address + 2] = instr.rs1;
		binary[address + 3] = instr.rs2;
		*(int32_t *)(binary + address + 4) = instr.imm;

		address += 8;
		instr_count++;
		line = line_end;
		if (*line == '\n') line++;
	}

	vm_symbol_table_free(symbols);
	*out_size = binary_size;
	return binary;
}

char *vm_disassemble(const uint8_t *bytecode, size_t size) {
	if (!bytecode || size == 0) return NULL;

	size_t num_instructions = size / 8;
	size_t output_size = num_instructions * 128;  /* Estimate */
	char *output = malloc(output_size);
	if (!output) return NULL;

	char *out_ptr = output;
	size_t out_remaining = output_size;

	for (size_t i = 0; i < num_instructions && (i * 8 + 8) <= size; i++) {
		vm_instruction_t instr;
		instr.opcode = bytecode[i * 8];
		instr.rd = bytecode[i * 8 + 1];
		instr.rs1 = bytecode[i * 8 + 2];
		instr.rs2 = bytecode[i * 8 + 3];
		instr.imm = *(int32_t *)(bytecode + i * 8 + 4);

		char line[256];
		vm_disassemble_instruction(&instr, line, sizeof(line));

		int written = snprintf(out_ptr, out_remaining, "%08zX: %s\n", i * 8, line);
		if (written < 0 || (size_t)written >= out_remaining) break;

		out_ptr += written;
		out_remaining -= written;
	}

	return output;
}

int vm_assemble_file(const char *input_file, const char *output_file) {
	if (!input_file || !output_file) return -1;

	FILE *in = fopen(input_file, "r");
	if (!in) return -1;

	fseek(in, 0, SEEK_END);
	long size = ftell(in);
	fseek(in, 0, SEEK_SET);

	char *text = malloc(size + 1);
	if (!text) {
		fclose(in);
		return -1;
	}

	fread(text, 1, size, in);
	text[size] = '\0';
	fclose(in);

	size_t binary_size;
	uint8_t *binary = vm_assemble(text, &binary_size);
	free(text);

	if (!binary) return -1;

	FILE *out = fopen(output_file, "wb");
	if (!out) {
		free(binary);
		return -1;
	}

	fwrite(binary, 1, binary_size, out);
	fclose(out);
	free(binary);
	return 0;
}

int vm_disassemble_file(const char *input_file, const char *output_file) {
	if (!input_file || !output_file) return -1;

	FILE *in = fopen(input_file, "rb");
	if (!in) return -1;

	fseek(in, 0, SEEK_END);
	long size = ftell(in);
	fseek(in, 0, SEEK_SET);

	uint8_t *binary = malloc(size);
	if (!binary) {
		fclose(in);
		return -1;
	}

	fread(binary, 1, size, in);
	fclose(in);

	char *text = vm_disassemble(binary, size);
	free(binary);

	if (!text) return -1;

	FILE *out = fopen(output_file, "w");
	if (!out) {
		free(text);
		return -1;
	}

	fprintf(out, "%s", text);
	fclose(out);
	free(text);
	return 0;
}

/*
 * Debugging
 */

void vm_enable_debug(vm_state_t *vm, FILE *output) {
	if (!vm) return;
	vm->debug = 1;
	vm->debug_output = output ? output : stderr;
}

void vm_disable_debug(vm_state_t *vm) {
	if (!vm) return;
	vm->debug = 0;
}

void vm_print_state(const vm_state_t *vm, FILE *output) {
	if (!vm || !output) return;

	fprintf(output, "VM State:\n");
	fprintf(output, "  PC: 0x%08X\n", vm->pc);
	fprintf(output, "  Flags: 0x%08X [%c%c%c%c]\n", vm->flags,
	        (vm->flags & VM_FLAG_ZERO) ? 'Z' : '-',
	        (vm->flags & VM_FLAG_NEGATIVE) ? 'N' : '-',
	        (vm->flags & VM_FLAG_CARRY) ? 'C' : '-',
	        (vm->flags & VM_FLAG_OVERFLOW) ? 'V' : '-');
	fprintf(output, "  Running: %d\n", vm->running);

	vm_print_registers(vm, output);
}

void vm_print_registers(const vm_state_t *vm, FILE *output) {
	if (!vm || !output) return;

	fprintf(output, "Registers:\n");
	for (int i = 0; i < VM_NUM_REGISTERS; i++) {
		fprintf(output, "  %3s: 0x%08X (%d)\n",
		        vm_register_name(i), vm->regs[i], vm->regs[i]);
	}
}

void vm_print_memory(const vm_state_t *vm, uint32_t start, uint32_t end, FILE *output) {
	if (!vm || !output) return;

	fprintf(output, "Memory [0x%08X - 0x%08X]:\n", start, end);

	for (uint32_t addr = start; addr < end && addr < vm->mem_size; addr += 16) {
		fprintf(output, "%08X: ", addr);

		/* Hex dump */
		for (int i = 0; i < 16 && (addr + i) < end && (addr + i) < vm->mem_size; i++) {
			fprintf(output, "%02X ", vm->memory[addr + i]);
		}

		/* ASCII dump */
		fprintf(output, " |");
		for (int i = 0; i < 16 && (addr + i) < end && (addr + i) < vm->mem_size; i++) {
			uint8_t byte = vm->memory[addr + i];
			fprintf(output, "%c", (byte >= 32 && byte < 127) ? byte : '.');
		}
		fprintf(output, "|\n");
	}
}

void vm_print_disassembly(const vm_state_t *vm, int before, int after, FILE *output) {
	if (!vm || !output) return;

	int32_t start_addr = vm->pc - (before * 8);
	if (start_addr < 0) start_addr = 0;

	int32_t end_addr = vm->pc + (after * 8) + 8;
	if (end_addr > (int32_t)vm->mem_size) end_addr = vm->mem_size;

	fprintf(output, "Disassembly:\n");

	for (int32_t addr = start_addr; addr < end_addr; addr += 8) {
		if (addr + 8 > (int32_t)vm->mem_size) break;

		vm_instruction_t instr;
		instr.opcode = vm->memory[addr];
		instr.rd = vm->memory[addr + 1];
		instr.rs1 = vm->memory[addr + 2];
		instr.rs2 = vm->memory[addr + 3];
		instr.imm = *(int32_t *)(vm->memory + addr + 4);

		char disasm[256];
		vm_disassemble_instruction(&instr, disasm, sizeof(disasm));

		fprintf(output, "%s%08X: %s\n",
		        (addr == (int32_t)vm->pc) ? "=> " : "   ",
		        addr, disasm);
	}
}

void vm_trace(vm_state_t *vm, int enable) {
	if (!vm) return;
	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->trace_enabled = enable;
	if (enable && !vm->debug_output) {
		vm->debug_output = stderr;
	}
}

/*
 * Statistics
 */

void vm_get_stats(const vm_state_t *vm, vm_stats_t *stats) {
	if (!vm || !stats) return;
	const vm_state_internal_t *vmi = vm_to_internal_const(vm);
	*stats = vmi->stats;
}

void vm_reset_stats(vm_state_t *vm) {
	if (!vm) return;
	vm_state_internal_t *vmi = vm_to_internal(vm);
	memset(&vmi->stats, 0, sizeof(vm_stats_t));
}

void vm_print_stats(const vm_state_t *vm, FILE *output) {
	if (!vm || !output) return;

	const vm_state_internal_t *vmi = vm_to_internal_const(vm);
	fprintf(output, "Execution Statistics:\n");
	fprintf(output, "  Instructions: %llu\n", (unsigned long long)vmi->stats.instructions_executed);
	fprintf(output, "  Memory Reads: %llu\n", (unsigned long long)vmi->stats.memory_reads);
	fprintf(output, "  Memory Writes: %llu\n", (unsigned long long)vmi->stats.memory_writes);
	fprintf(output, "  System Calls: %llu\n", (unsigned long long)vmi->stats.syscalls);
}

/*
 * System Calls
 */

void vm_register_syscall_handler(vm_state_t *vm, vm_syscall_handler_t handler) {
	if (!vm) return;
	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->syscall_handler = handler;
}

int32_t vm_default_syscall_handler(vm_state_t *vm, uint32_t syscall_num) {
	if (!vm) return -1;

	switch (syscall_num) {
	case VM_SYSCALL_EXIT:
		vm->running = 0;
		return vm->regs[VM_R0];

	case VM_SYSCALL_GETCHAR:
		return getchar();

	case VM_SYSCALL_PUTCHAR:
		return putchar(vm->regs[VM_R0]);

	case VM_SYSCALL_WRITE: {
		/* r0 = fd, r1 = buffer addr, r2 = count */
		uint32_t addr = vm->regs[VM_R1];
		size_t count = vm->regs[VM_R2];
		if (addr + count > vm->mem_size) return -1;

		if (vm->regs[VM_R0] == 1 || vm->regs[VM_R0] == 2) {
			/* stdout or stderr */
			FILE *f = (vm->regs[VM_R0] == 1) ? stdout : stderr;
			return fwrite(vm->memory + addr, 1, count, f);
		}
		return -1;
	}

	case VM_SYSCALL_READ: {
		/* r0 = fd, r1 = buffer addr, r2 = count */
		uint32_t addr = vm->regs[VM_R1];
		size_t count = vm->regs[VM_R2];
		if (addr + count > vm->mem_size) return -1;

		if (vm->regs[VM_R0] == 0) {
			/* stdin */
			return fread(vm->memory + addr, 1, count, stdin);
		}
		return -1;
	}

	default:
		return -1;
	}
}

/*
 * Error Handling
 */

vm_error_t vm_get_error(const vm_state_t *vm) {
	if (!vm) return VM_OK;
	const vm_state_internal_t *vmi = vm_to_internal_const(vm);
	return vmi->last_error;
}

const char *vm_error_message(vm_error_t error) {
	switch (error) {
	case VM_OK: return "No error";
	case VM_ERROR_INVALID_OPCODE: return "Invalid opcode";
	case VM_ERROR_INVALID_REGISTER: return "Invalid register";
	case VM_ERROR_MEMORY_OUT_OF_BOUNDS: return "Memory out of bounds";
	case VM_ERROR_STACK_OVERFLOW: return "Stack overflow";
	case VM_ERROR_STACK_UNDERFLOW: return "Stack underflow";
	case VM_ERROR_DIVISION_BY_ZERO: return "Division by zero";
	case VM_ERROR_INVALID_INSTRUCTION: return "Invalid instruction";
	case VM_ERROR_ASSEMBLY_ERROR: return "Assembly error";
	case VM_ERROR_FILE_ERROR: return "File error";
	case VM_ERROR_HALT: return "Halted";
	default: return "Unknown error";
	}
}

void vm_set_error_handler(vm_state_t *vm, vm_error_handler_t handler) {
	if (!vm) return;
	vm_state_internal_t *vmi = vm_to_internal(vm);
	vmi->error_handler = handler;
}
