/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Code generation module
 * Converts PL/I AST to intermediate representation / assembly
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Label counter */
static int label_count = 0;

/* Temporary variable counter */
static int temp_count = 0;

/* Current output file */
static FILE *output = NULL;

/* Initialize code generator */
void codegen_init(FILE *outfile) {
	output = outfile;
	label_count = 0;
	temp_count = 0;
}

/* Generate new label */
int new_label(void) {
	return ++label_count;
}

/* Generate new temporary */
int new_temp(void) {
	return ++temp_count;
}

/* Emit label */
void emit_label(int label) {
	if (output) {
		fprintf(output, "L%d:\n", label);
	}
}

/* Emit assembly instruction */
void emit(const char *fmt, ...) {
	if (!output) return;

	va_list args;
	va_start(args, fmt);
	fprintf(output, "\t");
	vfprintf(output, fmt, args);
	fprintf(output, "\n");
	va_end(args);
}

/* Emit comment */
void emit_comment(const char *fmt, ...) {
	if (!output) return;

	va_list args;
	va_start(args, fmt);
	fprintf(output, "\t# ");
	vfprintf(output, fmt, args);
	fprintf(output, "\n");
	va_end(args);
}

/* Generate procedure prologue */
void gen_proc_prologue(const char *name) {
	if (!output) return;

	fprintf(output, "\t.text\n");
	fprintf(output, "\t.globl %s\n", name);
	fprintf(output, "\t.type %s, @function\n", name);
	fprintf(output, "%s:\n", name);
	emit("pushq %%rbp");
	emit("movq %%rsp, %%rbp");
	emit("subq $256, %%rsp");  /* Local variable space */
}

/* Generate procedure epilogue */
void gen_proc_epilogue(const char *name) {
	if (!output) return;

	emit_label(label_count + 1000);  /* Return label */
	emit("movq %%rbp, %%rsp");
	emit("popq %%rbp");
	emit("ret");
	fprintf(output, "\t.size %s, .-%s\n", name, name);
}

/* Generate assignment */
void gen_assignment(const char *var, const char *expr) {
	emit_comment("Assignment: %s = %s", var, expr);
	emit("movq %s, %%rax", expr);
	emit("movq %%rax, %s", var);
}

/* Generate function call */
void gen_call(const char *func, int nargs) {
	emit_comment("Call: %s with %d args", func, nargs);

	/* For runtime library functions */
	if (strcmp(func, "pli_put_string") == 0) {
		emit("call pli_put_string@PLT");
	} else if (strcmp(func, "pli_put_fixed") == 0) {
		emit("call pli_put_fixed@PLT");
	} else if (strcmp(func, "pli_put_skip") == 0) {
		emit("call pli_put_skip@PLT");
	} else {
		emit("call %s", func);
	}
}

/* Generate arithmetic operation */
void gen_binop(const char *op, const char *left, const char *right, const char *result) {
	emit_comment("Binary op: %s %s %s", left, op, right);

	if (strcmp(op, "+") == 0) {
		emit("movq %s, %%rax", left);
		emit("addq %s, %%rax", right);
		emit("movq %%rax, %s", result);
	} else if (strcmp(op, "-") == 0) {
		emit("movq %s, %%rax", left);
		emit("subq %s, %%rax", right);
		emit("movq %%rax, %s", result);
	} else if (strcmp(op, "*") == 0) {
		emit("movq %s, %%rax", left);
		emit("imulq %s, %%rax", right);
		emit("movq %%rax, %s", result);
	} else if (strcmp(op, "/") == 0) {
		emit("movq %s, %%rax", left);
		emit("cqto");
		emit("idivq %s", right);
		emit("movq %%rax, %s", result);
	}
}

/* Generate if statement */
void gen_if(const char *cond, int then_label, int else_label) {
	emit_comment("If statement");
	emit("cmpq $0, %s", cond);
	emit("je L%d", else_label);
	emit("jmp L%d", then_label);
}

/* Generate do loop */
void gen_do_loop(const char *var, const char *start, const char *end, int loop_label, int exit_label) {
	emit_comment("DO loop");
	emit("movq %s, %s", start, var);
	emit_label(loop_label);
	emit("movq %s, %%rax", var);
	emit("cmpq %s, %%rax", end);
	emit("jg L%d", exit_label);
}

/* Generate loop increment */
void gen_loop_increment(const char *var, const char *by, int loop_label) {
	emit("movq %s, %%rax", var);
	emit("addq %s, %%rax", by);
	emit("movq %%rax, %s", var);
	emit("jmp L%d", loop_label);
}

/* Generate string constant */
void gen_string_constant(const char *str, int id) {
	if (!output) return;

	fprintf(output, "\t.section .rodata\n");
	fprintf(output, ".LC%d:\n", id);
	fprintf(output, "\t.string \"%s\"\n", str);
	fprintf(output, "\t.text\n");
}

/* Generate data section */
void gen_data_section(void) {
	if (!output) return;
	fprintf(output, "\t.data\n");
}

/* Generate BSS section */
void gen_bss_section(void) {
	if (!output) return;
	fprintf(output, "\t.bss\n");
}

/* Generate global variable */
void gen_global_var(const char *name, int size) {
	if (!output) return;

	fprintf(output, "\t.comm %s,%d,%d\n", name, size, size);
}

/* Generate local variable allocation */
void gen_local_var(const char *name, int offset, int size) {
	emit_comment("Local var: %s at offset %d", name, offset);
}

/* Generate return statement */
void gen_return(const char *expr) {
	if (expr) {
		emit_comment("Return with value");
		emit("movq %s, %%rax", expr);
	} else {
		emit_comment("Return void");
	}
	emit("jmp L%d", label_count + 1000);  /* Jump to epilogue */
}

/* Generate program startup */
void gen_program_start(void) {
	if (!output) return;

	/* Generate .note.GNU-stack section for security */
	fprintf(output, "\t.section .note.GNU-stack,\"\",@progbits\n");
	fprintf(output, "\t.text\n");
}

/* Generate main wrapper for OPTIONS(MAIN) procedures */
void gen_main_wrapper(const char *proc_name) {
	if (!output) return;

	fprintf(output, "\t.globl main\n");
	fprintf(output, "\t.type main, @function\n");
	fprintf(output, "main:\n");
	emit("pushq %%rbp");
	emit("movq %%rsp, %%rbp");
	emit_comment("Initialize PL/I runtime");
	emit("call pli_init@PLT");
	emit_comment("Call main procedure");
	emit("call %s", proc_name);
	emit_comment("Cleanup PL/I runtime");
	emit("call pli_finish@PLT");
	emit("xorq %%rax, %%rax");
	emit("popq %%rbp");
	emit("ret");
	fprintf(output, "\t.size main, .-main\n");
}
