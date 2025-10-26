/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * Code generation header
 */

#ifndef CODEGEN_H
#define CODEGEN_H

#include <stdio.h>
#include <stdarg.h>

/* Initialize code generator */
void codegen_init(FILE *outfile);

/* Label and temporary management */
int new_label(void);
int new_temp(void);
void emit_label(int label);

/* Emission functions */
void emit(const char *fmt, ...);
void emit_comment(const char *fmt, ...);

/* Procedure generation */
void gen_proc_prologue(const char *name);
void gen_proc_epilogue(const char *name);

/* Statement generation */
void gen_assignment(const char *var, const char *expr);
void gen_call(const char *func, int nargs);
void gen_binop(const char *op, const char *left, const char *right, const char *result);
void gen_if(const char *cond, int then_label, int else_label);
void gen_do_loop(const char *var, const char *start, const char *end, int loop_label, int exit_label);
void gen_loop_increment(const char *var, const char *by, int loop_label);
void gen_return(const char *expr);

/* Data generation */
void gen_string_constant(const char *str, int id);
void gen_data_section(void);
void gen_bss_section(void);
void gen_global_var(const char *name, int size);
void gen_local_var(const char *name, int offset, int size);

/* Program structure */
void gen_program_start(void);
void gen_main_wrapper(const char *proc_name);

#endif /* CODEGEN_H */
