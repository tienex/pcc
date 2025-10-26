/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * PCC IR generation interface
 */

#ifndef IR_H
#define IR_H

#include "../mip/manifest.h"

/* Initialize IR generation */
void ir_init(void);

/* Label management */
int ir_newlabel(void);
void ir_label(int label);

/* Node creation */
NODE *ir_icon(CONSZ val);
NODE *ir_fcon(double val);
NODE *ir_name(char *name, TWORD type);
NODE *ir_assign(NODE *var, NODE *expr);
NODE *ir_binop(int op, NODE *left, NODE *right, TWORD type);
NODE *ir_unop(int op, NODE *operand, TWORD type);
NODE *ir_call(char *funcname, NODE *args, TWORD rettype);
NODE *ir_arg(NODE *arg, NODE *next_args);
NODE *ir_cbranch(NODE *cond, int label);
NODE *ir_goto(int label);
NODE *ir_return(NODE *expr);

/* Function management */
void ir_function_start(struct symtab *sp, int is_main);
void ir_function_end(int is_main);

/* Emit nodes to backend */
void ir_emit(NODE *p);

/* PL/I runtime library helpers */
NODE *ir_pli_call(const char *funcname, NODE *args);
NODE *gen_put_skip(void);
NODE *gen_put_string(NODE *str);
NODE *gen_put_fixed(NODE *val);
NODE *gen_put_float(NODE *val);

/* Control flow generation */
void gen_do_loop(NODE *var, NODE *start, NODE *end, NODE *by, NODE *body);

/* Built-in function generation */
NODE *gen_builtin_call(const char *name, NODE *args);

#endif /* IR_H */
