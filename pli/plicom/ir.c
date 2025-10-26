/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * PCC IR (NODE) generation for PL/I
 * This replaces direct assembly generation with proper PCC intermediate representation
 */

#include "pass1.h"
#include "../mip/manifest.h"
#include "../mip/pass2.h"
#include <stdlib.h>
#include <string.h>

/* Current function being compiled */
static struct symtab *curftn = NULL;

/* Label counter */
static int labelno = 100;

/* Initialize IR generation */
void ir_init(void) {
	labelno = 100;
	curftn = NULL;
}

/* Create a new label */
int ir_newlabel(void) {
	return ++labelno;
}

/* Build a NODE tree */
static NODE *block(int op, NODE *l, NODE *r, TWORD t, union dimfun *df, struct attr *ap) {
	NODE *p = talloc();

	p->n_op = op;
	p->n_left = l;
	p->n_right = r;
	p->n_type = t;
	p->n_df = df;
	p->n_ap = ap;
	p->n_su = 0;

	return p;
}

/* Map PL/I types to PCC types */
static TWORD pli_to_pcc_type(TNODE *pli_type) {
	if (!pli_type) return INT;

	switch (pli_type->ttype) {
	case TFIXED:
		return INT;  /* FIXED BINARY */
	case TFLOAT:
		return DOUBLE;  /* FLOAT BINARY */
	case TBIT:
	case TCHAR:
		return CHAR;
	case TPOINTER:
		return INCREF(INT);  /* Pointer type */
	case TBYTE:
		return UCHAR;
	case TWORD:
		return USHORT;
	case TDWORD:
		return UNSIGNED;
	case TINTEGER:
		return SHORT;
	case TREAL:
		return FLOAT;
	default:
		return INT;
	}
}

/* Create integer constant node */
NODE *ir_icon(CONSZ val) {
	NODE *p = block(ICON, NIL, NIL, INT, 0, 0);
	setlval(p, val);
	return p;
}

/* Create floating constant node */
NODE *ir_fcon(double val) {
	NODE *p = block(FCON, NIL, NIL, DOUBLE, 0, 0);
	/* Store float value - PCC uses n_dcon for this */
	p->n_dcon = (void *)(long)val;  /* Simplified */
	return p;
}

/* Create name (variable reference) node */
NODE *ir_name(char *name, TWORD type) {
	NODE *p = block(NAME, NIL, NIL, type, 0, 0);
	p->n_name = name;
	return p;
}

/* Create assignment node: var = expr */
NODE *ir_assign(NODE *var, NODE *expr) {
	return block(ASSIGN, var, expr, var->n_type, 0, 0);
}

/* Create binary operation node */
NODE *ir_binop(int op, NODE *left, NODE *right, TWORD type) {
	return block(op, left, right, type, 0, 0);
}

/* Create unary operation node */
NODE *ir_unop(int op, NODE *operand, TWORD type) {
	return block(op, operand, NIL, type, 0, 0);
}

/* Create function call node */
NODE *ir_call(char *funcname, NODE *args, TWORD rettype) {
	NODE *func = block(NAME, NIL, NIL, INCREF(FTN|rettype), 0, 0);
	func->n_name = funcname;

	/* CALL node with function and arguments */
	return block(CALL, func, args, rettype, 0, 0);
}

/* Create argument list (using CM - comma operator) */
NODE *ir_arg(NODE *arg, NODE *next_args) {
	if (next_args == NIL) {
		return arg;
	}
	return block(CM, arg, next_args, INT, 0, 0);
}

/* Create conditional branch */
NODE *ir_cbranch(NODE *cond, int label) {
	NODE *lab = block(ICON, NIL, NIL, INT, 0, 0);
	setlval(lab, label);

	return block(CBRANCH, cond, lab, INT, 0, 0);
}

/* Create goto */
NODE *ir_goto(int label) {
	NODE *p = block(GOTO, NIL, NIL, INT, 0, 0);
	p->n_label = label;
	return p;
}

/* Create return statement */
NODE *ir_return(NODE *expr) {
	return block(RETURN, expr, NIL, INT, 0, 0);
}

/* Emit a label */
void ir_label(int label) {
	send_passt(IP_DEFLABEL, label);
}

/* Start function prologue */
void ir_function_start(struct symtab *sp, int is_main) {
	curftn = sp;

	/* Send function prologue to backend */
	send_passt(IP_PROLOG, sp);

	/* If this is OPTIONS(MAIN), we need special handling */
	if (is_main) {
		/* Generate call to pli_init() */
		NODE *init_call = ir_call("pli_init", NIL, VOID);
		send_passt(IP_NODE, init_call);
	}
}

/* End function epilogue */
void ir_function_end(int is_main) {
	if (is_main) {
		/* Generate call to pli_finish() */
		NODE *finish_call = ir_call("pli_finish", NIL, VOID);
		send_passt(IP_NODE, finish_call);
	}

	/* Send function epilogue */
	send_passt(IP_EPILOG, curftn, 0);

	curftn = NULL;
}

/* Emit a statement (node tree) */
void ir_emit(NODE *p) {
	if (p == NIL) return;
	send_passt(IP_NODE, p);
}

/* Generate PL/I runtime library call */
NODE *ir_pli_call(const char *funcname, NODE *args) {
	return ir_call((char *)funcname, args, INT);
}

/* Helper: Generate PUT SKIP */
NODE *gen_put_skip(void) {
	return ir_pli_call("pli_put_skip", NIL);
}

/* Helper: Generate PUT LIST(string) */
NODE *gen_put_string(NODE *str) {
	return ir_pli_call("pli_put_string", str);
}

/* Helper: Generate PUT LIST(integer) */
NODE *gen_put_fixed(NODE *val) {
	return ir_pli_call("pli_put_fixed", val);
}

/* Helper: Generate PUT LIST(float) */
NODE *gen_put_float(NODE *val) {
	return ir_pli_call("pli_put_float", val);
}

/* Example: Generate DO loop
 * Generates:
 *   var = start;
 * LOOP:
 *   if (var > end) goto EXIT;
 *   ... body ...
 *   var = var + by;
 *   goto LOOP;
 * EXIT:
 */
void gen_do_loop(NODE *var, NODE *start, NODE *end, NODE *by, NODE *body) {
	int loop_label = ir_newlabel();
	int exit_label = ir_newlabel();

	/* var = start */
	ir_emit(ir_assign(var, start));

	/* LOOP: */
	ir_label(loop_label);

	/* if (var > end) goto EXIT */
	NODE *cond = ir_binop(GT, var, end, INT);
	ir_emit(ir_cbranch(cond, exit_label));

	/* Body */
	ir_emit(body);

	/* var = var + by */
	NODE *inc = ir_binop(PLUS, var, by, INT);
	ir_emit(ir_assign(var, inc));

	/* goto LOOP */
	ir_emit(ir_goto(loop_label));

	/* EXIT: */
	ir_label(exit_label);
}

/* PL/I built-in function mapping to runtime calls */
NODE *gen_builtin_call(const char *name, NODE *args) {
	/* Map PL/I built-in names to runtime function names */
	char runtime_name[100];

	if (strcmp(name, "ABS") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_abs_fixed");
	} else if (strcmp(name, "SQRT") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_sqrt");
	} else if (strcmp(name, "SIN") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_sin");
	} else if (strcmp(name, "COS") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_cos");
	} else if (strcmp(name, "EXP") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_exp");
	} else if (strcmp(name, "LOG") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_log");
	} else if (strcmp(name, "INDEX") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_index");
	} else if (strcmp(name, "LENGTH") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_length");
	} else if (strcmp(name, "SUBSTR") == 0) {
		snprintf(runtime_name, sizeof(runtime_name), "pli_substr");
	} else {
		/* Default: try pli_<lowercase name> */
		snprintf(runtime_name, sizeof(runtime_name), "pli_%s", name);
		for (char *p = runtime_name; *p; p++) {
			*p = tolower(*p);
		}
	}

	return ir_pli_call(runtime_name, args);
}
