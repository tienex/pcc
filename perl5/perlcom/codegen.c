/*	$Id$	*/
/*
 * Perl 5 Compiler - Code Generation
 *
 * This module generates intermediate representation (IR) code
 * that can be consumed by the PCC middle-end (mip/).
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static FILE *output = NULL;
static int label_counter = 0;

void perl_codegen_init(FILE *fp) {
	output = fp;
	label_counter = 0;
}

int perl_gen_label(void) {
	return ++label_counter;
}

void perl_emit_label(int label) {
	if (output) {
		fprintf(output, "L%d:\n", label);
	}
}

void perl_gen_assign(void *lval, void *rval) {
	/* Generate assignment IR code */
	if (output) {
		fprintf(output, "\t; assign\n");
	}
}

void perl_gen_sub_call(char *name, void *args) {
	/* Generate subroutine call IR */
	if (output) {
		fprintf(output, "\t; call %s\n", name);
	}
}

void perl_gen_if(void *cond, void *then_block, void *else_block) {
	int else_label, end_label;

	else_label = perl_gen_label();
	end_label = perl_gen_label();

	/* Generate condition evaluation */
	if (output) {
		fprintf(output, "\t; if condition\n");
		fprintf(output, "\t; branch if false to L%d\n", else_label);
	}

	/* Then block */
	if (output) {
		fprintf(output, "\t; then block\n");
		fprintf(output, "\t; goto L%d\n", end_label);
	}

	/* Else block */
	perl_emit_label(else_label);
	if (else_block && output) {
		fprintf(output, "\t; else block\n");
	}

	perl_emit_label(end_label);
}

void perl_gen_while(void *cond, void *block) {
	int loop_label, end_label;

	loop_label = perl_gen_label();
	end_label = perl_gen_label();

	perl_emit_label(loop_label);

	if (output) {
		fprintf(output, "\t; while condition\n");
		fprintf(output, "\t; branch if false to L%d\n", end_label);
		fprintf(output, "\t; while body\n");
		fprintf(output, "\t; goto L%d\n", loop_label);
	}

	perl_emit_label(end_label);
}

void perl_gen_for(void *init, void *cond, void *incr, void *block) {
	int loop_label, end_label;

	loop_label = perl_gen_label();
	end_label = perl_gen_label();

	if (output) {
		fprintf(output, "\t; for init\n");
	}

	perl_emit_label(loop_label);

	if (output) {
		fprintf(output, "\t; for condition\n");
		fprintf(output, "\t; branch if false to L%d\n", end_label);
		fprintf(output, "\t; for body\n");
		fprintf(output, "\t; for increment\n");
		fprintf(output, "\t; goto L%d\n", loop_label);
	}

	perl_emit_label(end_label);
}

void perl_gen_foreach(void *var, void *list, void *block) {
	int loop_label, end_label;

	loop_label = perl_gen_label();
	end_label = perl_gen_label();

	if (output) {
		fprintf(output, "\t; foreach setup\n");
	}

	perl_emit_label(loop_label);

	if (output) {
		fprintf(output, "\t; get next item\n");
		fprintf(output, "\t; branch if no more items to L%d\n", end_label);
		fprintf(output, "\t; foreach body\n");
		fprintf(output, "\t; goto L%d\n", loop_label);
	}

	perl_emit_label(end_label);
}

/* Built-in function code generation */

void perl_builtin_print(void *args) {
	if (output) {
		fprintf(output, "\t; builtin print\n");
	}
}

void perl_builtin_push(void *array, void *values) {
	if (output) {
		fprintf(output, "\t; builtin push\n");
	}
}

void perl_builtin_pop(void *array) {
	if (output) {
		fprintf(output, "\t; builtin pop\n");
	}
}

void perl_builtin_shift(void *array) {
	if (output) {
		fprintf(output, "\t; builtin shift\n");
	}
}

void perl_builtin_unshift(void *array, void *values) {
	if (output) {
		fprintf(output, "\t; builtin unshift\n");
	}
}

/* Regular expression code generation */

void *perl_regex_compile(char *pattern, char *flags) {
	if (output) {
		fprintf(output, "\t; compile regex /%s/%s\n", pattern, flags);
	}
	return NULL;
}

void *perl_regex_match(void *string, void *regex) {
	if (output) {
		fprintf(output, "\t; regex match\n");
	}
	return NULL;
}

void *perl_regex_subst(void *string, void *pattern, void *replacement, char *flags) {
	if (output) {
		fprintf(output, "\t; regex substitute\n");
	}
	return NULL;
}
