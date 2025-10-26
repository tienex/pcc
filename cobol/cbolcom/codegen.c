/*
 * COBOL code generation to intermediate representation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

void
gen_prologue(void)
{
	/* Generate function prologue */
	fprintf(outfile, "\t.text\n");
	fprintf(outfile, "\t.globl\tmain\n");
	fprintf(outfile, "main:\n");
}

void
gen_epilogue(void)
{
	/* Generate function epilogue */
	fprintf(outfile, "\txorl\t%%eax, %%eax\n");
	fprintf(outfile, "\tret\n");
}

NODE *
gen_move(NODE *src, NODE *dst)
{
	/* Generate MOVE statement */
	return buildtree(ASSIGN, dst, src);
}

NODE *
gen_call(const char *name, NODE *args)
{
	NODE *fn = talloc();
	fn->n_op = NAME;
	fn->n_name = xstrdup(name);
	fn->n_type = FTN | INT;

	NODE *call = talloc();
	call->n_op = CALL;
	call->n_left = fn;
	call->n_right = args;
	call->n_type = INT;

	return call;
}

NODE *
gen_if(NODE *cond, NODE *then_part, NODE *else_part)
{
	/* Generate IF statement */
	NODE *p = talloc();
	p->n_op = IF;
	p->n_left = cond;

	if (else_part) {
		NODE *branches = talloc();
		branches->n_op = COLON;
		branches->n_left = then_part;
		branches->n_right = else_part;
		p->n_right = branches;
	} else {
		p->n_right = then_part;
	}

	return p;
}

NODE *
gen_perform(const char *label, NODE *until)
{
	/* Generate PERFORM statement */
	/* This is simplified - actual implementation would be more complex */
	return gen_call(label, NULL);
}

/* OO COBOL support */
struct cobol_class *
define_class(const char *name, const char *inherits)
{
	struct cobol_class *cls = xmalloc(sizeof(*cls));
	memset(cls, 0, sizeof(*cls));

	cls->name = xstrdup(name);
	if (inherits)
		cls->inherits = xstrdup(inherits);

	return cls;
}

struct cobol_method *
define_method(const char *name, int is_static)
{
	struct cobol_method *m = xmalloc(sizeof(*m));
	memset(m, 0, sizeof(*m));

	m->name = xstrdup(name);
	m->is_static = is_static;

	return m;
}

NODE *
gen_invoke(const char *obj, const char *method, NODE *args)
{
	/* Generate method invocation */
	char buf[256];
	snprintf(buf, sizeof(buf), "%s_%s", obj, method);
	return gen_call(buf, args);
}

NODE *
gen_new(const char *classname)
{
	/* Generate object allocation */
	char buf[256];
	snprintf(buf, sizeof(buf), "%s_new", classname);
	return gen_call(buf, NULL);
}
