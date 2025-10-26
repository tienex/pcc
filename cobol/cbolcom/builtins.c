/*
 * COBOL built-in functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

NODE *
gen_builtin_accept(NODE *var)
{
	/* Generate code for ACCEPT statement */
	/* This would call a runtime library function */
	return gen_call("__cobol_accept", var);
}

NODE *
gen_builtin_display(NODE *args)
{
	/* Generate code for DISPLAY statement */
	/* This would call a runtime library function */
	return gen_call("__cobol_display", args);
}

NODE *
gen_builtin_compute(NODE *result, NODE *expr)
{
	/* Generate code for COMPUTE statement */
	return gen_move(expr, result);
}
