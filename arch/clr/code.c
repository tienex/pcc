/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC CLR Backend Contributors.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * CLR (Common Language Runtime) backend - code generation
 * Generates ILASM (IL Assembly) code for the Microsoft .NET Framework
 */

#include "pass1.h"

#ifdef LANG_CXX
#define	p1listf	listf
#define	p1tfree tfree
#else
#define	NODE P1ND
#define	talloc p1alloc
#endif

static int max_stack_depth = 0;		/* Track max evaluation stack depth */
static int current_stack_depth = 0;	/* Current evaluation stack depth */
static int local_count = 0;		/* Number of local variables */

/*
 * Print out CLR segment/section name.
 * CLR doesn't use traditional segments, but we map them to IL constructs.
 */
void
setseg(int seg, char *name)
{
	/* CLR doesn't use explicit segments like assembly */
	/* Everything is managed by .class, .method, .field directives */

	switch (seg) {
	case PROG:	/* Code section - handled by .method */
		break;
	case DATA:	/* Data section - handled by .field */
	case LDATA:
		break;
	case UDATA:	/* Uninitialized data */
		break;
	case RDATA:	/* Read-only data */
		break;
	case STRNG:	/* String constants */
		break;
	default:
		break;
	}
	/* No output needed - CLR IL is self-describing */
}

/*
 * Define location and attributes for data/function.
 * In CLR, this translates to method/field visibility and attributes.
 */
void
defloc(struct symtab *sp)
{
	char *name = getexname(sp);

	if (ISFTN(sp->stype)) {
		/* Functions are handled by bfcode/efcode */
		return;
	}

	/* Global variable/field */
	if (sp->sclass == EXTDEF || sp->sclass == EXTERN) {
		printf("\t.field public static ");
		/* Print type */
		if (sp->stype == INT || sp->stype == UNSIGNED)
			printf("int32");
		else if (sp->stype == LONG || sp->stype == ULONG)
			printf("int32");
		else if (sp->stype == LONGLONG || sp->stype == ULONGLONG)
			printf("int64");
		else if (sp->stype == SHORT || sp->stype == USHORT)
			printf("int16");
		else if (sp->stype == CHAR)
			printf("int8");
		else if (sp->stype == UCHAR)
			printf("unsigned int8");
		else if (sp->stype == FLOAT)
			printf("float32");
		else if (sp->stype == DOUBLE || sp->stype == LDOUBLE)
			printf("float64");
		else if (ISPTR(sp->stype))
			printf("native int");
		else
			printf("int32");

		printf(" %s\n", name);
	}
}

/*
 * Beginning of job - called once at start of compilation unit.
 * Emit CLR assembly header and namespace declaration.
 */
void
bjobcode(void)
{
	/* Emit CLR assembly metadata */
	printf("// PCC CLR Backend - Generated IL Assembly\n");
	printf("// Target: Microsoft .NET Framework / Mono\n\n");

	/* Reference to mscorlib (standard library) */
	printf(".assembly extern mscorlib\n");
	printf("{\n");
	printf("\t.publickeytoken = (B7 7A 5C 56 19 34 E0 89)\n");
	printf("\t.ver 4:0:0:0\n");
	printf("}\n\n");

	/* Define our assembly */
	printf(".assembly PCCOutput\n");
	printf("{\n");
	printf("\t.ver 1:0:0:0\n");
	printf("}\n\n");

	/* Define module */
	printf(".module PCCOutput.exe\n\n");

	/* Begin namespace */
	printf(".namespace PCC\n");
	printf("{\n");

	/* Define static class to hold all generated functions */
	printf("\t.class public abstract sealed auto ansi beforefieldinit\n");
	printf("\t       GeneratedCode extends [mscorlib]System.Object\n");
	printf("\t{\n");
}

/*
 * End of job - called once at end of compilation unit.
 * Close namespace and class definitions.
 */
void
ejobcode(int flag)
{
	/* Close class */
	printf("\t} // end of class GeneratedCode\n");

	/* Close namespace */
	printf("} // end of namespace PCC\n");
}

/*
 * Convert C type to CLR type string
 */
static char *
clr_type_name(TWORD type, union dimfun *df, struct attr *ap)
{
	static char buf[256];

	switch (type) {
	case VOID:
		return "void";
	case CHAR:
		return "int8";
	case UCHAR:
	case BOOL:
		return "unsigned int8";
	case SHORT:
		return "int16";
	case USHORT:
		return "unsigned int16";
	case INT:
		return "int32";
	case UNSIGNED:
		return "unsigned int32";
	case LONG:
		return "int32";
	case ULONG:
		return "unsigned int32";
	case LONGLONG:
		return "int64";
	case ULONGLONG:
		return "unsigned int64";
	case FLOAT:
		return "float32";
	case DOUBLE:
	case LDOUBLE:
		return "float64";
	default:
		if (ISPTR(type)) {
			/* Pointers are native int or object references */
			TWORD base = DECREF(type);
			if (base == VOID)
				return "native int";
			else if (base == CHAR || base == UCHAR)
				return "string";	/* char* -> string */
			else
				return "native int";
		}
		return "int32";
	}
}

/*
 * Calculate stack depth for arguments
 */
static int
calc_arg_stack(struct symtab **sp, int cnt)
{
	int depth = 0;
	int i;

	for (i = 0; i < cnt; i++) {
		if (sp[i]->stype == LONGLONG || sp[i]->stype == ULONGLONG ||
		    sp[i]->stype == DOUBLE || sp[i]->stype == LDOUBLE)
			depth += 2;
		else
			depth += 1;
	}
	return depth;
}

/*
 * Beginning of function code.
 * sp is array of parameter symbols, cnt is parameter count.
 * sp[cnt] points to the function symbol itself.
 */
void
bfcode(struct symtab **sp, int cnt)
{
	struct symtab *fsym = cftnsp;	/* Current function symbol */
	TWORD rettype = DECREF(fsym->stype);
	char *funcname = getexname(fsym);
	int i;

	/* Reset counters for this function */
	max_stack_depth = 8;	/* Conservative default */
	current_stack_depth = 0;
	local_count = 0;

	/* Emit method header */
	printf("\n\t\t.method public static ");

	/* Return type */
	printf("%s ", clr_type_name(rettype, fsym->sdf, fsym->sap));

	/* Function name - special handling for 'main' */
	if (strcmp(funcname, "main") == 0) {
		printf("Main");
	} else {
		printf("%s", funcname);
	}

	printf("(");

	/* Parameters */
	for (i = 0; i < cnt; i++) {
		if (i > 0)
			printf(", ");
		printf("%s %s",
		       clr_type_name(sp[i]->stype, sp[i]->sdf, sp[i]->sap),
		       getexname(sp[i]));
	}

	printf(") cil managed\n");
	printf("\t\t{\n");

	/* Mark as entry point if this is 'main' */
	if (strcmp(funcname, "main") == 0) {
		printf("\t\t\t.entrypoint\n");
	}

	/* Emit .maxstack directive (will be updated if needed) */
	printf("\t\t\t.maxstack %d\n", max_stack_depth);

	/* We'll emit .locals init later if needed, after we know local count */
	printf("\t\t\t// .locals init will be inserted here if needed\n");
}

/*
 * End of function code.
 * Emit return instruction if not already present.
 */
void
efcode(void)
{
	/* Ensure function ends with a return */
	TWORD rettype = DECREF(cftnsp->stype);

	if (rettype == VOID) {
		/* Void function - just ret */
		printf("\t\t\tret\n");
	}

	/* Close method */
	printf("\t\t} // end of method %s\n", getexname(cftnsp));
}

/*
 * Called for each function during parsing (pass 1).
 * Can be used to set up function-specific data structures.
 */
void
funcode(NODE *p)
{
	/* Optional: handle function attributes, calling conventions, etc. */
}

/*
 * Allocate a temporary local variable.
 * Returns the local variable index.
 */
int
clr_alloc_local(TWORD type)
{
	int idx = local_count++;
	/* TODO: Track local types for .locals init directive */
	return idx;
}

/*
 * Update maximum stack depth tracking.
 * Called by code generator when stack depth changes.
 */
void
clr_update_stack_depth(int depth)
{
	current_stack_depth = depth;
	if (depth > max_stack_depth)
		max_stack_depth = depth;
}

/*
 * Emit local variable declarations.
 * Called before emitting function body.
 */
void
clr_emit_locals(void)
{
	if (local_count > 0) {
		printf("\t\t\t.locals init (\n");
		/* TODO: Emit actual local variable types */
		/* For now, just reserve slots */
		printf("\t\t\t\t// %d local variables\n", local_count);
		printf("\t\t\t)\n");
	}
}

/*
 * Return offset of argument on stack.
 * CLR uses zero-based argument indexing.
 */
int
argoff(int argnum)
{
	return argnum * SZINT;
}

/*
 * Emit prolog code for special cases (e.g., alloca, dynamic arrays)
 */
void
clr_prolog_special(void)
{
	/* Handle special prolog requirements */
	/* e.g., pinning for P/Invoke, unsafe context, etc. */
}

/*
 * Emit epilog code for special cases
 */
void
clr_epilog_special(void)
{
	/* Handle special epilog requirements */
	/* e.g., unpinning, exception handling cleanup */
}

/*
 * Check if type needs boxing when used as object
 */
int
clr_needs_boxing(TWORD type)
{
	/* Value types need boxing when converted to object */
	switch (type) {
	case CHAR:
	case UCHAR:
	case SHORT:
	case USHORT:
	case INT:
	case UNSIGNED:
	case LONG:
	case ULONG:
	case LONGLONG:
	case ULONGLONG:
	case FLOAT:
	case DOUBLE:
	case LDOUBLE:
	case BOOL:
		return 1;
	default:
		return 0;
	}
}

/*
 * Emit box instruction for value type
 */
void
clr_emit_box(TWORD type)
{
	printf("\t\t\tbox\t%s\n", clr_type_name(type, NULL, NULL));
}

/*
 * Emit unbox instruction for value type
 */
void
clr_emit_unbox(TWORD type)
{
	printf("\t\t\tunbox.any\t%s\n", clr_type_name(type, NULL, NULL));
}

/*
 * Emit P/Invoke declaration for native interop
 * This enables C++/CLI-style mixed mode programming
 */
void
clr_emit_pinvoke(char *dllname, char *funcname, char *signature)
{
	printf("\t\t.method public static pinvokeimpl(\"%s\")\n", dllname);
	printf("\t\t\t%s\n", signature);
	printf("\t\t{\n");
	printf("\t\t}\n");
}

/*
 * Emit managed/unmanaged transition code
 * Inspired by C++/CLI's mixed mode capabilities
 */
void
clr_emit_transition(int to_managed)
{
	if (to_managed) {
		/* Transition from unmanaged to managed */
		/* May need to update security stack, etc. */
	} else {
		/* Transition from managed to unmanaged */
		/* May need to suppress GC transitions */
	}
}

/*
 * Emit .NET Framework runtime call
 * Enables accessing framework features from C/Fortran
 */
void
clr_emit_runtime_call(char *namespace, char *classname,
                      char *method, char *signature)
{
	printf("\t\t\tcall\t%s [mscorlib]%s.%s::%s\n",
	       signature, namespace, classname, method);
}

/*
 * Generate code for garbage collector interaction
 * Allows manual GC control in C/Fortran code
 */
void
clr_emit_gc_call(char *method)
{
	if (strcmp(method, "collect") == 0) {
		printf("\t\t\tcall\tvoid [mscorlib]System.GC::Collect()\n");
	} else if (strcmp(method, "suppress") == 0) {
		printf("\t\t\tcall\tvoid [mscorlib]System.GC::SuppressFinalize(object)\n");
	}
}

/*
 * Emit exception handling constructs
 * Maps C/Fortran error handling to .NET exceptions
 */
void
clr_emit_try_begin(void)
{
	printf("\t\t\t.try\n");
	printf("\t\t\t{\n");
}

void
clr_emit_try_end(void)
{
	printf("\t\t\t}\n");
}

void
clr_emit_catch(char *exception_type)
{
	printf("\t\t\tcatch [mscorlib]System.%s\n", exception_type);
	printf("\t\t\t{\n");
}

void
clr_emit_finally(void)
{
	printf("\t\t\tfinally\n");
	printf("\t\t\t{\n");
}

void
clr_emit_endcatch(void)
{
	printf("\t\t\t}\n");
}

/*
 * Emit array creation and access
 * Provides access to .NET array features
 */
void
clr_emit_newarr(TWORD elemtype, int size)
{
	printf("\t\t\tldc.i4\t%d\n", size);
	printf("\t\t\tnewarr\t%s\n", clr_type_name(elemtype, NULL, NULL));
}

void
clr_emit_ldelem(TWORD elemtype)
{
	if (elemtype == INT || elemtype == UNSIGNED)
		printf("\t\t\tldelem.i4\n");
	else if (elemtype == LONGLONG || elemtype == ULONGLONG)
		printf("\t\t\tldelem.i8\n");
	else if (elemtype == FLOAT)
		printf("\t\t\tldelem.r4\n");
	else if (elemtype == DOUBLE || elemtype == LDOUBLE)
		printf("\t\t\tldelem.r8\n");
	else
		printf("\t\t\tldelem\t%s\n", clr_type_name(elemtype, NULL, NULL));
}

void
clr_emit_stelem(TWORD elemtype)
{
	if (elemtype == INT || elemtype == UNSIGNED)
		printf("\t\t\tstelem.i4\n");
	else if (elemtype == LONGLONG || elemtype == ULONGLONG)
		printf("\t\t\tstelem.i8\n");
	else if (elemtype == FLOAT)
		printf("\t\t\tstelem.r4\n");
	else if (elemtype == DOUBLE || elemtype == LDOUBLE)
		printf("\t\t\tstelem.r8\n");
	else
		printf("\t\t\tstelem\t%s\n", clr_type_name(elemtype, NULL, NULL));
}
