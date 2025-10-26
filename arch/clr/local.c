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
 * CLR backend - local pass 1 optimizations and symbol handling
 */

#include "pass1.h"

#ifdef LANG_CXX
#define	p1listf	listf
#define	p1tfree tfree
#else
#define	NODE P1ND
#define	talloc p1alloc
#endif

/*
 * Make a symbol external.
 * CLR uses .method or .field with visibility modifiers.
 */
void
defnam(struct symtab *sp)
{
	char *name = getexname(sp);
	int isfunction = ISFTN(sp->stype);

	if (sp->sclass != EXTDEF && sp->sclass != EXTERN)
		return;

	if (isfunction) {
		/* Functions handled by bfcode/efcode in code.c */
	} else {
		/* Global variable */
		defloc(sp);
	}
}

/*
 * Print out a string of characters.
 * For CLR, we need to handle this specially as we'll use .NET strings.
 */
void
instring(struct symtab *sp)
{
	char *s = sp->sname;
	int len = 0;

	/* Count length */
	while (*s++)
		len++;

	/* Emit as a string literal field or constant */
	printf("\t\t\tldstr\t\"");

	s = sp->sname;
	while (*s) {
		if (*s == '\n')
			printf("\\n");
		else if (*s == '\t')
			printf("\\t");
		else if (*s == '\r')
			printf("\\r");
		else if (*s == '\"')
			printf("\\\"");
		else if (*s == '\\')
			printf("\\\\");
		else
			printf("%c", *s);
		s++;
	}
	printf("\"\n");
}

/*
 * Print out a wide string of characters.
 */
void
inwstring(struct symtab *sp)
{
	/* CLR strings are Unicode by default */
	instring(sp);
}

/*
 * Beginning of block (compound statement).
 */
void
bfunc(void)
{
	/* No special handling needed for CLR */
}

/*
 * End of block (compound statement).
 */
void
efunc(void)
{
	/* No special handling needed for CLR */
}

/*
 * Beginning of function arguments.
 */
void
deflab1(int label)
{
	printf(LABFMT ":\n", label);
}

/*
 * Print integer constant.
 */
void
defalign(int al)
{
	/* CLR handles alignment automatically */
	/* No directive needed */
}

/*
 * Set location counter to a specific value.
 */
void
setloc1(int locc)
{
	/* Not applicable for CLR - IL is position-independent */
}

/*
 * Target-specific symbol table processing.
 */
void
fixdef(struct symtab *sp)
{
	/* Perform any necessary symbol table fixups */
	/* e.g., handle CLR-specific attributes */
}

/*
 * Check if a structure can be returned in registers.
 * CLR handles struct return automatically.
 */
int
clr_struct_return(struct symtab *sp)
{
	/* CLR manages struct returns via managed references */
	return 0;  /* Always use standard return mechanism */
}

/*
 * Emit initialization for global/static variables.
 */
void
ninval(CONSZ off, int fsz, NODE *p)
{
	union { float f; int i; } float_conv;
	union { double d; long long l; } double_conv;

	switch (p->n_type) {
	case INT:
	case UNSIGNED:
	case LONG:
	case ULONG:
		printf("\t\t\tldc.i4\t%lld\n", (long long)p->n_lval);
		break;

	case LONGLONG:
	case ULONGLONG:
		printf("\t\t\tldc.i8\t%lld\n", (long long)p->n_lval);
		break;

	case FLOAT:
		float_conv.i = (int)p->n_lval;
		printf("\t\t\tldc.r4\t%f\n", float_conv.f);
		break;

	case DOUBLE:
	case LDOUBLE:
		double_conv.l = (long long)p->n_lval;
		printf("\t\t\tldc.r8\t%f\n", double_conv.d);
		break;

	default:
		printf("\t\t\tldc.i4\t%lld\n", (long long)p->n_lval);
		break;
	}
}

/*
 * Emit beginning of data initialization.
 */
void
beginit(struct symtab *sp)
{
	/* CLR fields can have .data or constructor initialization */
	char *name = getexname(sp);
	printf("\t\t// Initialize %s\n", name);
}

/*
 * Emit end of data initialization.
 */
void
endinit(void)
{
	/* End of initialization block */
}

/*
 * Define a common storage area (tentative definition).
 */
void
defzero(struct symtab *sp)
{
	int size = (int)(tsize(sp->stype, sp->sdf, sp->sap) / SZCHAR);
	char *name = getexname(sp);

	/* Emit field with default initialization (zero) */
	defloc(sp);
}

/*
 * Emit assembler comment.
 */
void
asmline(char *s)
{
	printf("\t\t// %s\n", s);
}

/*
 * Called when a identifier is reference before it's declared.
 * Handle forward references.
 */
void
addstub(struct symtab **spp)
{
	/* CLR handles forward references automatically */
	/* Just ensure the symbol will be resolved later */
}

/*
 * Print out float/double constant as hex (for exact representation).
 */
int
fldal(unsigned int al)
{
	/* CLR uses .NET float representation */
	return al;
}

/*
 * Determine if symbol should use GOT (Global Offset Table).
 * Not applicable for CLR.
 */
int
attr_iscg(struct attr *ap)
{
	return 0;  /* CLR doesn't use GOT */
}

/*
 * Handle inline assembly.
 * CLR supports limited inline IL.
 */
void
pass1_lastchance(struct symtab *sp)
{
	/* Last chance to modify symbol before code generation */
}

/*
 * Emit special CLR directives for a symbol.
 */
void
clr_emit_field_directive(struct symtab *sp)
{
	char *name = getexname(sp);

	/* Check for CLR-specific attributes */
	/* e.g., [ThreadStatic], [MarshalAs], etc. */

	/* Example: Thread-local storage */
	if (sp->sclass == STATIC && (sp->sflags & STLS)) {
		printf("\t.field [threadstatic] static ");
	}
}

/*
 * Emit P/Invoke (Platform Invoke) declaration.
 * Enables calling native C libraries from CLR code.
 */
void
clr_emit_pinvoke_stub(char *libname, char *symbol)
{
	printf("\t.method public static pinvokeimpl(\"%s\" as \"%s\")\n",
	       libname, symbol);
	printf("\t\t// P/Invoke stub for native interop\n");
}

/*
 * Emit managed C++ (/CLI) style delegate.
 */
void
clr_emit_delegate(struct symtab *sp)
{
	char *name = getexname(sp);

	/* Emit delegate type for function pointer */
	printf("\t.class public sealed auto ansi %sDelegate\n", name);
	printf("\t\textends [mscorlib]System.MulticastDelegate\n");
	printf("\t{\n");
	printf("\t\t.method public hidebysig specialname rtspecialname\n");
	printf("\t\t\tinstance void .ctor(object 'object', native int 'method')\n");
	printf("\t\t\truntime managed {}\n");
	printf("\t\t// Invoke method signature here\n");
	printf("\t}\n");
}

/*
 * Emit custom attribute for CLR metadata.
 */
void
clr_emit_attribute(char *attrname, char *args)
{
	printf("\t.custom instance void [mscorlib]System.%s::.ctor(%s)\n",
	       attrname, args);
}

/*
 * Handle reference parameters (ref/out in C#).
 * Enables passing by-ref semantics from C/Fortran.
 */
void
clr_emit_byref_param(struct symtab *sp)
{
	/* Add '&' to type for by-reference parameter */
	printf("&");  /* Will be appended to type name */
}

/*
 * Emit value type (struct) definition.
 * Maps C struct to CLR value type.
 */
void
clr_emit_valuetype(struct symtab *sp)
{
	char *name = getexname(sp);

	printf("\t.class public sequential ansi sealed beforefieldinit\n");
	printf("\t\t%s extends [mscorlib]System.ValueType\n", name);
	printf("\t{\n");

	/* Emit struct fields */
	/* TODO: Iterate through struct members */

	printf("\t}\n");
}

/*
 * Emit reference type (class) definition.
 * Maps C struct (with heap allocation) to CLR class.
 */
void
clr_emit_reftype(struct symtab *sp)
{
	char *name = getexname(sp);

	printf("\t.class public auto ansi beforefieldinit\n");
	printf("\t\t%s extends [mscorlib]System.Object\n", name);
	printf("\t{\n");

	/* Emit class fields and methods */

	printf("\t}\n");
}

/*
 * Target-specific node transformation.
 * Called during tree rewriting in pass 1.
 */
NODE *
clr_funarg(NODE *p)
{
	/* Transform function arguments for CLR calling convention */
	return p;
}

/*
 * Check if function uses varargs.
 */
int
clr_has_varargs(struct symtab *sp)
{
	/* Check if function signature includes __arglist or varargs */
	return 0;  /* TODO: Implement varargs detection */
}

/*
 * Emit varargs handling code.
 * CLR supports __arglist for C-style varargs.
 */
void
clr_emit_varargs_start(void)
{
	printf("\t\t\targlist\n");
}

void
clr_emit_varargs_get(TWORD type)
{
	printf("\t\t\trefanyval\t%s\n", clr_type_name(type, NULL, NULL));
}
