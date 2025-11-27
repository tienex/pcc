/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Team.
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
 * Generalized ARC (Automatic Reference Counting) Implementation
 */

#include "pass1.h"
#include "arc.h"
#include <stdlib.h>
#include <string.h>

/* Global ARC configuration */
int arc_enabled = 0;
int arc_weak = 1;
int arc_autoreleasing = 1;

/* Current scope tracking */
static struct arc_scope *current_scope = NULL;

/* ARC runtime function names (Apple runtime by default) */
struct arc_runtime arc_rt = {
	.retain = "objc_retain",
	.release = "objc_release",
	.autorelease = "objc_autorelease",
	.retain_autorelease = "objc_retainAutorelease",
	.store_strong = "objc_storeStrong",
	.store_weak = "objc_storeWeak",
	.load_weak = "objc_loadWeak",
	.destroy_weak = "objc_destroyWeak",
	.move_weak = "objc_moveWeak",
	.copy_weak = "objc_copyWeak",
	.retain_block = "objc_retainBlock",
};

/* ARC statistics */
struct arc_stats arc_statistics = { 0, 0, 0, 0, 0 };

/*
 * Initialize ARC subsystem
 */
void
arc_init(void)
{
	current_scope = NULL;
	arc_reset_stats();
}

/*
 * Set custom ARC runtime (for GNU runtime, etc.)
 */
void
arc_set_runtime(struct arc_runtime *rt)
{
	if (rt)
		arc_rt = *rt;
}

/*
 * Check if a type is an object type that needs ARC management
 */
int
arc_is_object_type(TWORD type)
{
	TWORD bt = BTYPE(type);

	/* Objective-C object types */
	if (bt == OBJC_ID || bt == OBJC_CLASS)
		return 1;

	/* Pointers to Objective-C objects */
	if (ISPTR(type)) {
		TWORD base = DECREF(type);
		bt = BTYPE(base);
		if (bt == OBJC_ID || bt == OBJC_CLASS)
			return 1;
	}

	return 0;
}

/*
 * Get ARC qualifier from attributes
 */
int
arc_get_qualifier(struct attr *ap)
{
	/* TODO: Extract qualifier from attribute chain */
	/* For now, return strong (default) for object types */
	return ARC_QUAL_STRONG;
}

/*
 * Push a new ARC scope
 */
struct arc_scope *
arc_scope_push(int level)
{
	struct arc_scope *scope;

	scope = calloc(1, sizeof(struct arc_scope));
	if (scope == NULL)
		cerror("arc_scope_push: out of memory");

	scope->level = level;
	scope->vars = NULL;
	scope->parent = current_scope;
	current_scope = scope;

	return scope;
}

/*
 * Pop current ARC scope
 */
void
arc_scope_pop(void)
{
	struct arc_scope *scope = current_scope;
	struct arc_var *var, *next;

	if (scope == NULL)
		return;

	/* Free variable list */
	for (var = scope->vars; var != NULL; var = next) {
		next = var->next;
		free(var);
	}

	current_scope = scope->parent;
	free(scope);
}

/*
 * Add a variable to current scope
 */
void
arc_scope_add_var(struct symtab *var, int qualifier)
{
	struct arc_var *av;

	if (current_scope == NULL)
		return;

	av = calloc(1, sizeof(struct arc_var));
	if (av == NULL)
		cerror("arc_scope_add_var: out of memory");

	av->var = var;
	av->qualifier = qualifier;
	av->needs_cleanup = (qualifier == ARC_QUAL_STRONG ||
	                     qualifier == ARC_QUAL_WEAK);
	av->next = current_scope->vars;
	current_scope->vars = av;
}

/*
 * Find a variable in current or parent scopes
 */
struct arc_var *
arc_scope_find_var(struct symtab *var)
{
	struct arc_scope *scope;
	struct arc_var *av;

	for (scope = current_scope; scope != NULL; scope = scope->parent) {
		for (av = scope->vars; av != NULL; av = av->next) {
			if (av->var == var)
				return av;
		}
	}

	return NULL;
}

/*
 * Create a call to an ARC runtime function
 */
static P1ND *
arc_call_runtime(char *funcname, P1ND *arg)
{
	P1ND *fn, *call;

	/* Create function name node */
	fn = bdty(NAME, addname(funcname));

	/* Create function call */
	call = biop(arg ? CALL : UCALL, fn, arg);

	return call;
}

/*
 * Insert a retain operation
 */
P1ND *
arc_insert_retain(P1ND *expr)
{
	if (!arc_enabled)
		return expr;

	if (!arc_is_object_type(expr->n_type))
		return expr;

	arc_statistics.retains++;
	return arc_call_runtime(arc_rt.retain, expr);
}

/*
 * Insert a release operation
 */
P1ND *
arc_insert_release(P1ND *expr)
{
	if (!arc_enabled)
		return expr;

	if (!arc_is_object_type(expr->n_type))
		return expr;

	arc_statistics.releases++;
	return arc_call_runtime(arc_rt.release, expr);
}

/*
 * Insert an autorelease operation
 */
P1ND *
arc_insert_autorelease(P1ND *expr)
{
	if (!arc_enabled)
		return expr;

	if (!arc_is_object_type(expr->n_type))
		return expr;

	arc_statistics.autoreleases++;
	return arc_call_runtime(arc_rt.autorelease, expr);
}

/*
 * Insert a store strong operation
 */
P1ND *
arc_insert_store_strong(P1ND *dest, P1ND *src)
{
	P1ND *args;

	if (!arc_enabled)
		return biop(ASSIGN, dest, src);

	/* Create argument list: (dest_addr, src) */
	args = biop(CM, biop(ADDROF, dest, NULL), src);

	return arc_call_runtime(arc_rt.store_strong, args);
}

/*
 * Insert a store weak operation
 */
P1ND *
arc_insert_store_weak(P1ND *dest, P1ND *src)
{
	P1ND *args;

	if (!arc_enabled || !arc_weak)
		return biop(ASSIGN, dest, src);

	arc_statistics.weak_refs++;

	/* Create argument list: (dest_addr, src) */
	args = biop(CM, biop(ADDROF, dest, NULL), src);

	return arc_call_runtime(arc_rt.store_weak, args);
}

/*
 * Insert a load weak operation
 */
P1ND *
arc_insert_load_weak(P1ND *expr)
{
	P1ND *addr;

	if (!arc_enabled || !arc_weak)
		return expr;

	arc_statistics.weak_refs++;

	/* Take address of weak variable */
	addr = biop(ADDROF, expr, NULL);

	return arc_call_runtime(arc_rt.load_weak, addr);
}

/*
 * Handle assignment with ARC
 */
P1ND *
arc_handle_assign(P1ND *dest, P1ND *src)
{
	struct arc_var *av;
	int dest_qual = ARC_QUAL_STRONG;

	if (!arc_enabled)
		return biop(ASSIGN, dest, src);

	/* Check if destination is an ARC-managed variable */
	if (dest->n_op == NAME && dest->n_sp != NULL) {
		av = arc_scope_find_var(dest->n_sp);
		if (av != NULL)
			dest_qual = av->qualifier;
	}

	/* Handle assignment based on destination qualifier */
	switch (dest_qual) {
	case ARC_QUAL_STRONG:
		return arc_insert_store_strong(dest, src);

	case ARC_QUAL_WEAK:
		return arc_insert_store_weak(dest, src);

	case ARC_QUAL_UNSAFE:
		/* Unsafe unretained - just assign */
		return biop(ASSIGN, dest, src);

	case ARC_QUAL_AUTORELEASING:
		/* Autorelease before assignment */
		src = arc_insert_autorelease(src);
		return biop(ASSIGN, dest, src);

	default:
		return biop(ASSIGN, dest, src);
	}
}

/*
 * Handle variable initialization
 */
P1ND *
arc_handle_init(struct symtab *var, P1ND *init)
{
	struct arc_var *av;

	if (!arc_enabled)
		return init;

	av = arc_scope_find_var(var);
	if (av == NULL)
		return init;

	/* Strong variables retain the initializer */
	if (av->qualifier == ARC_QUAL_STRONG)
		return arc_insert_retain(init);

	return init;
}

/*
 * Handle return statement
 */
P1ND *
arc_handle_return(P1ND *expr)
{
	if (!arc_enabled)
		return expr;

	if (!arc_is_object_type(expr->n_type))
		return expr;

	/* Return values are autoreleased */
	return arc_insert_autorelease(arc_insert_retain(expr));
}

/*
 * Called when a variable is declared
 */
void
arc_var_declared(struct symtab *var)
{
	int qual;

	if (!arc_enabled)
		return;

	if (!arc_is_object_type(var->stype))
		return;

	qual = arc_get_qualifier(var->sap);
	arc_scope_add_var(var, qual);
}

/*
 * Called when a variable is initialized
 */
void
arc_var_initialized(struct symtab *var, P1ND *init)
{
	/* Initialization is handled in arc_handle_init */
}

/*
 * Generate cleanup code for a variable
 */
P1ND *
arc_var_cleanup(struct symtab *var)
{
	struct arc_var *av;
	P1ND *cleanup = NULL;

	if (!arc_enabled)
		return NULL;

	av = arc_scope_find_var(var);
	if (av == NULL || !av->needs_cleanup)
		return NULL;

	/* Create variable reference */
	P1ND *varref = bdty(NAME, var->sname);
	varref->n_sp = var;

	switch (av->qualifier) {
	case ARC_QUAL_STRONG:
		/* Release strong variables */
		cleanup = arc_insert_release(varref);
		break;

	case ARC_QUAL_WEAK:
		/* Destroy weak references */
		if (arc_weak) {
			P1ND *addr = biop(ADDROF, varref, NULL);
			cleanup = arc_call_runtime(arc_rt.destroy_weak, addr);
		}
		break;
	}

	return cleanup;
}

/*
 * Generate cleanup code for current scope
 */
P1ND *
arc_scope_cleanup(void)
{
	struct arc_var *av;
	P1ND *cleanup = NULL, *stmt;

	if (!arc_enabled || current_scope == NULL)
		return NULL;

	/* Generate cleanup for each variable in reverse order */
	for (av = current_scope->vars; av != NULL; av = av->next) {
		if (!av->needs_cleanup)
			continue;

		stmt = arc_var_cleanup(av->var);
		if (stmt != NULL) {
			if (cleanup == NULL)
				cleanup = stmt;
			else
				cleanup = biop(CM, stmt, cleanup);
		}
	}

	return cleanup;
}

/*
 * Check if expression result is already retained
 */
int
arc_expr_is_retained(P1ND *expr)
{
	/* Check for common retained expressions:
	 * - alloc, new, copy, mutableCopy methods
	 * - functions returning retained objects
	 */

	if (expr->n_op == CALL) {
		/* Check function name for retain semantics */
		/* This is simplified - real implementation would check naming conventions */
		return 0;
	}

	return 0;
}

/*
 * Check if expression needs retain
 */
int
arc_expr_needs_retain(P1ND *expr)
{
	if (!arc_enabled)
		return 0;

	if (!arc_is_object_type(expr->n_type))
		return 0;

	/* Variables and retained expressions don't need retain */
	if (expr->n_op == NAME || arc_expr_is_retained(expr))
		return 0;

	return 1;
}

/*
 * Check if expression needs release
 */
int
arc_expr_needs_release(P1ND *expr)
{
	/* This depends on context - typically handled by scope cleanup */
	return 0;
}

/*
 * Optimize retain/release pairs
 */
P1ND *
arc_optimize_retain_release(P1ND *expr)
{
	/* TODO: Implement retain/release elimination
	 * Look for patterns like:
	 * - retain immediately followed by release
	 * - redundant retains/releases
	 */
	return expr;
}

/*
 * Add ARC qualifier to attributes
 */
struct attr *
arc_add_qualifier(struct attr *ap, int qualifier)
{
	/* TODO: Store qualifier in attribute chain */
	return ap;
}

/*
 * Check if two qualifiers are compatible
 */
int
arc_check_compatible(int qual1, int qual2)
{
	/* Strong and weak are incompatible */
	if ((qual1 == ARC_QUAL_STRONG && qual2 == ARC_QUAL_WEAK) ||
	    (qual1 == ARC_QUAL_WEAK && qual2 == ARC_QUAL_STRONG))
		return 0;

	return 1;
}

/*
 * Check assignment compatibility
 */
void
arc_check_assignment(TWORD desttype, struct attr *destap,
                     TWORD srctype, struct attr *srcap)
{
	int dest_qual, src_qual;

	if (!arc_enabled)
		return;

	dest_qual = arc_get_qualifier(destap);
	src_qual = arc_get_qualifier(srcap);

	if (!arc_check_compatible(dest_qual, src_qual))
		werror("incompatible ARC qualifiers in assignment");
}

/*
 * Bridging functions for Core Foundation interop
 */

P1ND *
arc_bridge_retained(P1ND *expr)
{
	/* __bridge_retained: transfer ownership to CF */
	if (arc_enabled)
		return arc_insert_retain(expr);
	return expr;
}

P1ND *
arc_bridge_transfer(P1ND *expr)
{
	/* __bridge_transfer: transfer ownership from CF */
	/* No retain needed - caller already owns it */
	return expr;
}

P1ND *
arc_bridge(P1ND *expr)
{
	/* __bridge: no ownership transfer */
	return expr;
}

/*
 * Function entry/exit
 */

void
arc_function_entry(void)
{
	if (!arc_enabled)
		return;

	/* Push function scope */
	arc_scope_push(0);
}

void
arc_function_exit(void)
{
	if (!arc_enabled)
		return;

	/* Pop function scope */
	arc_scope_pop();
}

P1ND *
arc_function_cleanup(void)
{
	return arc_scope_cleanup();
}

/*
 * Exception support
 */

static int try_depth = 0;

void
arc_try_begin(void)
{
	try_depth++;
}

void
arc_try_end(void)
{
	if (try_depth > 0)
		try_depth--;
}

void
arc_catch_begin(void)
{
	/* Push catch scope */
	if (arc_enabled)
		arc_scope_push(try_depth);
}

void
arc_catch_end(void)
{
	/* Pop catch scope */
	if (arc_enabled)
		arc_scope_pop();
}

P1ND *
arc_exception_cleanup(void)
{
	/* Generate cleanup for exception unwinding */
	return arc_scope_cleanup();
}

/*
 * Diagnostics
 */

void
arc_warn_unsafe_assign(P1ND *dest, P1ND *src)
{
	if (arc_enabled)
		werror("unsafe assignment of object to __unsafe_unretained variable");
}

void
arc_warn_leak(P1ND *expr)
{
	if (arc_enabled)
		werror("potential memory leak");
}

void
arc_warn_double_release(P1ND *expr)
{
	if (arc_enabled)
		werror("potential double release");
}

void
arc_error_retain_cycle(struct symtab *var)
{
	if (arc_enabled)
		uerror("retain cycle detected");
}

/*
 * Optimization
 */

void
arc_optimize_function(void)
{
	/* TODO: Perform function-level ARC optimizations */
}

P1ND *
arc_eliminate_redundant_ops(P1ND *tree)
{
	/* TODO: Eliminate redundant retain/release operations */
	return tree;
}

void
arc_fold_retain_autorelease(void)
{
	/* TODO: Fold retain+autorelease into retainAutorelease */
}

/*
 * Statistics
 */

void
arc_print_stats(void)
{
	if (!arc_enabled)
		return;

	fprintf(stderr, "ARC Statistics:\n");
	fprintf(stderr, "  Retains:        %d\n", arc_statistics.retains);
	fprintf(stderr, "  Releases:       %d\n", arc_statistics.releases);
	fprintf(stderr, "  Autoreleases:   %d\n", arc_statistics.autoreleases);
	fprintf(stderr, "  Optimized away: %d\n", arc_statistics.optimized_away);
	fprintf(stderr, "  Weak refs:      %d\n", arc_statistics.weak_refs);
}

void
arc_reset_stats(void)
{
	memset(&arc_statistics, 0, sizeof(arc_statistics));
}
