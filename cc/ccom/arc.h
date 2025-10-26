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
 * Generalized ARC (Automatic Reference Counting) Support
 *
 * This module provides a front-end agnostic ARC implementation that can be
 * used by Objective-C, Objective-C++, and other languages requiring automatic
 * memory management through reference counting.
 */

#ifndef ARC_H
#define ARC_H

#include "pass1.h"

/*
 * ARC Configuration Flags
 */
extern int arc_enabled;        /* ARC is enabled (-fobjc-arc) */
extern int arc_weak;           /* Weak references supported */
extern int arc_autoreleasing;  /* Autoreleasing supported */

/*
 * ARC Ownership Qualifiers
 * These define how objects are retained/released
 */
#define ARC_QUAL_NONE           0x00    /* No ARC qualifier */
#define ARC_QUAL_STRONG         0x01    /* __strong (default) - retains object */
#define ARC_QUAL_WEAK           0x02    /* __weak - zeroing weak reference */
#define ARC_QUAL_UNSAFE         0x04    /* __unsafe_unretained - no retain */
#define ARC_QUAL_AUTORELEASING  0x08    /* __autoreleasing - autorelease pool */

/*
 * ARC Object Lifecycle Events
 * Track when retain/release operations need to be inserted
 */
typedef enum {
	ARC_RETAIN,           /* Object needs to be retained */
	ARC_RELEASE,          /* Object needs to be released */
	ARC_AUTORELEASE,      /* Object needs to be autoreleased */
	ARC_STRONG_ASSIGN,    /* Assignment to __strong variable */
	ARC_WEAK_ASSIGN,      /* Assignment to __weak variable */
	ARC_LOAD_WEAK,        /* Load from __weak variable */
	ARC_STORE_STRONG,     /* Store to __strong variable */
	ARC_DESTROY,          /* Destroy variable at end of scope */
} ARC_EVENT;

/*
 * ARC Variable Tracking
 * Track variables that need ARC management
 */
struct arc_var {
	struct symtab *var;       /* Variable symbol */
	int qualifier;            /* ARC ownership qualifier */
	int needs_cleanup;        /* Needs cleanup at end of scope */
	struct arc_var *next;     /* Next in list */
};

/*
 * ARC Scope Information
 * Track variables and cleanup points for each scope
 */
struct arc_scope {
	int level;                    /* Scope nesting level */
	struct arc_var *vars;         /* Variables in this scope */
	struct arc_scope *parent;     /* Parent scope */
};

/*
 * ARC Runtime Function Names
 * Configurable for different runtime implementations
 */
struct arc_runtime {
	char *retain;              /* objc_retain */
	char *release;             /* objc_release */
	char *autorelease;         /* objc_autorelease */
	char *retain_autorelease;  /* objc_retainAutorelease */
	char *store_strong;        /* objc_storeStrong */
	char *store_weak;          /* objc_storeWeak */
	char *load_weak;           /* objc_loadWeak */
	char *destroy_weak;        /* objc_destroyWeak */
	char *move_weak;           /* objc_moveWeak */
	char *copy_weak;           /* objc_copyWeak */
	char *retain_block;        /* objc_retainBlock */
};

extern struct arc_runtime arc_rt;

/*
 * ARC Initialization and Configuration
 */
void arc_init(void);
void arc_set_runtime(struct arc_runtime *rt);
int arc_is_object_type(TWORD type);
int arc_get_qualifier(struct attr *ap);

/*
 * ARC Scope Management
 */
struct arc_scope *arc_scope_push(int level);
void arc_scope_pop(void);
void arc_scope_add_var(struct symtab *var, int qualifier);
struct arc_var *arc_scope_find_var(struct symtab *var);

/*
 * ARC Code Generation Helpers
 */
P1ND *arc_insert_retain(P1ND *expr);
P1ND *arc_insert_release(P1ND *expr);
P1ND *arc_insert_autorelease(P1ND *expr);
P1ND *arc_insert_store_strong(P1ND *dest, P1ND *src);
P1ND *arc_insert_store_weak(P1ND *dest, P1ND *src);
P1ND *arc_insert_load_weak(P1ND *expr);

/*
 * ARC Assignment Handling
 */
P1ND *arc_handle_assign(P1ND *dest, P1ND *src);
P1ND *arc_handle_init(struct symtab *var, P1ND *init);
P1ND *arc_handle_return(P1ND *expr);

/*
 * ARC Variable Lifecycle
 */
void arc_var_declared(struct symtab *var);
void arc_var_initialized(struct symtab *var, P1ND *init);
P1ND *arc_var_cleanup(struct symtab *var);
P1ND *arc_scope_cleanup(void);

/*
 * ARC Expression Analysis
 */
int arc_expr_is_retained(P1ND *expr);
int arc_expr_needs_retain(P1ND *expr);
int arc_expr_needs_release(P1ND *expr);
P1ND *arc_optimize_retain_release(P1ND *expr);

/*
 * ARC Attribute Handling
 */
struct attr *arc_add_qualifier(struct attr *ap, int qualifier);
int arc_check_compatible(int qual1, int qual2);
void arc_check_assignment(TWORD desttype, struct attr *destap,
                         TWORD srctype, struct attr *srcap);

/*
 * ARC Bridging (for Core Foundation types)
 */
P1ND *arc_bridge_retained(P1ND *expr);     /* __bridge_retained */
P1ND *arc_bridge_transfer(P1ND *expr);     /* __bridge_transfer */
P1ND *arc_bridge(P1ND *expr);              /* __bridge */

/*
 * ARC Function Analysis
 */
void arc_function_entry(void);
void arc_function_exit(void);
P1ND *arc_function_cleanup(void);

/*
 * ARC Exception Support
 */
void arc_try_begin(void);
void arc_try_end(void);
void arc_catch_begin(void);
void arc_catch_end(void);
P1ND *arc_exception_cleanup(void);

/*
 * ARC Diagnostics
 */
void arc_warn_unsafe_assign(P1ND *dest, P1ND *src);
void arc_warn_leak(P1ND *expr);
void arc_warn_double_release(P1ND *expr);
void arc_error_retain_cycle(struct symtab *var);

/*
 * ARC Optimization
 */
void arc_optimize_function(void);
P1ND *arc_eliminate_redundant_ops(P1ND *tree);
void arc_fold_retain_autorelease(void);

/*
 * ARC Statistics (for debugging/optimization)
 */
struct arc_stats {
	int retains;
	int releases;
	int autoreleases;
	int optimized_away;
	int weak_refs;
};

extern struct arc_stats arc_statistics;
void arc_print_stats(void);
void arc_reset_stats(void);

#endif /* ARC_H */
