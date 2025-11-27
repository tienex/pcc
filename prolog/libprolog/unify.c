/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Unification algorithm with occurs check
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "runtime.h"

/* Dereference a term (follow reference chains) */
word_t deref(prolog_engine_t *eng, word_t term) {
	while (IS_REF(term)) {
		word_t *addr = (word_t *)UNTAG(term);
		word_t val = *addr;

		/* If points to itself, it's unbound */
		if (val == term)
			break;

		term = val;
	}

	return term;
}

/* Trail a binding (for backtracking) */
void trail(prolog_engine_t *eng, word_t *addr) {
	/* Only trail heap addresses that are older than last choice point */
	if (!is_heap_addr(eng, addr))
		return;

	if (eng->choice && addr < (word_t *)eng->choice->heap_top)
		return;

	/* Add to trail */
	if (eng->trail_top >= eng->trail_limit) {
		prolog_error(eng, "resource_error", "trail exhausted");
		return;
	}

	*eng->trail_top++ = (word_t)addr;
}

/* Bind variable to value */
static void bind(prolog_engine_t *eng, word_t *var_addr, word_t value) {
	trail(eng, var_addr);
	*var_addr = value;
}

/* Occurs check - check if var occurs in term */
static int occurs_check(prolog_engine_t *eng, word_t *var, word_t term) {
	term = deref(eng, term);

	if (IS_REF(term)) {
		word_t *addr = (word_t *)UNTAG(term);
		return addr == var;
	}

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		for (int i = 0; i < f->arity; i++) {
			if (occurs_check(eng, var, p[i + 1]))
				return 1;
		}
	}

	if (IS_LIST(term)) {
		word_t *p = (word_t *)UNTAG(term);
		if (occurs_check(eng, var, p[0]))
			return 1;
		if (occurs_check(eng, var, p[1]))
			return 1;
	}

	return 0;
}

/* Push term pair onto PDL (push-down list) for unification */
static void pdl_push(prolog_engine_t *eng, word_t t1, word_t t2) {
	if (eng->pdl_top + 2 > eng->pdl_limit) {
		prolog_error(eng, "resource_error", "pdl exhausted");
		return;
	}

	*eng->pdl_top++ = t1;
	*eng->pdl_top++ = t2;
}

/* Pop term pair from PDL */
static int pdl_pop(prolog_engine_t *eng, word_t *t1, word_t *t2) {
	if (eng->pdl_top <= eng->pdl) {
		return 0;  /* Empty */
	}

	*t2 = *--eng->pdl_top;
	*t1 = *--eng->pdl_top;
	return 1;
}

/* Unify two terms */
int unify(prolog_engine_t *eng, word_t term1, word_t term2) {
	word_t *pdl_base = eng->pdl_top;

	eng->unifications++;

	/* Push initial pair */
	pdl_push(eng, term1, term2);

	/* Process PDL */
	while (eng->pdl_top > pdl_base) {
		word_t t1, t2;
		pdl_pop(eng, &t1, &t2);

		/* Dereference */
		t1 = deref(eng, t1);
		t2 = deref(eng, t2);

		/* Already equal? */
		if (t1 == t2)
			continue;

		/* Variable cases */
		if (IS_REF(t1)) {
			word_t *var = (word_t *)UNTAG(t1);

			/* Occurs check if enabled */
			if (eng->occurs_check && occurs_check(eng, var, t2)) {
				eng->pdl_top = pdl_base;  /* Restore PDL */
				return 0;  /* Fail */
			}

			bind(eng, var, t2);
			continue;
		}

		if (IS_REF(t2)) {
			word_t *var = (word_t *)UNTAG(t2);

			/* Occurs check if enabled */
			if (eng->occurs_check && occurs_check(eng, var, t1)) {
				eng->pdl_top = pdl_base;  /* Restore PDL */
				return 0;  /* Fail */
			}

			bind(eng, var, t1);
			continue;
		}

		/* Integer */
		if (IS_INT(t1) && IS_INT(t2)) {
			if (GET_INT(t1) != GET_INT(t2)) {
				eng->pdl_top = pdl_base;
				return 0;  /* Fail */
			}
			continue;
		}

		/* Atom */
		if (IS_ATOM(t1) && IS_ATOM(t2)) {
			atom_entry_t *a1 = (atom_entry_t *)UNTAG(t1);
			atom_entry_t *a2 = (atom_entry_t *)UNTAG(t2);

			if (a1 != a2) {
				eng->pdl_top = pdl_base;
				return 0;  /* Fail */
			}
			continue;
		}

		/* Float */
		if (IS_FLOAT(t1) && IS_FLOAT(t2)) {
			float_box_t *f1 = (float_box_t *)UNTAG(t1);
			float_box_t *f2 = (float_box_t *)UNTAG(t2);

			if (f1->value != f2->value) {
				eng->pdl_top = pdl_base;
				return 0;  /* Fail */
			}
			continue;
		}

		/* Structure */
		if (IS_STRUCT(t1) && IS_STRUCT(t2)) {
			word_t *p1 = (word_t *)UNTAG(t1);
			word_t *p2 = (word_t *)UNTAG(t2);

			functor_t *f1 = (functor_t *)p1[0];
			functor_t *f2 = (functor_t *)p2[0];

			/* Same functor? */
			if (f1->name != f2->name || f1->arity != f2->arity) {
				eng->pdl_top = pdl_base;
				return 0;  /* Fail */
			}

			/* Push arguments */
			for (int i = f1->arity - 1; i >= 0; i--) {
				pdl_push(eng, p1[i + 1], p2[i + 1]);
			}
			continue;
		}

		/* List */
		if (IS_LIST(t1) && IS_LIST(t2)) {
			word_t *p1 = (word_t *)UNTAG(t1);
			word_t *p2 = (word_t *)UNTAG(t2);

			/* Push head and tail */
			pdl_push(eng, p1[0], p2[0]);
			pdl_push(eng, p1[1], p2[1]);
			continue;
		}

		/* String */
		if (IS_STR(t1) && IS_STR(t2)) {
			string_t *s1 = (string_t *)UNTAG(t1);
			string_t *s2 = (string_t *)UNTAG(t2);

			if (s1->length != s2->length ||
			    memcmp(s1->data, s2->data, s1->length) != 0) {
				eng->pdl_top = pdl_base;
				return 0;  /* Fail */
			}
			continue;
		}

		/* Type mismatch */
		eng->pdl_top = pdl_base;
		return 0;  /* Fail */
	}

	/* Success */
	return 1;
}

/* Unwind trail to a mark (for backtracking) */
void unwind_trail(prolog_engine_t *eng, word_t *trail_mark) {
	while (eng->trail_top > trail_mark) {
		word_t *addr = (word_t *)*--eng->trail_top;
		*addr = MAKE_TAG(addr, TAG_REF);  /* Reset to unbound */
	}
}

/* Compare terms (standard order) */
int term_compare(prolog_engine_t *eng, word_t t1, word_t t2) {
	t1 = deref(eng, t1);
	t2 = deref(eng, t2);

	/* Same term */
	if (t1 == t2)
		return 0;

	/* Type order: var < num < atom < string < compound */
	int type1 = TAG(t1);
	int type2 = TAG(t2);

	if (type1 != type2) {
		/* Order by type tag */
		if (type1 < type2) return -1;
		if (type1 > type2) return 1;
	}

	/* Same type, compare values */
	if (IS_INT(t1)) {
		intptr_t v1 = GET_INT(t1);
		intptr_t v2 = GET_INT(t2);
		if (v1 < v2) return -1;
		if (v1 > v2) return 1;
		return 0;
	}

	if (IS_FLOAT(t1)) {
		float_box_t *f1 = (float_box_t *)UNTAG(t1);
		float_box_t *f2 = (float_box_t *)UNTAG(t2);
		if (f1->value < f2->value) return -1;
		if (f1->value > f2->value) return 1;
		return 0;
	}

	if (IS_ATOM(t1)) {
		atom_entry_t *a1 = (atom_entry_t *)UNTAG(t1);
		atom_entry_t *a2 = (atom_entry_t *)UNTAG(t2);
		return strcmp(a1->name, a2->name);
	}

	if (IS_STRUCT(t1)) {
		word_t *p1 = (word_t *)UNTAG(t1);
		word_t *p2 = (word_t *)UNTAG(t2);

		functor_t *f1 = (functor_t *)p1[0];
		functor_t *f2 = (functor_t *)p2[0];

		/* Compare arity first */
		if (f1->arity < f2->arity) return -1;
		if (f1->arity > f2->arity) return 1;

		/* Compare functor name */
		int cmp = strcmp(f1->name->name, f2->name->name);
		if (cmp != 0) return cmp;

		/* Compare arguments */
		for (int i = 0; i < f1->arity; i++) {
			cmp = term_compare(eng, p1[i + 1], p2[i + 1]);
			if (cmp != 0) return cmp;
		}

		return 0;
	}

	if (IS_LIST(t1)) {
		word_t *p1 = (word_t *)UNTAG(t1);
		word_t *p2 = (word_t *)UNTAG(t2);

		/* Compare head */
		int cmp = term_compare(eng, p1[0], p2[0]);
		if (cmp != 0) return cmp;

		/* Compare tail */
		return term_compare(eng, p1[1], p2[1]);
	}

	/* Variables - compare addresses */
	if (IS_REF(t1)) {
		word_t *a1 = (word_t *)UNTAG(t1);
		word_t *a2 = (word_t *)UNTAG(t2);
		if (a1 < a2) return -1;
		if (a1 > a2) return 1;
		return 0;
	}

	return 0;
}

/* Check if term is ground (no variables) */
int term_ground(prolog_engine_t *eng, word_t term) {
	term = deref(eng, term);

	if (IS_REF(term))
		return 0;

	if (IS_STRUCT(term)) {
		word_t *p = (word_t *)UNTAG(term);
		functor_t *f = (functor_t *)p[0];

		for (int i = 0; i < f->arity; i++) {
			if (!term_ground(eng, p[i + 1]))
				return 0;
		}
	}

	if (IS_LIST(term)) {
		word_t *p = (word_t *)UNTAG(term);
		if (!term_ground(eng, p[0]))
			return 0;
		if (!term_ground(eng, p[1]))
			return 0;
	}

	return 1;
}
