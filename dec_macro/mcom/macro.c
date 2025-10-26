/*
 * Copyright (c) 2025 PCC DEC MACRO Compiler
 *
 * Macro definition and expansion
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Macro list */
static MACRO *macros = NULL;

/*
 * Initialize macro system
 */
void
macro_init(void)
{
	macros = NULL;
}

/*
 * Define a new macro
 */
MACRO *
define_macro(const char *name, char **params, int nparam, const char *body)
{
	MACRO *m;
	int i;

	/* Check if macro already exists */
	m = lookup_macro(name);
	if (m != NULL) {
		warning("macro '%s' redefined", name);
		/* Free old definition */
		free(m->mname);
		for (i = 0; i < m->nparam; i++)
			free(m->mparams[i]);
		free(m->mparams);
		free(m->mbody);
	} else {
		/* Create new macro */
		m = (MACRO *)malloc(sizeof(MACRO));
		if (m == NULL)
			fatal("out of memory");

		/* Link into list */
		m->mnext = macros;
		macros = m;
	}

	/* Fill in macro definition */
	m->mname = strdup(name);
	if (m->mname == NULL)
		fatal("out of memory");

	m->nparam = nparam;
	if (nparam > 0) {
		m->mparams = (char **)malloc(nparam * sizeof(char *));
		if (m->mparams == NULL)
			fatal("out of memory");
		for (i = 0; i < nparam; i++) {
			m->mparams[i] = strdup(params[i]);
			if (m->mparams[i] == NULL)
				fatal("out of memory");
		}
	} else {
		m->mparams = NULL;
	}

	m->mbody = strdup(body);
	if (m->mbody == NULL)
		fatal("out of memory");

	return m;
}

/*
 * Look up a macro by name
 */
MACRO *
lookup_macro(const char *name)
{
	MACRO *m;

	for (m = macros; m != NULL; m = m->mnext) {
		if (strcmp(m->mname, name) == 0)
			return m;
	}
	return NULL;
}

/*
 * Expand a macro with arguments
 * Returns newly allocated string with expanded text
 */
char *
expand_macro(MACRO *m, char **args, int nargs)
{
	char *result;
	char *p, *q;
	int i, len;
	char param_name[256];

	/* Check argument count */
	if (nargs != m->nparam) {
		error("macro '%s' expects %d arguments, got %d",
		      m->mname, m->nparam, nargs);
		return strdup("");
	}

	/* Estimate result size (body + args) */
	len = strlen(m->mbody);
	for (i = 0; i < nargs; i++)
		len += strlen(args[i]);
	len += 1024; /* Extra space for expansion */

	result = (char *)malloc(len);
	if (result == NULL)
		fatal("out of memory");

	/* Expand macro body */
	q = result;
	p = m->mbody;
	while (*p) {
		if (*p == '\\' && *(p+1) >= '0' && *(p+1) <= '9') {
			/* Positional parameter \1, \2, etc. */
			int pnum = *(p+1) - '0';
			p += 2;
			if (pnum > 0 && pnum <= nargs) {
				strcpy(q, args[pnum-1]);
				q += strlen(args[pnum-1]);
			}
		} else if (*p == '\\' && *(p+1) == '<') {
			/* Named parameter \<param> */
			p += 2;
			i = 0;
			while (*p && *p != '>' && i < 255) {
				param_name[i++] = *p++;
			}
			param_name[i] = '\0';
			if (*p == '>')
				p++;

			/* Find matching parameter */
			for (i = 0; i < m->nparam; i++) {
				if (strcmp(m->mparams[i], param_name) == 0) {
					strcpy(q, args[i]);
					q += strlen(args[i]);
					break;
				}
			}
			if (i >= m->nparam) {
				error("undefined macro parameter '%s'", param_name);
			}
		} else {
			*q++ = *p++;
		}
	}
	*q = '\0';

	return result;
}
