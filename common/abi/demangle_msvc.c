/*
 * Copyright (c) 2025 PCC Project
 *
 * Microsoft Visual C++ ABI Demangler
 *
 * MSVC mangling format:
 *   ? <name> @ <class> @ <modifiers> <type> @Z
 *
 * Examples:
 *   ?foo@@YAXH@Z  ->  void __cdecl foo(int)
 *   ?method@Class@@QAEXH@Z  ->  public: void __thiscall Class::method(int)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "demangle.h"

/* Forward declarations */
static int msvc_demangle_name(demangle_ctx_t *ctx);
static int msvc_demangle_qualname(demangle_ctx_t *ctx);
static int msvc_demangle_type(demangle_ctx_t *ctx);
static int msvc_demangle_function_class(demangle_ctx_t *ctx);
static int msvc_demangle_calling_conv(demangle_ctx_t *ctx);
static int msvc_demangle_return_type(demangle_ctx_t *ctx);
static int msvc_demangle_params(demangle_ctx_t *ctx);
static int msvc_demangle_modifier(demangle_ctx_t *ctx);

/*
 * Check if string is MSVC mangled
 */
int
is_msvc_mangled(const char *name)
{
	if (name == NULL)
		return 0;

	/* MSVC C++ mangled names start with ? */
	if (name[0] == '?')
		return 1;

	/* C names may start with _ or @ */
	if (name[0] == '_' && name[1] != 'Z')
		return 1;  /* Might be C name */

	return 0;
}

/*
 * Main MSVC demangling function
 */
char *
demangle_msvc(const char *mangled_name, demangle_options_t opts)
{
	demangle_ctx_t *ctx;
	char *result;

	if (!is_msvc_mangled(mangled_name))
		return strdup(mangled_name);

	ctx = demangle_ctx_create(mangled_name, ABI_MSVC);
	if (ctx == NULL)
		return NULL;

	ctx->options = opts;

	/* Simple C name with _ prefix */
	if (ctx->pos[0] == '_' && ctx->pos[1] != '?') {
		result = strdup(ctx->pos + 1);  /* Skip leading _ */
		demangle_ctx_destroy(ctx);
		return result;
	}

	/* C++ mangled name */
	if (ctx->pos[0] == '?') {
		demangle_next(ctx);  /* Skip ? */

		/* Check for special names */
		if (ctx->pos[0] == '?') {
			demangle_next(ctx);
			char c = demangle_peek(ctx);
			if (c == '0') {
				demangle_next(ctx);
				demangle_append(ctx, "[constructor] ");
			} else if (c == '1') {
				demangle_next(ctx);
				demangle_append(ctx, "[destructor] ");
			} else if (c == '_') {
				/* Special name like ??_7 (vtable) */
				demangle_next(ctx);
				c = demangle_next(ctx);
				if (c == '7') {
					demangle_append(ctx, "vtable for ");
				} else if (c == 'R') {
					demangle_append(ctx, "RTTI for ");
				}
			}
		}

		/* Demangle qualified name */
		if (!msvc_demangle_qualname(ctx)) {
			result = strdup(mangled_name);
		} else {
			result = strdup(ctx->output);
		}
	} else {
		result = strdup(mangled_name);
	}

	demangle_ctx_destroy(ctx);
	return result;
}

/*
 * Demangle qualified name (name@class@)
 */
static int
msvc_demangle_qualname(demangle_ctx_t *ctx)
{
	int first = 1;

	/* Read name components until @@ */
	while (demangle_peek(ctx) != '@' || ctx->pos[1] != '@') {
		if (demangle_peek(ctx) == '\0')
			break;

		if (demangle_peek(ctx) == '@') {
			demangle_next(ctx);
			if (!first)
				demangle_append(ctx, "::");
			first = 0;
			continue;
		}

		/* Read identifier */
		while (demangle_peek(ctx) != '@' && demangle_peek(ctx) != '\0') {
			demangle_append_char(ctx, demangle_next(ctx));
		}
	}

	/* Skip @@ */
	if (demangle_peek(ctx) == '@' && ctx->pos[1] == '@') {
		demangle_next(ctx);
		demangle_next(ctx);
	}

	/* Function class and modifiers */
	if (demangle_peek(ctx) != '@' && demangle_peek(ctx) != 'Z') {
		if (ctx->options & DEMANGLE_OPT_PARAMS) {
			demangle_append_char(ctx, '(');
			msvc_demangle_params(ctx);
			demangle_append_char(ctx, ')');
		}
	}

	return 1;
}

/*
 * Demangle type
 */
static int
msvc_demangle_type(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	/* Modifiers */
	if (c == 'Q' || c == 'P' || c == 'R') {
		return msvc_demangle_modifier(ctx);
	}

	demangle_next(ctx);

	switch (c) {
	case 'X': demangle_append(ctx, "void"); break;
	case 'D': demangle_append(ctx, "char"); break;
	case 'C': demangle_append(ctx, "signed char"); break;
	case 'E': demangle_append(ctx, "unsigned char"); break;
	case 'F': demangle_append(ctx, "short"); break;
	case 'G': demangle_append(ctx, "unsigned short"); break;
	case 'H': demangle_append(ctx, "int"); break;
	case 'I': demangle_append(ctx, "unsigned int"); break;
	case 'J': demangle_append(ctx, "long"); break;
	case 'K': demangle_append(ctx, "unsigned long"); break;
	case 'M': demangle_append(ctx, "float"); break;
	case 'N': demangle_append(ctx, "double"); break;
	case 'O': demangle_append(ctx, "long double"); break;
	case '_':
		/* Extended types */
		c = demangle_next(ctx);
		if (c == 'J') {
			demangle_append(ctx, "__int64");
		} else if (c == 'K') {
			demangle_append(ctx, "unsigned __int64");
		} else if (c == 'N') {
			demangle_append(ctx, "bool");
		}
		break;
	case 'V':
		/* Class */
		demangle_append(ctx, "class ");
		while (demangle_peek(ctx) != '@' && demangle_peek(ctx) != '\0') {
			demangle_append_char(ctx, demangle_next(ctx));
		}
		if (demangle_peek(ctx) == '@') {
			demangle_next(ctx);
			if (demangle_peek(ctx) == '@')
				demangle_next(ctx);
		}
		break;
	default:
		demangle_append(ctx, "?");
		break;
	}

	return 1;
}

/*
 * Demangle modifier (pointer, reference, etc.)
 */
static int
msvc_demangle_modifier(demangle_ctx_t *ctx)
{
	char c = demangle_next(ctx);

	if (!msvc_demangle_type(ctx))
		return 0;

	switch (c) {
	case 'P':
	case 'Q':
		demangle_append_char(ctx, '*');
		break;
	case 'A':
		demangle_append_char(ctx, '&');
		break;
	case '$':
		if (demangle_peek(ctx) == '$')
			demangle_next(ctx);
		demangle_append(ctx, "&&");
		break;
	}

	return 1;
}

/*
 * Demangle parameters
 */
static int
msvc_demangle_params(demangle_ctx_t *ctx)
{
	int first = 1;

	while (demangle_peek(ctx) != '@' && demangle_peek(ctx) != 'Z' &&
	       demangle_peek(ctx) != '\0') {
		if (!first)
			demangle_append(ctx, ", ");
		first = 0;

		if (!msvc_demangle_type(ctx))
			return 0;
	}

	return 1;
}
