/*
 * Copyright (c) 2025 PCC Project
 *
 * Itanium C++ ABI Demangler
 *
 * Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#mangling
 *
 * Grammar (simplified):
 *   <mangled-name> ::= _Z <encoding>
 *   <encoding> ::= <function name> <bare-function-type>
 *              ::= <data name>
 *              ::= <special-name>
 *   <name> ::= <nested-name>
 *          ::= <unscoped-name>
 *          ::= <unscoped-template-name> <template-args>
 *          ::= <local-name>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include "demangle.h"

/* Forward declarations */
static int itanium_demangle_encoding(demangle_ctx_t *ctx);
static int itanium_demangle_name(demangle_ctx_t *ctx);
static int itanium_demangle_nested_name(demangle_ctx_t *ctx);
static int itanium_demangle_prefix(demangle_ctx_t *ctx);
static int itanium_demangle_template_prefix(demangle_ctx_t *ctx);
static int itanium_demangle_unqualified_name(demangle_ctx_t *ctx);
static int itanium_demangle_source_name(demangle_ctx_t *ctx);
static int itanium_demangle_operator_name(demangle_ctx_t *ctx);
static int itanium_demangle_special_name(demangle_ctx_t *ctx);
static int itanium_demangle_type(demangle_ctx_t *ctx);
static int itanium_demangle_builtin_type(demangle_ctx_t *ctx);
static int itanium_demangle_function_type(demangle_ctx_t *ctx);
static int itanium_demangle_bare_function_type(demangle_ctx_t *ctx);
static int itanium_demangle_template_args(demangle_ctx_t *ctx);
static int itanium_demangle_template_arg(demangle_ctx_t *ctx);
static int itanium_demangle_expression(demangle_ctx_t *ctx);
static int itanium_demangle_substitution(demangle_ctx_t *ctx);
static int itanium_demangle_call_offset(demangle_ctx_t *ctx);
static int itanium_demangle_cv_qualifiers(demangle_ctx_t *ctx);
static int itanium_demangle_number(demangle_ctx_t *ctx, int *result);
static int itanium_demangle_seq_id(demangle_ctx_t *ctx);

/* Operator name table */
static const struct {
	const char *code;
	const char *name;
} operator_table[] = {
	{"nw", "new"},
	{"na", "new[]"},
	{"dl", "delete"},
	{"da", "delete[]"},
	{"ps", "+"},  /* unary */
	{"ng", "-"},  /* unary */
	{"ad", "&"},  /* unary */
	{"de", "*"},  /* unary */
	{"co", "~"},
	{"pl", "+"},
	{"mi", "-"},
	{"ml", "*"},
	{"dv", "/"},
	{"rm", "%"},
	{"an", "&"},
	{"or", "|"},
	{"eo", "^"},
	{"aS", "="},
	{"pL", "+="},
	{"mI", "-="},
	{"mL", "*="},
	{"dV", "/="},
	{"rM", "%="},
	{"aN", "&="},
	{"oR", "|="},
	{"eO", "^="},
	{"ls", "<<"},
	{"rs", ">>"},
	{"lS", "<<="},
	{"rS", ">>="},
	{"eq", "=="},
	{"ne", "!="},
	{"lt", "<"},
	{"gt", ">"},
	{"le", "<="},
	{"ge", ">="},
	{"ss", "<=>"},  /* C++20 spaceship */
	{"nt", "!"},
	{"aa", "&&"},
	{"oo", "||"},
	{"pp", "++"},   /* postfix */
	{"mm", "--"},   /* postfix */
	{"cm", ","},
	{"pm", "->*"},
	{"pt", "->"},
	{"cl", "()"},
	{"ix", "[]"},
	{"qu", "?"},
	{"st", "sizeof"},
	{"sz", "sizeof..."},
	{"at", "alignof"},
	{"az", "alignof..."},
	{"cv", "cast"},
	{"li", "literal"},
	{NULL, NULL}
};

/*
 * Check if string is Itanium mangled
 */
int
is_itanium_mangled(const char *name)
{
	if (name == NULL)
		return 0;

	/* Itanium mangled names start with _Z */
	if (name[0] == '_' && name[1] == 'Z')
		return 1;

	/* Special names start with _GLOBAL__ */
	if (strncmp(name, "_GLOBAL__", 9) == 0)
		return 1;

	return 0;
}

/*
 * Main Itanium demangling function
 */
char *
demangle_itanium(const char *mangled_name, demangle_options_t opts)
{
	demangle_ctx_t *ctx;
	char *result;

	if (!is_itanium_mangled(mangled_name))
		return strdup(mangled_name);

	ctx = demangle_ctx_create(mangled_name, ABI_ITANIUM);
	if (ctx == NULL)
		return NULL;

	ctx->options = opts;

	/* Skip _Z prefix */
	if (ctx->pos[0] == '_' && ctx->pos[1] == 'Z')
		ctx->pos += 2;

	/* Demangle encoding */
	if (!itanium_demangle_encoding(ctx)) {
		result = strdup(mangled_name);  /* Failed, return original */
	} else {
		result = strdup(ctx->output);
	}

	demangle_ctx_destroy(ctx);
	return result;
}

/*
 * Demangle encoding
 * <encoding> ::= <function name> <bare-function-type>
 *            ::= <data name>
 *            ::= <special-name>
 */
static int
itanium_demangle_encoding(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	/* Special names */
	if (c == 'G' || c == 'T') {
		return itanium_demangle_special_name(ctx);
	}

	/* Name */
	if (!itanium_demangle_name(ctx))
		return 0;

	/* Check for function parameters */
	c = demangle_peek(ctx);
	if (c != '\0' && c != 'E' && (ctx->options & DEMANGLE_OPT_PARAMS)) {
		demangle_append_char(ctx, '(');
		if (!itanium_demangle_bare_function_type(ctx))
			return 0;
		demangle_append_char(ctx, ')');
	}

	return 1;
}

/*
 * Demangle name
 * <name> ::= <nested-name>
 *        ::= <unscoped-name>
 *        ::= <unscoped-template-name> <template-args>
 *        ::= <local-name>
 */
static int
itanium_demangle_name(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	switch (c) {
	case 'N':
		return itanium_demangle_nested_name(ctx);

	case 'Z':
		/* Local name */
		demangle_next(ctx);
		if (!itanium_demangle_encoding(ctx))
			return 0;
		demangle_append(ctx, "::");
		if (!itanium_demangle_name(ctx))
			return 0;
		return 1;

	case 'S':
		/* Substitution or std:: */
		if (ctx->pos[1] == 't') {
			demangle_next(ctx);
			demangle_next(ctx);
			demangle_append(ctx, "std::");
			if (!itanium_demangle_unqualified_name(ctx))
				return 0;
		} else {
			if (!itanium_demangle_substitution(ctx))
				return 0;
		}
		/* Check for template args */
		if (demangle_peek(ctx) == 'I') {
			return itanium_demangle_template_args(ctx);
		}
		return 1;

	default:
		/* Unscoped name */
		if (!itanium_demangle_unqualified_name(ctx))
			return 0;

		/* Check for template args */
		if (demangle_peek(ctx) == 'I') {
			return itanium_demangle_template_args(ctx);
		}
		return 1;
	}
}

/*
 * Demangle nested name
 * <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E
 *               ::= N [<CV-qualifiers>] <template-prefix> <template-args> E
 */
static int
itanium_demangle_nested_name(demangle_ctx_t *ctx)
{
	int has_qualifiers = 0;

	/* Consume 'N' */
	demangle_next(ctx);

	/* CV-qualifiers */
	if (demangle_peek(ctx) == 'r' || demangle_peek(ctx) == 'V' ||
	    demangle_peek(ctx) == 'K') {
		has_qualifiers = 1;
		itanium_demangle_cv_qualifiers(ctx);
	}

	/* Prefix */
	if (!itanium_demangle_prefix(ctx))
		return 0;

	/* Expect 'E' */
	if (demangle_peek(ctx) != 'E') {
		demangle_error(ctx, "expected 'E' in nested name");
		return 0;
	}
	demangle_next(ctx);

	/* Append qualifiers at end if present */
	if (has_qualifiers && (ctx->options & DEMANGLE_OPT_QUALIFIERS)) {
		/* Qualifiers already added */
	}

	return 1;
}

/*
 * Demangle prefix
 * <prefix> ::= <prefix> <unqualified-name>
 *          ::= <template-prefix> <template-args>
 *          ::= <template-param>
 *          ::= <substitution>
 *          ::= # empty
 */
static int
itanium_demangle_prefix(demangle_ctx_t *ctx)
{
	int first = 1;

	while (demangle_peek(ctx) != 'E' && demangle_peek(ctx) != '\0') {
		if (!first)
			demangle_append(ctx, "::");
		first = 0;

		char c = demangle_peek(ctx);

		/* Template args */
		if (c == 'I') {
			if (!itanium_demangle_template_args(ctx))
				return 0;
			continue;
		}

		/* Substitution */
		if (c == 'S') {
			if (!itanium_demangle_substitution(ctx))
				return 0;
			continue;
		}

		/* Unqualified name */
		if (!itanium_demangle_unqualified_name(ctx))
			return 0;
	}

	return 1;
}

/*
 * Demangle unqualified name
 * <unqualified-name> ::= <operator-name>
 *                    ::= <ctor-dtor-name>
 *                    ::= <source-name>
 */
static int
itanium_demangle_unqualified_name(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	/* Constructor/Destructor */
	if (c == 'C' || c == 'D') {
		demangle_next(ctx);
		char type = demangle_next(ctx);  /* C1, C2, C3 or D0, D1, D2 */
		if (c == 'C') {
			demangle_append(ctx, "[constructor]");
		} else {
			demangle_append(ctx, "[destructor]");
		}
		return 1;
	}

	/* Operator */
	if (c >= 'a' && c <= 'z') {
		return itanium_demangle_operator_name(ctx);
	}

	/* Source name (digit) */
	if (c >= '0' && c <= '9') {
		return itanium_demangle_source_name(ctx);
	}

	demangle_error(ctx, "invalid unqualified name");
	return 0;
}

/*
 * Demangle source name
 * <source-name> ::= <positive length number> <identifier>
 */
static int
itanium_demangle_source_name(demangle_ctx_t *ctx)
{
	int len;
	int i;

	if (!itanium_demangle_number(ctx, &len))
		return 0;

	if (len <= 0 || len > 1000) {
		demangle_error(ctx, "invalid source name length");
		return 0;
	}

	for (i = 0; i < len; i++) {
		char c = demangle_next(ctx);
		if (c == '\0') {
			demangle_error(ctx, "unexpected end in source name");
			return 0;
		}
		demangle_append_char(ctx, c);
	}

	return 1;
}

/*
 * Demangle operator name
 */
static int
itanium_demangle_operator_name(demangle_ctx_t *ctx)
{
	char code[3];
	int i;

	code[0] = demangle_next(ctx);
	code[1] = demangle_next(ctx);
	code[2] = '\0';

	/* Look up in operator table */
	for (i = 0; operator_table[i].code != NULL; i++) {
		if (strcmp(operator_table[i].code, code) == 0) {
			demangle_append(ctx, "operator");
			demangle_append(ctx, operator_table[i].name);
			return 1;
		}
	}

	/* Not found */
	demangle_append(ctx, "operator??");
	return 1;
}

/*
 * Demangle special name
 */
static int
itanium_demangle_special_name(demangle_ctx_t *ctx)
{
	char c = demangle_next(ctx);

	switch (c) {
	case 'T':
		/* VTT, vtable, typeinfo, etc. */
		c = demangle_next(ctx);
		switch (c) {
		case 'V':
			demangle_append(ctx, "vtable for ");
			break;
		case 'T':
			demangle_append(ctx, "VTT for ");
			break;
		case 'I':
			demangle_append(ctx, "typeinfo for ");
			break;
		case 'S':
			demangle_append(ctx, "typeinfo name for ");
			break;
		default:
			demangle_append(ctx, "[special ");
			demangle_append_char(ctx, c);
			demangle_append(ctx, "] for ");
			break;
		}
		return itanium_demangle_type(ctx);

	case 'G':
		/* Guard variable */
		c = demangle_next(ctx);
		if (c == 'V') {
			demangle_append(ctx, "guard variable for ");
			return itanium_demangle_name(ctx);
		}
		break;
	}

	demangle_append(ctx, "[special name]");
	return 1;
}

/*
 * Demangle type
 */
static int
itanium_demangle_type(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	/* Qualifiers */
	while (c == 'r' || c == 'V' || c == 'K') {
		demangle_next(ctx);
		/* Qualifiers will be added at end */
		c = demangle_peek(ctx);
	}

	switch (c) {
	case 'v': case 'w': case 'b': case 'c': case 'a': case 'h':
	case 's': case 't': case 'i': case 'j': case 'l': case 'm':
	case 'x': case 'y': case 'n': case 'o': case 'f': case 'd':
	case 'e': case 'g': case 'z':
		return itanium_demangle_builtin_type(ctx);

	case 'P':  /* Pointer */
		demangle_next(ctx);
		if (!itanium_demangle_type(ctx))
			return 0;
		demangle_append_char(ctx, '*');
		return 1;

	case 'R':  /* Reference */
		demangle_next(ctx);
		if (!itanium_demangle_type(ctx))
			return 0;
		demangle_append_char(ctx, '&');
		return 1;

	case 'O':  /* Rvalue reference */
		demangle_next(ctx);
		if (!itanium_demangle_type(ctx))
			return 0;
		demangle_append(ctx, "&&");
		return 1;

	case 'A':  /* Array */
		demangle_next(ctx);
		/* Skip array size for now */
		while (isdigit(demangle_peek(ctx)))
			demangle_next(ctx);
		if (demangle_peek(ctx) == '_')
			demangle_next(ctx);
		if (!itanium_demangle_type(ctx))
			return 0;
		demangle_append(ctx, "[]");
		return 1;

	case 'F':  /* Function type */
		return itanium_demangle_function_type(ctx);

	case 'N':  /* Nested name */
		return itanium_demangle_nested_name(ctx);

	case 'S':  /* Substitution */
		return itanium_demangle_substitution(ctx);

	default:
		/* Try as name */
		return itanium_demangle_name(ctx);
	}
}

/*
 * Demangle builtin type
 */
static int
itanium_demangle_builtin_type(demangle_ctx_t *ctx)
{
	char c = demangle_next(ctx);

	switch (c) {
	case 'v': demangle_append(ctx, "void"); break;
	case 'w': demangle_append(ctx, "wchar_t"); break;
	case 'b': demangle_append(ctx, "bool"); break;
	case 'c': demangle_append(ctx, "char"); break;
	case 'a': demangle_append(ctx, "signed char"); break;
	case 'h': demangle_append(ctx, "unsigned char"); break;
	case 's': demangle_append(ctx, "short"); break;
	case 't': demangle_append(ctx, "unsigned short"); break;
	case 'i': demangle_append(ctx, "int"); break;
	case 'j': demangle_append(ctx, "unsigned int"); break;
	case 'l': demangle_append(ctx, "long"); break;
	case 'm': demangle_append(ctx, "unsigned long"); break;
	case 'x': demangle_append(ctx, "long long"); break;
	case 'y': demangle_append(ctx, "unsigned long long"); break;
	case 'n': demangle_append(ctx, "__int128"); break;
	case 'o': demangle_append(ctx, "unsigned __int128"); break;
	case 'f': demangle_append(ctx, "float"); break;
	case 'd': demangle_append(ctx, "double"); break;
	case 'e': demangle_append(ctx, "long double"); break;
	case 'g': demangle_append(ctx, "__float128"); break;
	case 'z': demangle_append(ctx, "..."); break;
	default:
		demangle_append(ctx, "?");
		break;
	}

	return 1;
}

/*
 * Demangle function type
 */
static int
itanium_demangle_function_type(demangle_ctx_t *ctx)
{
	demangle_next(ctx);  /* Skip 'F' */

	/* Return type (if requested) */
	if (ctx->options & DEMANGLE_OPT_RETURN) {
		if (!itanium_demangle_type(ctx))
			return 0;
		demangle_append(ctx, " ");
	}

	demangle_append_char(ctx, '(');

	/* Parameters */
	return itanium_demangle_bare_function_type(ctx);
}

/*
 * Demangle bare function type (parameters only)
 */
static int
itanium_demangle_bare_function_type(demangle_ctx_t *ctx)
{
	int first = 1;

	while (demangle_peek(ctx) != 'E' && demangle_peek(ctx) != '\0') {
		if (!first)
			demangle_append(ctx, ", ");
		first = 0;

		if (!itanium_demangle_type(ctx))
			return 0;
	}

	if (demangle_peek(ctx) == 'E')
		demangle_next(ctx);

	demangle_append_char(ctx, ')');
	return 1;
}

/*
 * Demangle template args
 */
static int
itanium_demangle_template_args(demangle_ctx_t *ctx)
{
	int first = 1;

	/* Skip 'I' */
	demangle_next(ctx);

	demangle_append_char(ctx, '<');

	while (demangle_peek(ctx) != 'E' && demangle_peek(ctx) != '\0') {
		if (!first)
			demangle_append(ctx, ", ");
		first = 0;

		if (!itanium_demangle_template_arg(ctx))
			return 0;
	}

	if (demangle_peek(ctx) != 'E') {
		demangle_error(ctx, "expected 'E' in template args");
		return 0;
	}
	demangle_next(ctx);

	demangle_append_char(ctx, '>');
	return 1;
}

/*
 * Demangle template arg
 */
static int
itanium_demangle_template_arg(demangle_ctx_t *ctx)
{
	char c = demangle_peek(ctx);

	/* Type argument */
	if (c != 'L' && c != 'X') {
		return itanium_demangle_type(ctx);
	}

	/* Expression argument */
	if (c == 'L') {
		return itanium_demangle_expression(ctx);
	}

	/* Template template argument */
	if (c == 'X') {
		demangle_next(ctx);
		if (!itanium_demangle_expression(ctx))
			return 0;
		if (demangle_peek(ctx) == 'E')
			demangle_next(ctx);
		return 1;
	}

	return 0;
}

/*
 * Demangle expression (simplified)
 */
static int
itanium_demangle_expression(demangle_ctx_t *ctx)
{
	char c = demangle_next(ctx);

	if (c == 'L') {
		/* Literal */
		if (!itanium_demangle_type(ctx))
			return 0;

		/* Value */
		int value;
		if (itanium_demangle_number(ctx, &value)) {
			demangle_append_number(ctx, value);
		}

		if (demangle_peek(ctx) == 'E')
			demangle_next(ctx);

		return 1;
	}

	demangle_append(ctx, "???");
	return 1;
}

/*
 * Demangle substitution
 */
static int
itanium_demangle_substitution(demangle_ctx_t *ctx)
{
	char c;

	/* Skip 'S' */
	demangle_next(ctx);

	c = demangle_peek(ctx);

	/* Standard substitutions */
	if (c == 't') {
		demangle_next(ctx);
		demangle_append(ctx, "std");
		return 1;
	}
	if (c == 'a') {
		demangle_next(ctx);
		demangle_append(ctx, "std::allocator");
		return 1;
	}
	if (c == 'b') {
		demangle_next(ctx);
		demangle_append(ctx, "std::basic_string");
		return 1;
	}
	if (c == 's') {
		demangle_next(ctx);
		demangle_append(ctx, "std::string");
		return 1;
	}
	if (c == 'i') {
		demangle_next(ctx);
		demangle_append(ctx, "std::istream");
		return 1;
	}
	if (c == 'o') {
		demangle_next(ctx);
		demangle_append(ctx, "std::ostream");
		return 1;
	}
	if (c == 'd') {
		demangle_next(ctx);
		demangle_append(ctx, "std::iostream");
		return 1;
	}

	/* Numbered substitution */
	int seq_id = itanium_demangle_seq_id(ctx);
	const char *subst = demangle_get_substitution(ctx, seq_id);
	if (subst) {
		demangle_append(ctx, subst);
		return 1;
	}

	demangle_append(ctx, "S?");
	return 1;
}

/*
 * Demangle CV qualifiers
 */
static int
itanium_demangle_cv_qualifiers(demangle_ctx_t *ctx)
{
	while (1) {
		char c = demangle_peek(ctx);
		if (c == 'r') {
			demangle_next(ctx);
			/* restrict */
		} else if (c == 'V') {
			demangle_next(ctx);
			demangle_append(ctx, " volatile");
		} else if (c == 'K') {
			demangle_next(ctx);
			demangle_append(ctx, " const");
		} else {
			break;
		}
	}
	return 1;
}

/*
 * Parse a number
 */
static int
itanium_demangle_number(demangle_ctx_t *ctx, int *result)
{
	int value = 0;
	int negative = 0;

	if (demangle_peek(ctx) == 'n') {
		negative = 1;
		demangle_next(ctx);
	}

	if (!isdigit(demangle_peek(ctx)))
		return 0;

	while (isdigit(demangle_peek(ctx))) {
		value = value * 10 + (demangle_next(ctx) - '0');
	}

	*result = negative ? -value : value;
	return 1;
}

/*
 * Parse a seq-id (base-36 substitution index)
 */
static int
itanium_demangle_seq_id(demangle_ctx_t *ctx)
{
	int value = 0;
	char c;

	if (demangle_peek(ctx) == '_') {
		demangle_next(ctx);
		return 0;
	}

	while ((c = demangle_peek(ctx)) != '_' && c != '\0') {
		demangle_next(ctx);
		if (c >= '0' && c <= '9') {
			value = value * 36 + (c - '0');
		} else if (c >= 'A' && c <= 'Z') {
			value = value * 36 + (c - 'A' + 10);
		} else {
			break;
		}
	}

	if (demangle_peek(ctx) == '_')
		demangle_next(ctx);

	return value + 1;
}
