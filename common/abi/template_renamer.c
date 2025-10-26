/*
 * Copyright (c) 2025 PCC Project
 *
 * Dynamic Template/Generic Renamer Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "template_renamer.h"

#define MAX_PARAMS 32
#define MAX_DEPTH 16

/*
 * Initialize renamer context
 */
rename_context_t *
rename_init(void)
{
	rename_context_t *ctx = calloc(1, sizeof(rename_context_t));
	if (ctx)
		ctx->max_depth = MAX_DEPTH;
	return ctx;
}

/*
 * Destroy renamer context
 */
void
rename_destroy(rename_context_t *ctx)
{
	rename_rule_t *rule, *next;

	if (!ctx)
		return;

	for (rule = ctx->rules; rule; rule = next) {
		next = rule->next;
		free(rule->pattern);
		free(rule->replacement);
		free(rule);
	}
	free(ctx);
}

/*
 * Add renaming rule
 */
void
rename_add_rule(rename_context_t *ctx, const char *pattern,
                const char *replacement, int recursive)
{
	rename_rule_t *rule;

	if (!ctx || !pattern || !replacement)
		return;

	rule = calloc(1, sizeof(rename_rule_t));
	rule->pattern = strdup(pattern);
	rule->replacement = strdup(replacement);
	rule->recursive = recursive;

	/* Add to end of list */
	if (!ctx->rules) {
		ctx->rules = rule;
	} else {
		rename_rule_t *last = ctx->rules;
		while (last->next)
			last = last->next;
		last->next = rule;
	}
}

/*
 * Find matching closing bracket for template arguments
 */
int
template_find_matching_bracket(const char *str, int start)
{
	int depth = 1;
	int i = start + 1;

	while (str[i] && depth > 0) {
		if (str[i] == '<')
			depth++;
		else if (str[i] == '>')
			depth--;
		i++;
	}

	return (depth == 0) ? i - 1 : -1;
}

/*
 * Parse template arguments into array
 */
char **
template_parse_args(const char *template_str, int *count)
{
	char **args = NULL;
	int argc = 0;
	int max_args = 16;
	const char *start, *p;
	int depth;

	*count = 0;

	/* Find start of template args */
	start = strchr(template_str, '<');
	if (!start)
		return NULL;
	start++;

	args = calloc(max_args, sizeof(char *));
	p = start;
	depth = 0;

	const char *arg_start = p;

	while (*p && !(depth == 0 && *p == '>')) {
		if (*p == '<') {
			depth++;
		} else if (*p == '>') {
			depth--;
		} else if (*p == ',' && depth == 0) {
			/* Found argument separator */
			int len = p - arg_start;
			args[argc] = malloc(len + 1);
			memcpy(args[argc], arg_start, len);
			args[argc][len] = '\0';

			/* Trim whitespace */
			char *s = args[argc];
			while (*s == ' ' || *s == '\t') s++;
			if (s != args[argc])
				memmove(args[argc], s, strlen(s) + 1);

			argc++;
			if (argc >= max_args) {
				max_args *= 2;
				args = realloc(args, max_args * sizeof(char *));
			}

			p++;
			while (*p == ' ' || *p == '\t') p++;
			arg_start = p;
			continue;
		}
		p++;
	}

	/* Last argument */
	if (p > arg_start && depth == 0) {
		int len = p - arg_start;
		args[argc] = malloc(len + 1);
		memcpy(args[argc], arg_start, len);
		args[argc][len] = '\0';

		/* Trim whitespace */
		char *s = args[argc];
		while (*s == ' ' || *s == '\t') s++;
		if (s != args[argc])
			memmove(args[argc], s, strlen(s) + 1);

		argc++;
	}

	*count = argc;
	return args;
}

/*
 * Free parsed template arguments
 */
void
template_free_args(char **args, int count)
{
	int i;
	if (!args)
		return;

	for (i = 0; i < count; i++)
		free(args[i]);
	free(args);
}

/*
 * Match pattern against input and extract parameters
 */
template_param_t *
template_match(const char *pattern, const char *input)
{
	template_param_t *params = NULL;
	template_param_t *last = NULL;
	const char *p = pattern;
	const char *i = input;

	while (*p && *i) {
		if (*p == '?') {
			/* Parameter placeholder */
			p++; /* Skip '?' */
			if (!isdigit(*p))
				goto fail;

			/* Find end of parameter */
			const char *param_start = i;
			const char *param_end;

			/* Find next fixed character in pattern */
			p++;
			while (isdigit(*p)) p++;

			if (*p == ',' || *p == '>' || *p == '\0') {
				/* Parameter extends to delimiter */
				param_end = i;
				while (*param_end && *param_end != ',' && *param_end != '>')
					param_end++;
			} else {
				/* Find next occurrence of pattern char */
				param_end = strchr(i, *p);
				if (!param_end)
					goto fail;
			}

			/* Create parameter */
			template_param_t *param = calloc(1, sizeof(template_param_t));
			int len = param_end - param_start;
			param->value = malloc(len + 1);
			memcpy(param->value, param_start, len);
			param->value[len] = '\0';

			/* Add to list */
			if (!params) {
				params = param;
				last = param;
			} else {
				last->next = param;
				last = param;
			}

			i = param_end;
		} else if (*p == *i) {
			/* Fixed character match */
			p++;
			i++;
		} else {
			/* Mismatch */
			goto fail;
		}
	}

	/* Both should be at end */
	if (*p == '\0' && *i == '\0')
		return params;

fail:
	template_params_destroy(params);
	return NULL;
}

/*
 * Destroy parameter list
 */
void
template_params_destroy(template_param_t *params)
{
	template_param_t *p, *next;

	for (p = params; p; p = next) {
		next = p->next;
		free(p->value);
		free(p);
	}
}

/*
 * Substitute parameters in replacement string
 */
char *
template_substitute(const char *replacement, template_param_t *params)
{
	char result[4096];
	int pos = 0;
	const char *r = replacement;

	result[0] = '\0';

	while (*r) {
		if (*r == '$') {
			/* Parameter substitution */
			r++;
			if (isdigit(*r)) {
				int param_num = *r - '0';
				r++;
				while (isdigit(*r)) {
					param_num = param_num * 10 + (*r - '0');
					r++;
				}

				/* Find parameter */
				template_param_t *p = params;
				int n = 1;
				while (p && n < param_num) {
					p = p->next;
					n++;
				}

				if (p && p->value) {
					int len = strlen(p->value);
					if (pos + len < sizeof(result)) {
						strcpy(result + pos, p->value);
						pos += len;
					}
				}
			}
		} else {
			if (pos + 1 < sizeof(result))
				result[pos++] = *r;
			r++;
		}
	}

	result[pos] = '\0';
	return strdup(result);
}

/*
 * Apply all renaming rules to a name
 */
char *
rename_apply(rename_context_t *ctx, const char *name)
{
	char *result = strdup(name);
	rename_rule_t *rule;
	int changed;
	int depth = 0;

	if (!ctx || !name)
		return result;

	do {
		changed = 0;

		for (rule = ctx->rules; rule; rule = rule->next) {
			template_param_t *params = template_match(rule->pattern, result);

			if (params) {
				char *new_result = template_substitute(rule->replacement, params);
				template_params_destroy(params);

				if (strcmp(result, new_result) != 0) {
					free(result);
					result = new_result;
					changed = 1;

					if (!rule->recursive)
						break;
				} else {
					free(new_result);
				}
			}
		}

		depth++;
	} while (changed && depth < ctx->max_depth);

	return result;
}

/*
 * Predefined transformations
 */

void
rename_std_to_boost(rename_context_t *ctx)
{
	rename_add_rule(ctx, "std::?1", "boost::$1", 1);
	rename_add_rule(ctx, "_ZSt?1", "_ZN5boost$1", 1);
}

void
rename_std_containers_to_custom(rename_context_t *ctx)
{
	rename_add_rule(ctx, "std::vector<?1>", "Array<$1>", 1);
	rename_add_rule(ctx, "std::map<?1,?2>", "Map<$1,$2>", 1);
	rename_add_rule(ctx, "std::string", "String", 0);
}

void
rename_cpp_to_d(rename_context_t *ctx)
{
	rename_add_rule(ctx, "std::vector<?1>", "?1[]", 1);
	rename_add_rule(ctx, "std::string", "string", 0);
	rename_add_rule(ctx, "std::map<?1,?2>", "?2[?1]", 1);
}

void
rename_itanium_to_msvc(rename_context_t *ctx)
{
	/* Basic Itanium to MSVC symbol transformation */
	rename_add_rule(ctx, "_Z?1", "?$1@@", 0);
	rename_add_rule(ctx, "_ZN?1E", "?$1@@", 0);
}

void
rename_add_abi_transform(rename_context_t *ctx,
                          const char *from_abi,
                          const char *to_abi)
{
	/* Generic ABI transformation */
	char pattern[256];
	char replacement[256];

	snprintf(pattern, sizeof(pattern), "%s_?1", from_abi);
	snprintf(replacement, sizeof(replacement), "%s_$1", to_abi);

	rename_add_rule(ctx, pattern, replacement, 1);
}
