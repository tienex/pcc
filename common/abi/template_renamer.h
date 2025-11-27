/*
 * Copyright (c) 2025 PCC Project
 *
 * Dynamic Template/Generic Renamer
 *
 * Allows transformation of template/generic types with parameter substitution:
 *   Pattern: "ns1::object<?1, int, ?2>"
 *   Replace: "ns2::object<$2, $1>"
 *   Input:   "ns1::object<string, int, float>"
 *   Output:  "ns2::object<float, string>"
 *
 * Supports:
 * - Parameter capture (?1, ?2, ... ?N)
 * - Parameter substitution ($1, $2, ... $N)
 * - Parameter reordering
 * - Fixed type matching
 * - Namespace/prefix replacement
 * - Recursive template matching
 * - Common ABI-specific patterns
 */

#ifndef TEMPLATE_RENAMER_H
#define TEMPLATE_RENAMER_H

#include <stddef.h>

/* Template parameter capture */
typedef struct template_param {
	char *value;                  /* Captured parameter value */
	struct template_param *next;
} template_param_t;

/* Renaming rule */
typedef struct rename_rule {
	char *pattern;                /* Pattern to match (with ?N placeholders) */
	char *replacement;            /* Replacement (with $N substitutions) */
	int recursive;                /* Apply recursively to nested templates */
	struct rename_rule *next;
} rename_rule_t;

/* Renamer context */
typedef struct rename_context {
	rename_rule_t *rules;         /* List of renaming rules */
	int max_depth;                /* Maximum recursion depth */
} rename_context_t;

/*
 * Initialize renamer context
 */
rename_context_t *rename_init(void);
void rename_destroy(rename_context_t *ctx);

/*
 * Add renaming rule
 */
void rename_add_rule(rename_context_t *ctx,
                     const char *pattern,
                     const char *replacement,
                     int recursive);

/*
 * Apply renaming rules to a mangled name
 */
char *rename_apply(rename_context_t *ctx, const char *name);

/*
 * Predefined rule sets for common transformations
 */

/* std:: -> boost:: */
void rename_std_to_boost(rename_context_t *ctx);

/* std::vector<T> -> Array<T> */
void rename_std_containers_to_custom(rename_context_t *ctx);

/* Itanium -> MSVC mangling transformation */
void rename_itanium_to_msvc(rename_context_t *ctx);

/* C++ -> D type mapping */
void rename_cpp_to_d(rename_context_t *ctx);

/* Generic ABI transformation */
void rename_add_abi_transform(rename_context_t *ctx,
                               const char *from_abi,
                               const char *to_abi);

/*
 * Match and extract template parameters
 */
template_param_t *template_match(const char *pattern, const char *input);
void template_params_destroy(template_param_t *params);

/*
 * Substitute template parameters
 */
char *template_substitute(const char *replacement, template_param_t *params);

/*
 * Parse template arguments (handle nested <>, recursion)
 */
char **template_parse_args(const char *template_str, int *count);
void template_free_args(char **args, int count);

/*
 * Utilities
 */

/* Find matching angle bracket */
int template_find_matching_bracket(const char *str, int start);

/* Check if string matches pattern */
int template_pattern_match(const char *pattern, const char *str);

#endif /* TEMPLATE_RENAMER_H */
