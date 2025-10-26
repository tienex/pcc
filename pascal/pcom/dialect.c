/*
 * Copyright (c) 2025 PCC Pascal Compiler
 *
 * Pascal dialect configuration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dialect.h"

/* Current dialect */
pascal_dialect_t current_dialect = DIALECT_ISO;
dialect_features_t *dialect_features = NULL;

/* ISO 7185 Standard Pascal (strict) */
dialect_features_t iso_features = {
	.allow_nested_comments = 0,
	.allow_cpp_comments = 0,
	.allow_dollar_ident = 0,
	.allow_underscores = 0,
	.allow_inline_assembly = 0,
	.allow_goto = 1,  /* ISO allows goto */

	.allow_cstring_literals = 0,
	.allow_string_concat = 0,
	.allow_escape_sequences = 0,
	.pstring_literals = 1,

	.allow_unsigned_types = 0,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 0,
	.allow_set_operators = 1,  /* Sets are ISO feature */
	.allow_string_type = 0,

	.allow_objects = 0,
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 0,
	.allow_forward_directive = 1,  /* ISO allows forward */
	.allow_external_directive = 0,
	.allow_interrupt_directive = 0,
	.allow_varargs = 0,

	.allow_units = 0,
	.allow_namespaces = 0,
	.allow_uses_clause = 0,

	.allow_break_continue = 0,
	.allow_exit_function = 0,
	.allow_case_else = 0,

	.allow_absolute = 0,
	.allow_packed = 1,  /* ISO allows packed */
	.allow_far_near = 0,
	.allow_interrupt = 0,
	.allow_assembler = 0,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 1,
	.require_program_header = 1,
	.require_forward_decl = 1,

	.default_int_size = 16,
	.default_real_type = 0,  /* real */
};

/* ISO 10206 Extended Pascal */
dialect_features_t iso_extended_features = {
	.allow_nested_comments = 1,  /* Extended Pascal allows this */
	.allow_cpp_comments = 0,
	.allow_dollar_ident = 0,
	.allow_underscores = 0,
	.allow_inline_assembly = 0,
	.allow_goto = 1,

	.allow_cstring_literals = 0,
	.allow_string_concat = 1,
	.allow_escape_sequences = 0,
	.pstring_literals = 1,

	.allow_unsigned_types = 0,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 1,  /* Extended Pascal has conformant arrays */
	.allow_set_operators = 1,
	.allow_string_type = 1,  /* Extended Pascal has string type */

	.allow_objects = 0,
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 0,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 0,
	.allow_varargs = 0,

	.allow_units = 1,  /* Extended Pascal has modules */
	.allow_namespaces = 0,
	.allow_uses_clause = 1,

	.allow_break_continue = 0,
	.allow_exit_function = 0,
	.allow_case_else = 1,

	.allow_absolute = 0,
	.allow_packed = 1,
	.allow_far_near = 0,
	.allow_interrupt = 0,
	.allow_assembler = 0,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 1,
	.require_forward_decl = 1,

	.default_int_size = 32,
	.default_real_type = 1,  /* double */
};

/* Microsoft Pascal 4.0 */
dialect_features_t microsoft_features = {
	.allow_nested_comments = 0,
	.allow_cpp_comments = 0,
	.allow_dollar_ident = 1,
	.allow_underscores = 1,
	.allow_inline_assembly = 1,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 1,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 0,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 0,
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 0,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 1,
	.allow_varargs = 0,

	.allow_units = 0,
	.allow_namespaces = 0,
	.allow_uses_clause = 0,

	.allow_break_continue = 0,
	.allow_exit_function = 0,
	.allow_case_else = 1,

	.allow_absolute = 1,
	.allow_packed = 1,
	.allow_far_near = 1,  /* MS Pascal has far/near */
	.allow_interrupt = 1,
	.allow_assembler = 1,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 1,
	.require_forward_decl = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* Apple Clascal (Object Pascal for Classic Mac) */
dialect_features_t clascal_features = {
	.allow_nested_comments = 0,
	.allow_cpp_comments = 0,
	.allow_dollar_ident = 0,
	.allow_underscores = 1,
	.allow_inline_assembly = 0,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 0,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 0,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 1,  /* Clascal is Object Pascal */
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 0,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 0,
	.allow_varargs = 0,

	.allow_units = 1,
	.allow_namespaces = 0,
	.allow_uses_clause = 1,

	.allow_break_continue = 0,
	.allow_exit_function = 0,
	.allow_case_else = 1,

	.allow_absolute = 0,
	.allow_packed = 1,
	.allow_far_near = 0,
	.allow_interrupt = 0,
	.allow_assembler = 0,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 0,
	.require_forward_decl = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* Apple MacPascal / MPW Pascal */
dialect_features_t macpascal_features = {
	.allow_nested_comments = 0,
	.allow_cpp_comments = 0,
	.allow_dollar_ident = 0,
	.allow_underscores = 1,
	.allow_inline_assembly = 1,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 0,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 0,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 1,
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 1,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 0,
	.allow_varargs = 0,

	.allow_units = 1,
	.allow_namespaces = 0,
	.allow_uses_clause = 1,

	.allow_break_continue = 0,
	.allow_exit_function = 0,
	.allow_case_else = 1,

	.allow_absolute = 0,
	.allow_packed = 1,
	.allow_far_near = 0,
	.allow_interrupt = 0,
	.allow_assembler = 0,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 0,
	.require_forward_decl = 0,

	.default_int_size = 16,
	.default_real_type = 2,  /* extended */
};

/* Borland Pascal / Turbo Pascal 7.0 */
dialect_features_t borland_features = {
	.allow_nested_comments = 1,
	.allow_cpp_comments = 1,
	.allow_dollar_ident = 1,
	.allow_underscores = 1,
	.allow_inline_assembly = 1,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 1,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 0,
	.allow_currency = 0,
	.allow_variant = 0,
	.allow_dynamic_arrays = 0,
	.allow_open_arrays = 1,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 1,  /* Turbo Pascal has objects */
	.allow_classes = 0,
	.allow_interfaces = 0,
	.allow_properties = 0,
	.allow_operator_overload = 0,

	.allow_default_params = 0,
	.allow_overloading = 0,
	.allow_inline_directive = 1,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 1,
	.allow_varargs = 0,

	.allow_units = 1,
	.allow_namespaces = 0,
	.allow_uses_clause = 1,

	.allow_break_continue = 1,  /* Turbo Pascal has break/continue */
	.allow_exit_function = 1,
	.allow_case_else = 1,

	.allow_absolute = 1,
	.allow_packed = 1,
	.allow_far_near = 1,
	.allow_interrupt = 1,
	.allow_assembler = 1,
	.allow_exports = 0,
	.allow_resourcestring = 0,
	.allow_threadvar = 0,
	.allow_generic = 0,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 0,
	.require_forward_decl = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* Delphi Object Pascal */
dialect_features_t delphi_features = {
	.allow_nested_comments = 1,
	.allow_cpp_comments = 1,
	.allow_dollar_ident = 1,
	.allow_underscores = 1,
	.allow_inline_assembly = 1,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 1,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 1,
	.allow_currency = 1,
	.allow_variant = 1,
	.allow_dynamic_arrays = 1,
	.allow_open_arrays = 1,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 1,
	.allow_classes = 1,  /* Delphi has full OOP */
	.allow_interfaces = 1,
	.allow_properties = 1,
	.allow_operator_overload = 1,

	.allow_default_params = 1,
	.allow_overloading = 1,
	.allow_inline_directive = 1,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 0,
	.allow_varargs = 1,

	.allow_units = 1,
	.allow_namespaces = 1,
	.allow_uses_clause = 1,

	.allow_break_continue = 1,
	.allow_exit_function = 1,
	.allow_case_else = 1,

	.allow_absolute = 1,
	.allow_packed = 1,
	.allow_far_near = 0,  /* Deprecated in 32-bit Delphi */
	.allow_interrupt = 0,
	.allow_assembler = 1,
	.allow_exports = 1,
	.allow_resourcestring = 1,
	.allow_threadvar = 1,
	.allow_generic = 1,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 0,
	.require_forward_decl = 0,

	.default_int_size = 32,
	.default_real_type = 1,  /* double */
};

/* Free Pascal (default mode) */
dialect_features_t freepascal_features = {
	.allow_nested_comments = 1,
	.allow_cpp_comments = 1,
	.allow_dollar_ident = 1,
	.allow_underscores = 1,
	.allow_inline_assembly = 1,
	.allow_goto = 1,

	.allow_cstring_literals = 1,
	.allow_string_concat = 1,
	.allow_escape_sequences = 1,
	.pstring_literals = 1,

	.allow_unsigned_types = 1,
	.allow_int64 = 1,
	.allow_currency = 1,
	.allow_variant = 1,
	.allow_dynamic_arrays = 1,
	.allow_open_arrays = 1,
	.allow_set_operators = 1,
	.allow_string_type = 1,

	.allow_objects = 1,
	.allow_classes = 1,
	.allow_interfaces = 1,
	.allow_properties = 1,
	.allow_operator_overload = 1,

	.allow_default_params = 1,
	.allow_overloading = 1,
	.allow_inline_directive = 1,
	.allow_forward_directive = 1,
	.allow_external_directive = 1,
	.allow_interrupt_directive = 1,
	.allow_varargs = 1,

	.allow_units = 1,
	.allow_namespaces = 1,
	.allow_uses_clause = 1,

	.allow_break_continue = 1,
	.allow_exit_function = 1,
	.allow_case_else = 1,

	.allow_absolute = 1,
	.allow_packed = 1,
	.allow_far_near = 1,
	.allow_interrupt = 1,
	.allow_assembler = 1,
	.allow_exports = 1,
	.allow_resourcestring = 1,
	.allow_threadvar = 1,
	.allow_generic = 1,

	.case_sensitive = 0,
	.strict_iso = 0,
	.require_program_header = 0,
	.require_forward_decl = 0,

	.default_int_size = 32,
	.default_real_type = 1,  /* double */
};

/*
 * Initialize dialect system
 */
void
dialect_init(void)
{
	/* Default to ISO Pascal */
	set_dialect(DIALECT_ISO);
}

/*
 * Set current dialect and configure features
 */
void
set_dialect(pascal_dialect_t dialect)
{
	current_dialect = dialect;

	switch (dialect) {
	case DIALECT_ISO:
		dialect_features = &iso_features;
		break;
	case DIALECT_ISO_EXTENDED:
		dialect_features = &iso_extended_features;
		break;
	case DIALECT_MICROSOFT:
		dialect_features = &microsoft_features;
		break;
	case DIALECT_CLASCAL:
		dialect_features = &clascal_features;
		break;
	case DIALECT_MACPASCAL:
		dialect_features = &macpascal_features;
		break;
	case DIALECT_BORLAND:
		dialect_features = &borland_features;
		break;
	case DIALECT_DELPHI:
		dialect_features = &delphi_features;
		break;
	case DIALECT_FREEPASCAL:
		dialect_features = &freepascal_features;
		break;
	case DIALECT_AUTO:
		/* Default to FreePascal for auto mode (most permissive) */
		dialect_features = &freepascal_features;
		break;
	default:
		dialect_features = &iso_features;
		break;
	}
}

/*
 * Get dialect name as string
 */
const char *
get_dialect_name(pascal_dialect_t dialect)
{
	switch (dialect) {
	case DIALECT_ISO:
		return "ISO Pascal";
	case DIALECT_ISO_EXTENDED:
		return "ISO Extended Pascal";
	case DIALECT_MICROSOFT:
		return "Microsoft Pascal 4.0";
	case DIALECT_CLASCAL:
		return "Clascal";
	case DIALECT_MACPASCAL:
		return "MacPascal";
	case DIALECT_BORLAND:
		return "Borland Pascal";
	case DIALECT_DELPHI:
		return "Delphi";
	case DIALECT_FREEPASCAL:
		return "Free Pascal";
	case DIALECT_AUTO:
		return "Auto-detect";
	default:
		return "Unknown";
	}
}

/*
 * Parse dialect from command-line string
 */
pascal_dialect_t
parse_dialect(const char *name)
{
	if (strcasecmp(name, "iso") == 0)
		return DIALECT_ISO;
	if (strcasecmp(name, "iso-extended") == 0 || strcasecmp(name, "extended") == 0)
		return DIALECT_ISO_EXTENDED;
	if (strcasecmp(name, "microsoft") == 0 || strcasecmp(name, "ms") == 0)
		return DIALECT_MICROSOFT;
	if (strcasecmp(name, "clascal") == 0)
		return DIALECT_CLASCAL;
	if (strcasecmp(name, "macpascal") == 0 || strcasecmp(name, "mac") == 0)
		return DIALECT_MACPASCAL;
	if (strcasecmp(name, "borland") == 0 || strcasecmp(name, "turbo") == 0 ||
	    strcasecmp(name, "tp") == 0)
		return DIALECT_BORLAND;
	if (strcasecmp(name, "delphi") == 0)
		return DIALECT_DELPHI;
	if (strcasecmp(name, "freepascal") == 0 || strcasecmp(name, "fpc") == 0)
		return DIALECT_FREEPASCAL;
	if (strcasecmp(name, "auto") == 0)
		return DIALECT_AUTO;

	return DIALECT_ISO;  /* Default */
}
