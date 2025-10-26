/*
 * Copyright (c) 2025 PCC BASIC Compiler
 *
 * BASIC dialect configuration
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dialect.h"

/* Current dialect */
basic_dialect_t current_dialect = DIALECT_GWBASIC;
dialect_features_t *dialect_features = NULL;

/* GW-BASIC (Microsoft, 1983) - Classic BASIC */
dialect_features_t gwbasic_features = {
	.require_line_numbers = 1,
	.allow_line_labels = 0,
	.allow_multistatement = 1,  /* : separator */

	.allow_rem_statement = 1,
	.allow_quote_comment = 0,
	.allow_inline_comments = 1,

	.allow_long_names = 0,  /* 2-char limit on GW-BASIC */
	.allow_underscores = 0,
	.require_type_suffix = 1,
	.allow_explicit_types = 0,
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 0,
	.max_string_length = 255,

	.allow_integer = 1,
	.allow_long = 0,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 0,
	.allow_byte = 0,
	.allow_boolean = 0,
	.allow_variant = 0,
	.allow_udt = 0,

	.allow_dynamic_arrays = 0,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 0,  /* Only IF THEN GOTO */
	.allow_select_case = 0,
	.allow_do_loop = 0,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 0,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 0,
	.allow_function_procedures = 0,
	.allow_byref_byval = 0,
	.allow_optional_params = 0,
	.allow_param_arrays = 0,

	.allow_static = 0,
	.allow_shared = 0,
	.allow_common = 1,
	.allow_local = 0,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 0,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 0,
	.allow_pointer_ops = 0,
	.allow_oop = 0,

	.allow_meta_commands = 0,
	.allow_option_explicit = 0,
	.allow_option_compare = 0,

	.allow_overloading = 0,
	.allow_operator_overload = 0,
	.allow_namespaces = 0,
	.allow_classes = 0,

	.default_int_size = 16,
	.default_real_type = 0,  /* single */
};

/* Turbo BASIC (Borland, 1987) */
dialect_features_t turbo_features = {
	.require_line_numbers = 0,  /* Optional */
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 1,  /* ' comments */
	.allow_inline_comments = 1,

	.allow_long_names = 1,  /* Up to 40 chars */
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,  /* AS INTEGER, etc. */
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 1,
	.max_string_length = 32767,

	.allow_integer = 1,
	.allow_long = 1,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 0,
	.allow_byte = 0,
	.allow_boolean = 0,
	.allow_variant = 0,
	.allow_udt = 0,

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,  /* IF...THEN...ELSE...END IF */
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 0,
	.allow_optional_params = 0,
	.allow_param_arrays = 0,

	.allow_static = 1,
	.allow_shared = 1,
	.allow_common = 1,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 1,
	.allow_pointer_ops = 0,
	.allow_oop = 0,

	.allow_meta_commands = 1,  /* $INCLUDE, etc. */
	.allow_option_explicit = 0,
	.allow_option_compare = 0,

	.allow_overloading = 0,
	.allow_operator_overload = 0,
	.allow_namespaces = 0,
	.allow_classes = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* Visual Basic for DOS (Microsoft, 1992) */
dialect_features_t vbdos_features = {
	.require_line_numbers = 0,
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 1,
	.allow_inline_comments = 1,

	.allow_long_names = 1,
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 1,
	.max_string_length = 32767,

	.allow_integer = 1,
	.allow_long = 1,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 1,
	.allow_byte = 0,
	.allow_boolean = 0,
	.allow_variant = 1,
	.allow_udt = 1,  /* TYPE...END TYPE */

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 1,  /* BYREF/BYVAL */
	.allow_optional_params = 1,
	.allow_param_arrays = 0,

	.allow_static = 1,
	.allow_shared = 1,
	.allow_common = 1,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 0,
	.allow_pointer_ops = 0,
	.allow_oop = 0,

	.allow_meta_commands = 1,
	.allow_option_explicit = 1,
	.allow_option_compare = 1,

	.allow_overloading = 0,
	.allow_operator_overload = 0,
	.allow_namespaces = 0,
	.allow_classes = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* Microsoft BASIC PDS 7.1 (Professional Development System, 1990) */
dialect_features_t pds_features = {
	.require_line_numbers = 0,
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 1,
	.allow_inline_comments = 1,

	.allow_long_names = 1,
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 1,
	.max_string_length = 32767,

	.allow_integer = 1,
	.allow_long = 1,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 0,
	.allow_byte = 0,
	.allow_boolean = 0,
	.allow_variant = 0,
	.allow_udt = 1,

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 1,
	.allow_optional_params = 0,
	.allow_param_arrays = 0,

	.allow_static = 1,
	.allow_shared = 1,
	.allow_common = 1,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 1,
	.allow_pointer_ops = 0,
	.allow_oop = 0,

	.allow_meta_commands = 1,
	.allow_option_explicit = 1,
	.allow_option_compare = 0,

	.allow_overloading = 0,
	.allow_operator_overload = 0,
	.allow_namespaces = 0,
	.allow_classes = 0,

	.default_int_size = 16,
	.default_real_type = 0,
};

/* PowerBASIC (PowerBASIC Inc., 1989-present) */
dialect_features_t powerbasic_features = {
	.require_line_numbers = 0,
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 1,
	.allow_inline_comments = 1,

	.allow_long_names = 1,
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 1,
	.max_string_length = 2147483647,  /* 2GB */

	.allow_integer = 1,
	.allow_long = 1,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 1,
	.allow_byte = 1,
	.allow_boolean = 1,
	.allow_variant = 1,
	.allow_udt = 1,

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 1,
	.allow_optional_params = 1,
	.allow_param_arrays = 1,

	.allow_static = 1,
	.allow_shared = 1,
	.allow_common = 1,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 1,
	.allow_pointer_ops = 1,
	.allow_oop = 0,  /* PowerBASIC has COM support but not full OOP */

	.allow_meta_commands = 1,
	.allow_option_explicit = 1,
	.allow_option_compare = 1,

	.allow_overloading = 1,
	.allow_operator_overload = 0,
	.allow_namespaces = 0,
	.allow_classes = 0,

	.default_int_size = 32,
	.default_real_type = 0,
};

/* FreeBASIC (2004-present) - Modern, C-compatible BASIC */
dialect_features_t freebasic_features = {
	.require_line_numbers = 0,
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 1,
	.allow_inline_comments = 1,

	.allow_long_names = 1,
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,
	.case_sensitive = 0,  /* Default, but can be enabled */

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 1,
	.max_string_length = 0,  /* unlimited */

	.allow_integer = 1,
	.allow_long = 1,
	.allow_single = 1,
	.allow_double = 1,
	.allow_string = 1,
	.allow_currency = 0,
	.allow_byte = 1,
	.allow_boolean = 1,
	.allow_variant = 0,
	.allow_udt = 1,

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 1,
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 1,
	.allow_optional_params = 1,
	.allow_param_arrays = 1,

	.allow_static = 1,
	.allow_shared = 1,
	.allow_common = 1,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 1,

	.allow_peek_poke = 1,
	.allow_inline_asm = 1,
	.allow_pointer_ops = 1,
	.allow_oop = 1,  /* Full OOP support */

	.allow_meta_commands = 1,
	.allow_option_explicit = 1,
	.allow_option_compare = 0,

	.allow_overloading = 1,
	.allow_operator_overload = 1,
	.allow_namespaces = 1,
	.allow_classes = 1,  /* TYPE with methods */

	.default_int_size = 32,
	.default_real_type = 1,  /* double */
};

/* ISO BASIC Advanced (ANSI/ISO/IEC 10279:1991) */
dialect_features_t iso_features = {
	.require_line_numbers = 0,
	.allow_line_labels = 1,
	.allow_multistatement = 1,

	.allow_rem_statement = 1,
	.allow_quote_comment = 0,  /* ! comments in ISO */
	.allow_inline_comments = 1,

	.allow_long_names = 1,
	.allow_underscores = 1,
	.require_type_suffix = 0,
	.allow_explicit_types = 1,
	.case_sensitive = 0,

	.allow_string_functions = 1,
	.allow_string_concat = 1,
	.allow_fixed_strings = 0,
	.max_string_length = 0,

	.allow_integer = 1,
	.allow_long = 0,
	.allow_single = 0,
	.allow_double = 1,  /* NUMERIC type */
	.allow_string = 1,
	.allow_currency = 0,
	.allow_byte = 0,
	.allow_boolean = 0,
	.allow_variant = 0,
	.allow_udt = 1,  /* STRUCTURE */

	.allow_dynamic_arrays = 1,
	.allow_multidim_arrays = 1,
	.default_array_base = 0,
	.allow_option_base = 1,

	.allow_structured_if = 1,
	.allow_select_case = 1,
	.allow_do_loop = 1,
	.allow_while_wend = 0,  /* ISO uses DO WHILE */
	.allow_for_next = 1,
	.allow_for_step = 1,
	.allow_exit_statement = 1,

	.allow_gosub = 1,
	.allow_def_fn = 1,
	.allow_sub_procedures = 1,
	.allow_function_procedures = 1,
	.allow_byref_byval = 1,
	.allow_optional_params = 0,
	.allow_param_arrays = 0,

	.allow_static = 0,
	.allow_shared = 1,
	.allow_common = 0,
	.allow_local = 1,

	.allow_print_using = 1,
	.allow_input_prompt = 1,
	.allow_line_input = 1,
	.allow_file_io = 1,
	.allow_binary_files = 1,
	.allow_random_files = 1,

	.allow_graphics = 1,
	.allow_screen_modes = 1,
	.allow_sound = 0,

	.allow_peek_poke = 0,
	.allow_inline_asm = 0,
	.allow_pointer_ops = 0,
	.allow_oop = 1,  /* MODULE with OOP features */

	.allow_meta_commands = 0,
	.allow_option_explicit = 1,
	.allow_option_compare = 0,

	.allow_overloading = 0,
	.allow_operator_overload = 0,
	.allow_namespaces = 1,  /* MODULE */
	.allow_classes = 0,

	.default_int_size = 32,
	.default_real_type = 1,
};

/*
 * Initialize dialect system
 */
void
dialect_init(void)
{
	/* Default to GW-BASIC */
	set_dialect(DIALECT_GWBASIC);
}

/*
 * Set current dialect and configure features
 */
void
set_dialect(basic_dialect_t dialect)
{
	current_dialect = dialect;

	switch (dialect) {
	case DIALECT_GWBASIC:
		dialect_features = &gwbasic_features;
		break;
	case DIALECT_TURBO:
		dialect_features = &turbo_features;
		break;
	case DIALECT_VBDOS:
		dialect_features = &vbdos_features;
		break;
	case DIALECT_PDS:
		dialect_features = &pds_features;
		break;
	case DIALECT_POWERBASIC:
		dialect_features = &powerbasic_features;
		break;
	case DIALECT_FREEBASIC:
		dialect_features = &freebasic_features;
		break;
	case DIALECT_ISO:
		dialect_features = &iso_features;
		break;
	case DIALECT_AUTO:
		/* Default to FreeBASIC for auto mode (most permissive) */
		dialect_features = &freebasic_features;
		break;
	default:
		dialect_features = &gwbasic_features;
		break;
	}
}

/*
 * Get dialect name as string
 */
const char *
get_dialect_name(basic_dialect_t dialect)
{
	switch (dialect) {
	case DIALECT_GWBASIC:
		return "GW-BASIC";
	case DIALECT_TURBO:
		return "Turbo BASIC";
	case DIALECT_VBDOS:
		return "Visual Basic for DOS";
	case DIALECT_PDS:
		return "Microsoft BASIC PDS 7.1";
	case DIALECT_POWERBASIC:
		return "PowerBASIC";
	case DIALECT_FREEBASIC:
		return "FreeBASIC";
	case DIALECT_ISO:
		return "ISO BASIC Advanced";
	case DIALECT_AUTO:
		return "Auto-detect";
	default:
		return "Unknown";
	}
}

/*
 * Parse dialect from command-line string
 */
basic_dialect_t
parse_dialect(const char *name)
{
	if (strcasecmp(name, "gwbasic") == 0 || strcasecmp(name, "gw") == 0)
		return DIALECT_GWBASIC;
	if (strcasecmp(name, "turbo") == 0 || strcasecmp(name, "turbobasic") == 0)
		return DIALECT_TURBO;
	if (strcasecmp(name, "vbdos") == 0 || strcasecmp(name, "vb") == 0)
		return DIALECT_VBDOS;
	if (strcasecmp(name, "pds") == 0 || strcasecmp(name, "basicpds") == 0)
		return DIALECT_PDS;
	if (strcasecmp(name, "powerbasic") == 0 || strcasecmp(name, "pb") == 0)
		return DIALECT_POWERBASIC;
	if (strcasecmp(name, "freebasic") == 0 || strcasecmp(name, "fb") == 0)
		return DIALECT_FREEBASIC;
	if (strcasecmp(name, "iso") == 0 || strcasecmp(name, "isobasic") == 0)
		return DIALECT_ISO;
	if (strcasecmp(name, "auto") == 0)
		return DIALECT_AUTO;

	return DIALECT_GWBASIC;  /* Default */
}
