/*
 * Copyright (c) 2025 PCC Paradox PAL Compiler
 *
 * Dialect configuration implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dialect.h"

/* Global current dialect */
pal_dialect_t current_dialect = DIALECT_OBJECTPAL_LATEST;
dialect_features_t *dialect_features = NULL;

/* Predefined dialect feature sets */
dialect_features_t pal_1_0_features = {
	.allow_procedures = 1,
	.allow_functions = 0,
	.allow_local_vars = 0,
	.allow_nested_procedures = 0,
	.allow_objects = 0,
	.allow_methods = 0,
	.allow_properties = 0,
	.allow_inheritance = 0,
	.allow_events = 0,
	.allow_private_members = 0,
	.allow_constructors = 0,
	.allow_destructors = 0,
	.allow_variant_type = 0,
	.allow_currency_type = 0,
	.allow_blob_types = 0,
	.allow_datetime_types = 0,
	.allow_dynamic_arrays = 0,
	.allow_strings = 1,
	.allow_pointer_types = 0,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 0,
	.allow_switch_statement = 0,
	.allow_break_continue = 0,
	.allow_return_statement = 0,
	.allow_try_except = 0,
	.allow_sql_support = 0,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 0,
	.allow_form_events = 0,
	.allow_ui_objects = 0,
	.allow_menu_support = 0,
	.allow_report_objects = 0,
	.allow_graphics = 0,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 0,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 0,
	.allow_block_comments = 0,
	.allow_include_files = 0,
	.allow_debug_statements = 0,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 0,
	.stdlib_system = 1,
	.version_major = 1,
	.version_minor = 0,
	.version_name = "PAL 1.0"
};

dialect_features_t pal_3_0_features = {
	.allow_procedures = 1,
	.allow_functions = 1,
	.allow_local_vars = 1,
	.allow_nested_procedures = 0,
	.allow_objects = 0,
	.allow_methods = 0,
	.allow_properties = 0,
	.allow_inheritance = 0,
	.allow_events = 0,
	.allow_private_members = 0,
	.allow_constructors = 0,
	.allow_destructors = 0,
	.allow_variant_type = 0,
	.allow_currency_type = 0,
	.allow_blob_types = 1,
	.allow_datetime_types = 1,
	.allow_dynamic_arrays = 0,
	.allow_strings = 1,
	.allow_pointer_types = 0,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 0,
	.allow_switch_statement = 0,
	.allow_break_continue = 0,
	.allow_return_statement = 1,
	.allow_try_except = 0,
	.allow_sql_support = 1,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 0,
	.allow_form_events = 1,
	.allow_ui_objects = 1,
	.allow_menu_support = 1,
	.allow_report_objects = 1,
	.allow_graphics = 0,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 1,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 0,
	.allow_block_comments = 0,
	.allow_include_files = 0,
	.allow_debug_statements = 0,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 1,
	.stdlib_system = 1,
	.version_major = 3,
	.version_minor = 0,
	.version_name = "PAL 3.0"
};

dialect_features_t pal_4_5_features = {
	.allow_procedures = 1,
	.allow_functions = 1,
	.allow_local_vars = 1,
	.allow_nested_procedures = 0,
	.allow_objects = 0,
	.allow_methods = 0,
	.allow_properties = 0,
	.allow_inheritance = 0,
	.allow_events = 1,
	.allow_private_members = 0,
	.allow_constructors = 0,
	.allow_destructors = 0,
	.allow_variant_type = 0,
	.allow_currency_type = 1,
	.allow_blob_types = 1,
	.allow_datetime_types = 1,
	.allow_dynamic_arrays = 1,
	.allow_strings = 1,
	.allow_pointer_types = 0,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 0,
	.allow_switch_statement = 1,
	.allow_break_continue = 0,
	.allow_return_statement = 1,
	.allow_try_except = 0,
	.allow_sql_support = 1,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 0,
	.allow_form_events = 1,
	.allow_ui_objects = 1,
	.allow_menu_support = 1,
	.allow_report_objects = 1,
	.allow_graphics = 1,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 1,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 0,
	.allow_block_comments = 1,
	.allow_include_files = 1,
	.allow_debug_statements = 0,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 1,
	.stdlib_system = 1,
	.version_major = 4,
	.version_minor = 5,
	.version_name = "PAL 4.5"
};

dialect_features_t objectpal_1_0_features = {
	.allow_procedures = 1,
	.allow_functions = 1,
	.allow_local_vars = 1,
	.allow_nested_procedures = 1,
	.allow_objects = 1,
	.allow_methods = 1,
	.allow_properties = 1,
	.allow_inheritance = 1,
	.allow_events = 1,
	.allow_private_members = 0,
	.allow_constructors = 1,
	.allow_destructors = 0,
	.allow_variant_type = 1,
	.allow_currency_type = 1,
	.allow_blob_types = 1,
	.allow_datetime_types = 1,
	.allow_dynamic_arrays = 1,
	.allow_strings = 1,
	.allow_pointer_types = 1,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 0,
	.allow_switch_statement = 1,
	.allow_break_continue = 1,
	.allow_return_statement = 1,
	.allow_try_except = 1,
	.allow_sql_support = 1,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 1,
	.allow_form_events = 1,
	.allow_ui_objects = 1,
	.allow_menu_support = 1,
	.allow_report_objects = 1,
	.allow_graphics = 1,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 1,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 1,
	.allow_block_comments = 1,
	.allow_include_files = 1,
	.allow_debug_statements = 1,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 1,
	.stdlib_system = 1,
	.version_major = 1,
	.version_minor = 0,
	.version_name = "ObjectPAL 1.0"
};

dialect_features_t objectpal_7_0_features = {
	.allow_procedures = 1,
	.allow_functions = 1,
	.allow_local_vars = 1,
	.allow_nested_procedures = 1,
	.allow_objects = 1,
	.allow_methods = 1,
	.allow_properties = 1,
	.allow_inheritance = 1,
	.allow_events = 1,
	.allow_private_members = 1,
	.allow_constructors = 1,
	.allow_destructors = 1,
	.allow_variant_type = 1,
	.allow_currency_type = 1,
	.allow_blob_types = 1,
	.allow_datetime_types = 1,
	.allow_dynamic_arrays = 1,
	.allow_strings = 1,
	.allow_pointer_types = 1,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 1,
	.allow_switch_statement = 1,
	.allow_break_continue = 1,
	.allow_return_statement = 1,
	.allow_try_except = 1,
	.allow_sql_support = 1,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 1,
	.allow_form_events = 1,
	.allow_ui_objects = 1,
	.allow_menu_support = 1,
	.allow_report_objects = 1,
	.allow_graphics = 1,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 1,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 1,
	.allow_block_comments = 1,
	.allow_include_files = 1,
	.allow_debug_statements = 1,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 1,
	.stdlib_system = 1,
	.version_major = 7,
	.version_minor = 0,
	.version_name = "ObjectPAL 7.0"
};

dialect_features_t objectpal_latest_features = {
	.allow_procedures = 1,
	.allow_functions = 1,
	.allow_local_vars = 1,
	.allow_nested_procedures = 1,
	.allow_objects = 1,
	.allow_methods = 1,
	.allow_properties = 1,
	.allow_inheritance = 1,
	.allow_events = 1,
	.allow_private_members = 1,
	.allow_constructors = 1,
	.allow_destructors = 1,
	.allow_variant_type = 1,
	.allow_currency_type = 1,
	.allow_blob_types = 1,
	.allow_datetime_types = 1,
	.allow_dynamic_arrays = 1,
	.allow_strings = 1,
	.allow_pointer_types = 1,
	.allow_while_loop = 1,
	.allow_for_loop = 1,
	.allow_foreach_loop = 1,
	.allow_switch_statement = 1,
	.allow_break_continue = 1,
	.allow_return_statement = 1,
	.allow_try_except = 1,
	.allow_sql_support = 1,
	.allow_table_operations = 1,
	.allow_query_by_example = 1,
	.allow_paradox_tables = 1,
	.allow_odbc_support = 1,
	.allow_form_events = 1,
	.allow_ui_objects = 1,
	.allow_menu_support = 1,
	.allow_report_objects = 1,
	.allow_graphics = 1,
	.allow_string_concat = 1,
	.allow_string_functions = 1,
	.allow_format_functions = 1,
	.allow_semicolon_comments = 1,
	.allow_slash_comments = 1,
	.allow_block_comments = 1,
	.allow_include_files = 1,
	.allow_debug_statements = 1,
	.allow_inline_code = 0,
	.case_sensitive = 0,
	.stdlib_math = 1,
	.stdlib_string = 1,
	.stdlib_date = 1,
	.stdlib_file = 1,
	.stdlib_system = 1,
	.version_major = 9,
	.version_minor = 0,
	.version_name = "ObjectPAL Latest"
};

void dialect_init(void)
{
	/* Default to latest ObjectPAL */
	current_dialect = DIALECT_OBJECTPAL_LATEST;
	dialect_features = &objectpal_latest_features;
}

void set_dialect(pal_dialect_t dialect)
{
	current_dialect = dialect;

	switch (dialect) {
	case DIALECT_PAL_1_0:
		dialect_features = &pal_1_0_features;
		break;
	case DIALECT_PAL_3_0:
		dialect_features = &pal_3_0_features;
		break;
	case DIALECT_PAL_4_5:
		dialect_features = &pal_4_5_features;
		break;
	case DIALECT_OBJECTPAL_1_0:
		dialect_features = &objectpal_1_0_features;
		break;
	case DIALECT_OBJECTPAL_7_0:
		dialect_features = &objectpal_7_0_features;
		break;
	case DIALECT_OBJECTPAL_LATEST:
		dialect_features = &objectpal_latest_features;
		break;
	default:
		dialect_features = &objectpal_latest_features;
		break;
	}
}

const char *get_dialect_name(pal_dialect_t dialect)
{
	switch (dialect) {
	case DIALECT_PAL_1_0:
		return "PAL 1.0";
	case DIALECT_PAL_3_0:
		return "PAL 3.0";
	case DIALECT_PAL_4_5:
		return "PAL 4.5";
	case DIALECT_OBJECTPAL_1_0:
		return "ObjectPAL 1.0";
	case DIALECT_OBJECTPAL_7_0:
		return "ObjectPAL 7.0";
	case DIALECT_OBJECTPAL_LATEST:
		return "ObjectPAL Latest";
	default:
		return "Unknown";
	}
}

pal_dialect_t parse_dialect(const char *name)
{
	if (strcmp(name, "pal-1.0") == 0)
		return DIALECT_PAL_1_0;
	if (strcmp(name, "pal-3.0") == 0)
		return DIALECT_PAL_3_0;
	if (strcmp(name, "pal-4.5") == 0)
		return DIALECT_PAL_4_5;
	if (strcmp(name, "objectpal-1.0") == 0)
		return DIALECT_OBJECTPAL_1_0;
	if (strcmp(name, "objectpal-7.0") == 0)
		return DIALECT_OBJECTPAL_7_0;
	if (strcmp(name, "objectpal-latest") == 0)
		return DIALECT_OBJECTPAL_LATEST;
	return (pal_dialect_t)-1;
}
