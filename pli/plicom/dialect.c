/*
 * Copyright (c) 2025 PCC PL/I Compiler
 *
 * PL/I dialect support implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "dialect.h"

/* Current dialect */
pli_dialect_t current_dialect = DIALECT_PLI;
dialect_features_t *dialect_features = NULL;

/* Feature sets for different dialects */
dialect_features_t pli_features;
dialect_features_t pli_subset_features;
dialect_features_t pli_optimizing_features;
dialect_features_t plm_features;
dialect_features_t plm86_features;
dialect_features_t plm386_features;
dialect_features_t plc_features;

/* Initialize standard PL/I features */
static void init_pli_features(void) {
	memset(&pli_features, 0, sizeof(pli_features));

	/* Basic language features */
	pli_features.allow_abbreviated_keywords = 1;
	pli_features.allow_dollar_ident = 1;
	pli_features.allow_underscores = 1;
	pli_features.min_keyword_length = 2;
	pli_features.case_sensitive = 0;

	/* Comments */
	pli_features.allow_c_comments = 1;
	pli_features.allow_cpp_comments = 0;
	pli_features.allow_nested_comments = 0;

	/* Data types */
	pli_features.allow_picture = 1;
	pli_features.allow_area = 1;
	pli_features.allow_task = 1;
	pli_features.allow_event = 1;
	pli_features.allow_ordinal = 1;
	pli_features.allow_generic = 1;
	pli_features.allow_builtin = 1;
	pli_features.allow_defined = 1;
	pli_features.allow_based = 1;
	pli_features.allow_controlled = 1;
	pli_features.allow_static = 1;

	/* Numeric defaults */
	pli_features.default_fixed_binary = 1;
	pli_features.max_fixed_precision = 31;
	pli_features.max_float_precision = 53;

	/* Strings */
	pli_features.allow_varying_strings = 1;
	pli_features.max_char_length = 32767;
	pli_features.max_bit_length = 32767;
	pli_features.allow_graphic = 1;
	pli_features.allow_widechar = 1;

	/* Structures */
	pli_features.allow_level_numbers = 1;
	pli_features.allow_factored_declares = 1;
	pli_features.allow_union = 1;
	pli_features.max_structure_depth = 255;

	/* Arrays */
	pli_features.allow_asterisk_extent = 1;
	pli_features.allow_refer_option = 1;
	pli_features.allow_hbound_lbound = 1;
	pli_features.max_array_dimensions = 32;
	pli_features.arrays_start_at_one = 1;

	/* Procedures */
	pli_features.allow_recursive = 1;
	pli_features.allow_reentrant = 1;
	pli_features.allow_main_procedure = 1;
	pli_features.allow_options_main = 1;
	pli_features.allow_entry_points = 1;
	pli_features.allow_generic_entry = 1;
	pli_features.allow_returns = 1;
	pli_features.allow_external = 1;
	pli_features.allow_internal = 1;

	/* Control structures */
	pli_features.allow_do_case = 0;
	pli_features.allow_do_while = 1;
	pli_features.allow_do_until = 1;
	pli_features.allow_do_to_by = 1;
	pli_features.allow_leave = 1;
	pli_features.allow_iterate = 1;
	pli_features.allow_select = 1;
	pli_features.allow_otherwise = 1;

	/* I/O */
	pli_features.allow_get_put = 1;
	pli_features.allow_read_write = 1;
	pli_features.allow_stream_io = 1;
	pli_features.allow_record_io = 1;
	pli_features.allow_locate_mode = 1;
	pli_features.allow_keyed_io = 1;
	pli_features.allow_sequential = 1;
	pli_features.allow_direct = 1;

	/* Exception handling */
	pli_features.allow_on_conditions = 1;
	pli_features.allow_signal_revert = 1;
	pli_features.allow_error_condition = 1;
	pli_features.allow_endfile_condition = 1;
	pli_features.allow_conversion_condition = 1;
	pli_features.allow_fixedoverflow = 1;
	pli_features.allow_zerodivide = 1;

	/* Compile-time facilities */
	pli_features.allow_percent_statements = 1;
	pli_features.allow_preprocessor = 1;
	pli_features.allow_include = 1;
	pli_features.allow_replace = 1;

	/* Storage management */
	pli_features.allow_allocate_free = 1;
	pli_features.allow_pointer_arithmetic = 1;
	pli_features.allow_addr_function = 1;
	pli_features.allow_null_builtin = 1;

	/* Multi-tasking */
	pli_features.allow_multitasking = 1;
	pli_features.allow_priority = 1;

	/* Misc */
	pli_features.allow_goto = 1;
	pli_features.allow_format = 1;
	pli_features.allow_return_expr = 1;
	pli_features.allow_stop_exit = 1;
	pli_features.allow_attributes_in_dcl = 1;
	pli_features.allow_implicit_declare = 0;
	pli_features.require_declare = 1;

	/* Defaults */
	pli_features.default_aligned = 1;
	pli_features.default_signed = 1;

	/* Not PL/M */
	pli_features.is_plm = 0;
}

/* Initialize PL/M features */
static void init_plm_features(void) {
	memset(&plm_features, 0, sizeof(plm_features));

	/* Copy base PL/I features */
	plm_features = pli_features;

	/* PL/M is simplified */
	plm_features.is_plm = 1;
	plm_features.allow_abbreviated_keywords = 1;
	plm_features.case_sensitive = 0;
	plm_features.allow_cpp_comments = 1;

	/* PL/M specific features */
	plm_features.allow_at_clause = 1;
	plm_features.allow_data_clause = 1;
	plm_features.allow_literally = 1;
	plm_features.allow_public_external = 1;
	plm_features.allow_interrupt = 1;
	plm_features.allow_byte_word_types = 1;
	plm_features.allow_do_case = 1;

	/* Limited features */
	plm_features.allow_picture = 0;
	plm_features.allow_area = 0;
	plm_features.allow_task = 0;
	plm_features.allow_event = 0;
	plm_features.allow_based = 0;
	plm_features.allow_controlled = 0;
	plm_features.max_fixed_precision = 16;
	plm_features.max_array_dimensions = 3;
	plm_features.allow_get_put = 0;
	plm_features.allow_stream_io = 0;
	plm_features.allow_record_io = 0;
	plm_features.allow_on_conditions = 0;
	plm_features.allow_multitasking = 0;
	plm_features.allow_percent_statements = 0;
	plm_features.allow_preprocessor = 0;

	/* Arrays start at 0 in PL/M */
	plm_features.arrays_start_at_one = 0;
}

/* Initialize PL/M-86 features */
static void init_plm86_features(void) {
	plm86_features = plm_features;
	plm86_features.allow_structure_assign = 0;  /* Not until 386 */
}

/* Initialize PL/M-386 features */
static void init_plm386_features(void) {
	plm386_features = plm86_features;
	plm386_features.allow_structure_assign = 1;
	plm386_features.max_array_dimensions = 8;
}

/* Initialize PL/C features (teaching dialect) */
static void init_plc_features(void) {
	plc_features = pli_features;
	plc_features.allow_implicit_declare = 1;  /* PL/C allows implicit */
	plc_features.require_declare = 0;
}

/* Initialize PL/I Subset G features */
static void init_pli_subset_features(void) {
	pli_subset_features = pli_features;

	/* Subset G has limitations */
	pli_subset_features.allow_task = 0;
	pli_subset_features.allow_event = 0;
	pli_subset_features.allow_multitasking = 0;
	pli_subset_features.allow_picture = 0;
	pli_subset_features.max_fixed_precision = 15;
	pli_subset_features.max_array_dimensions = 15;
	pli_subset_features.allow_based = 0;
	pli_subset_features.allow_controlled = 0;
}

/* Initialize Optimizing PL/I features */
static void init_pli_optimizing_features(void) {
	pli_optimizing_features = pli_features;
	/* Full PL/I with all optimizations */
}

/* Initialize dialect system */
void dialect_init(void) {
	init_pli_features();
	init_pli_subset_features();
	init_pli_optimizing_features();
	init_plm_features();
	init_plm86_features();
	init_plm386_features();
	init_plc_features();

	/* Default to standard PL/I */
	set_dialect(DIALECT_PLI);
}

/* Set dialect */
void set_dialect(pli_dialect_t dialect) {
	current_dialect = dialect;

	switch (dialect) {
	case DIALECT_PLI:
		dialect_features = &pli_features;
		break;
	case DIALECT_PLI_SUBSET:
		dialect_features = &pli_subset_features;
		break;
	case DIALECT_PLI_OPTIMIZING:
		dialect_features = &pli_optimizing_features;
		break;
	case DIALECT_PLM:
	case DIALECT_PLM86:
		dialect_features = &plm86_features;
		break;
	case DIALECT_PLM386:
		dialect_features = &plm386_features;
		break;
	case DIALECT_PLC:
		dialect_features = &plc_features;
		break;
	case DIALECT_PL1:
		/* PL/1 is same as PL/I */
		dialect_features = &pli_features;
		break;
	default:
		dialect_features = &pli_features;
		break;
	}
}

/* Get dialect name */
const char *get_dialect_name(pli_dialect_t dialect) {
	switch (dialect) {
	case DIALECT_PLI:            return "PL/I";
	case DIALECT_PLI_SUBSET:     return "PL/I Subset G";
	case DIALECT_PLI_OPTIMIZING: return "Optimizing PL/I";
	case DIALECT_PLM:            return "PL/M";
	case DIALECT_PLM86:          return "PL/M-86";
	case DIALECT_PLM386:         return "PL/M-386";
	case DIALECT_PLC:            return "PL/C";
	case DIALECT_PL1:            return "PL/1";
	case DIALECT_PLSQL:          return "PL/SQL";
	case DIALECT_AUTO:           return "Auto-detect";
	default:                     return "Unknown";
	}
}

/* Parse dialect from string */
pli_dialect_t parse_dialect(const char *name) {
	if (strcasecmp(name, "pli") == 0 || strcasecmp(name, "pl/i") == 0)
		return DIALECT_PLI;
	if (strcasecmp(name, "pl1") == 0 || strcasecmp(name, "pl/1") == 0)
		return DIALECT_PL1;
	if (strcasecmp(name, "subset") == 0 || strcasecmp(name, "subset-g") == 0)
		return DIALECT_PLI_SUBSET;
	if (strcasecmp(name, "optimizing") == 0)
		return DIALECT_PLI_OPTIMIZING;
	if (strcasecmp(name, "plm") == 0 || strcasecmp(name, "pl/m") == 0)
		return DIALECT_PLM;
	if (strcasecmp(name, "plm86") == 0 || strcasecmp(name, "pl/m-86") == 0)
		return DIALECT_PLM86;
	if (strcasecmp(name, "plm386") == 0 || strcasecmp(name, "pl/m-386") == 0)
		return DIALECT_PLM386;
	if (strcasecmp(name, "plc") == 0 || strcasecmp(name, "pl/c") == 0)
		return DIALECT_PLC;
	if (strcasecmp(name, "plsql") == 0 || strcasecmp(name, "pl/sql") == 0)
		return DIALECT_PLSQL;
	if (strcasecmp(name, "auto") == 0)
		return DIALECT_AUTO;

	return DIALECT_PLI;  /* Default */
}

/* Check if feature is enabled */
int feature_enabled(int feature) {
	/* This is a generic function, specific checks use macros */
	return 1;
}
