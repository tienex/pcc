/*
 * Language-Specific Debug Symbol Support - Core Implementation
 */

#include "pass1.h"
#include "debugsym.h"
#include "debugsym_lang.h"
#include <string.h>
#include <stdlib.h>

/* Current language setting */
static debug_language_t current_language = LANG_C99;

/*
 * Language Information Table
 * Comprehensive metadata for all supported languages
 */
const language_info_t language_table[] = {
	/* C Family */
	{ LANG_C89, "C89", "ANSI C (1989)", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_C99, "C99", "ISO C (1999)", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_C11, "C11", "ISO C (2011)", 0, 0, 0, 0, 0, 0, 0, 1 },
	{ LANG_C17, "C17", "ISO C (2017)", 0, 0, 0, 0, 0, 0, 0, 1 },
	{ LANG_C23, "C23", "ISO C (2023)", 0, 0, 0, 0, 0, 0, 0, 1 },

	/* C++ Family */
	{ LANG_CPP, "C++", "C++", 1, 1, 1, 1, 0, 0, 0, 0 },
	{ LANG_CPP_03, "C++03", "ISO C++ (2003)", 1, 1, 1, 1, 0, 0, 0, 0 },
	{ LANG_CPP_11, "C++11", "ISO C++ (2011)", 1, 1, 1, 1, 0, 0, 0, 1 },
	{ LANG_CPP_14, "C++14", "ISO C++ (2014)", 1, 1, 1, 1, 0, 0, 0, 1 },
	{ LANG_CPP_17, "C++17", "ISO C++ (2017)", 1, 1, 1, 1, 0, 0, 0, 1 },
	{ LANG_CPP_20, "C++20", "ISO C++ (2020)", 1, 1, 1, 1, 0, 1, 0, 1 },
	{ LANG_CPP_23, "C++23", "ISO C++ (2023)", 1, 1, 1, 1, 0, 1, 0, 1 },

	/* Objective-C */
	{ LANG_OBJC, "Objective-C", "Objective-C", 1, 0, 0, 1, 0, 0, 1, 0 },
	{ LANG_OBJCPP, "Objective-C++", "Objective-C++", 1, 1, 1, 1, 0, 0, 1, 0 },

	/* Fortran */
	{ LANG_FORTRAN77, "Fortran 77", "FORTRAN 77", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_FORTRAN90, "Fortran 90", "Fortran 90", 0, 0, 0, 0, 0, 1, 0, 0 },
	{ LANG_FORTRAN95, "Fortran 95", "Fortran 95", 0, 0, 0, 0, 0, 1, 0, 0 },
	{ LANG_FORTRAN03, "Fortran 2003", "Fortran 2003", 1, 0, 0, 0, 0, 1, 0, 0 },
	{ LANG_FORTRAN08, "Fortran 2008", "Fortran 2008", 1, 0, 0, 0, 0, 1, 0, 1 },
	{ LANG_FORTRAN18, "Fortran 2018", "Fortran 2018", 1, 0, 0, 0, 0, 1, 0, 1 },

	/* Pascal Family */
	{ LANG_PASCAL83, "Pascal", "ISO Pascal (1983)", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_MODULA2, "Modula-2", "Modula-2", 0, 0, 0, 0, 0, 1, 0, 1 },
	{ LANG_MODULA3, "Modula-3", "Modula-3", 1, 0, 0, 1, 0, 1, 1, 1 },
	{ LANG_OBERON, "Oberon", "Oberon", 1, 0, 0, 0, 0, 1, 1, 0 },

	/* Ada */
	{ LANG_ADA83, "Ada 83", "Ada (1983)", 0, 0, 0, 1, 1, 1, 0, 1 },
	{ LANG_ADA95, "Ada 95", "Ada 95", 1, 0, 0, 1, 1, 1, 0, 1 },
	{ LANG_ADA05, "Ada 2005", "Ada 2005", 1, 0, 0, 1, 1, 1, 0, 1 },
	{ LANG_ADA12, "Ada 2012", "Ada 2012", 1, 0, 0, 1, 1, 1, 0, 1 },

	/* COBOL */
	{ LANG_COBOL74, "COBOL 74", "COBOL (1974)", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_COBOL85, "COBOL 85", "COBOL (1985)", 0, 0, 0, 0, 0, 0, 0, 0 },
	{ LANG_COBOL02, "COBOL 2002", "COBOL 2002", 1, 0, 0, 1, 0, 0, 0, 0 },

	/* Modern Systems Languages */
	{ LANG_RUST, "Rust", "Rust", 0, 0, 0, 0, 1, 1, 0, 1 },
	{ LANG_GO, "Go", "Go", 0, 0, 0, 0, 0, 1, 1, 1 },
	{ LANG_D, "D", "D", 1, 1, 0, 1, 0, 1, 1, 1 },
	{ LANG_ZIG, "Zig", "Zig", 0, 0, 0, 0, 1, 0, 0, 1 },
	{ LANG_CRYSTAL, "Crystal", "Crystal", 1, 0, 0, 1, 1, 0, 1, 1 },

	/* Functional Languages */
	{ LANG_HASKELL, "Haskell", "Haskell", 0, 0, 0, 0, 0, 1, 1, 1 },
	{ LANG_OCAML, "OCaml", "OCaml", 1, 0, 0, 1, 0, 1, 1, 0 },
	{ LANG_PROLOG, "Prolog", "Prolog", 0, 0, 0, 0, 0, 1, 1, 0 },

	/* Other */
	{ LANG_DART, "Dart", "Dart", 1, 1, 0, 1, 1, 0, 1, 1 },
	{ LANG_KOTLIN, "Kotlin", "Kotlin", 1, 0, 0, 1, 1, 0, 1, 1 },
	{ LANG_JAVA, "Java", "Java", 1, 1, 0, 1, 1, 1, 1, 1 },
	{ LANG_CSHARP, "C#", "C#", 1, 1, 1, 1, 1, 1, 1, 1 },

	/* Sentinel */
	{ LANG_UNKNOWN, NULL, NULL, 0, 0, 0, 0, 0, 0, 0, 0 }
};

/*
 * Set the current source language
 */
void
debugsym_set_language(debug_language_t lang)
{
	current_language = lang;
}

/*
 * Get the current source language
 */
debug_language_t
debugsym_get_language(void)
{
	return current_language;
}

/*
 * Get the name of a language
 */
const char *
debugsym_language_name(debug_language_t lang)
{
	int i;

	for (i = 0; language_table[i].language != LANG_UNKNOWN; i++) {
		if (language_table[i].language == lang)
			return language_table[i].name;
	}

	return "Unknown";
}

/*
 * Get detailed information about a language
 */
const language_info_t *
debugsym_language_info(debug_language_t lang)
{
	int i;

	for (i = 0; language_table[i].language != LANG_UNKNOWN; i++) {
		if (language_table[i].language == lang)
			return &language_table[i];
	}

	return NULL;
}

/*
 * C++ Language Support
 */

debug_symbol_t *
debugsym_new_cpp_class(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_STRUCT;  /* Use struct as base, mark as class */
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_CPP_CLASS;

	return sym;
}

debug_symbol_t *
debugsym_new_cpp_namespace(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_NAMESPACE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_CPP_NAMESPACE;

	return sym;
}

debug_symbol_t *
debugsym_new_cpp_template(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_TEMPLATE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_CPP_TEMPLATE_TYPE_PARAM;

	return sym;
}

debug_type_t *
debugsym_cpp_class_type(const char *name)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_CLASS;
	type->name = debugsym_strdup(name);

	return type;
}

debug_type_t *
debugsym_cpp_reference_type(debug_type_t *base)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_REFERENCE;
	type->base_type = base;
	type->size = sizeof(void *);

	return type;
}

debug_type_t *
debugsym_cpp_rvalue_reference_type(debug_type_t *base)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_RVALUE_REFERENCE;
	type->base_type = base;
	type->size = sizeof(void *);

	return type;
}

void
debugsym_cpp_set_virtual(debug_symbol_t *sym, int is_virtual)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->cpp.virtual_function = is_virtual;
}

void
debugsym_cpp_set_mangled_name(debug_symbol_t *sym, const char *mangled)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->cpp.mangled_name = debugsym_strdup(mangled);
}

/*
 * Fortran Language Support
 */

debug_symbol_t *
debugsym_new_fortran_module(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_MODULE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_FORTRAN_MODULE;

	return sym;
}

debug_symbol_t *
debugsym_new_fortran_common(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_FORTRAN_COMMON_BLOCK;

	return sym;
}

debug_symbol_t *
debugsym_new_fortran_array(const char *name, int dimensions)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);

	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->fortran.dimensions = dimensions;

	return sym;
}

debug_type_t *
debugsym_fortran_array_type(debug_type_t *base, int dims, int *lower, int *upper)
{
	debug_type_t *type = debugsym_new_type();
	int i;

	type->encoding = DBGTYPE_ARRAY;
	type->base_type = base;
	type->array_dimensions = dims;

	if (type->array_dimensions > 0) {
		type->array_sizes = calloc(dims, sizeof(int));
		for (i = 0; i < dims; i++) {
			type->array_sizes[i] = upper[i] - lower[i] + 1;
		}
	}

	return type;
}

void
debugsym_fortran_set_array_bounds(debug_symbol_t *sym, int dim, int lower, int upper)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	if (dim >= 0 && dim < 7) {
		sym->lang_attrs->fortran.array_lower_bound[dim] = lower;
		sym->lang_attrs->fortran.array_upper_bound[dim] = upper;
	}
}

void
debugsym_fortran_set_intent(debug_symbol_t *sym, int intent)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->fortran.intent = intent;
}

/*
 * Go Language Support
 */

debug_symbol_t *
debugsym_new_go_interface(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_INTERFACE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_GO_INTERFACE;

	return sym;
}

debug_symbol_t *
debugsym_new_go_channel(const char *name, int direction)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_GO_CHANNEL;

	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->go.channel_direction = direction;

	return sym;
}

debug_symbol_t *
debugsym_new_go_slice(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_GO_SLICE;

	return sym;
}

debug_type_t *
debugsym_go_slice_type(debug_type_t *element)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_ARRAY;  /* Base on array */
	type->base_type = element;
	type->size = 24;  /* Go slice header: ptr + len + cap */

	return type;
}

debug_type_t *
debugsym_go_map_type(debug_type_t *key, debug_type_t *value)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_STRUCT;  /* Represent as struct */
	type->size = sizeof(void *);  /* Pointer to runtime map */

	return type;
}

debug_type_t *
debugsym_go_channel_type(debug_type_t *element, int direction)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_POINTER;
	type->base_type = element;
	type->size = sizeof(void *);

	return type;
}

void
debugsym_go_set_goroutine(debug_symbol_t *sym)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->go.is_goroutine = 1;
}

/*
 * Rust Language Support
 */

debug_symbol_t *
debugsym_new_rust_trait(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_INTERFACE;  /* Similar to interface */
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_RUST_TRAIT;

	return sym;
}

debug_symbol_t *
debugsym_new_rust_lifetime(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_TYPE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_RUST_LIFETIME;

	return sym;
}

debug_type_t *
debugsym_rust_option_type(debug_type_t *inner)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_UNION;  /* Option is an enum/union */
	type->base_type = inner;
	type->name = debugsym_strdup("Option");

	return type;
}

debug_type_t *
debugsym_rust_result_type(debug_type_t *ok, debug_type_t *err)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_UNION;  /* Result is an enum/union */
	type->name = debugsym_strdup("Result");

	return type;
}

void
debugsym_rust_set_lifetime(debug_symbol_t *sym, const char *lifetime)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->rust.lifetime = debugsym_strdup(lifetime);
}

/*
 * Objective-C Language Support
 */

debug_symbol_t *
debugsym_new_objc_protocol(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_INTERFACE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_OBJC_PROTOCOL;

	return sym;
}

debug_symbol_t *
debugsym_new_objc_category(const char *class_name, const char *category_name)
{
	debug_symbol_t *sym = debugsym_new_symbol();
	char *full_name;

	sym->kind = DBGSYM_STRUCT;
	sym->language_tag = LANGTAG_OBJC_CATEGORY;

	/* Create name as "ClassName(CategoryName)" */
	full_name = malloc(strlen(class_name) + strlen(category_name) + 3);
	sprintf(full_name, "%s(%s)", class_name, category_name);
	sym->name = full_name;

	return sym;
}

debug_symbol_t *
debugsym_new_objc_property(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_OBJC_PROPERTY;

	return sym;
}

void
debugsym_objc_set_selector(debug_symbol_t *sym, const char *selector)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->objc.selector = debugsym_strdup(selector);
}

/*
 * Ada Language Support
 */

debug_symbol_t *
debugsym_new_ada_package(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_MODULE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_ADA_PACKAGE;

	return sym;
}

debug_symbol_t *
debugsym_new_ada_task(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_FUNCTION;  /* Task is like a thread function */
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_ADA_TASK;

	return sym;
}

void
debugsym_ada_set_package(debug_symbol_t *sym, const char *package)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->ada.package_name = debugsym_strdup(package);
}

/*
 * Pascal/Modula/Oberon Language Support
 */

debug_type_t *
debugsym_pascal_set_type(debug_type_t *base)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_SET;
	type->base_type = base;
	type->size = (base->size + 7) / 8;  /* Bit set */

	return type;
}

debug_type_t *
debugsym_pascal_subrange_type(debug_type_t *base, int min, int max)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_SUBRANGE;
	type->base_type = base;
	type->size = base->size;

	return type;
}

/*
 * Haskell Language Support
 */

debug_symbol_t *
debugsym_new_haskell_typeclass(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_INTERFACE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_HASKELL_TYPECLASS;

	return sym;
}

/*
 * OCaml Language Support
 */

debug_symbol_t *
debugsym_new_ocaml_variant(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_UNION;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_OCAML_VARIANT;

	return sym;
}

/*
 * COBOL Language Support
 */

debug_symbol_t *
debugsym_new_cobol_paragraph(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_FUNCTION;  /* Paragraph is like a function */
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_COBOL_PARAGRAPH;

	return sym;
}

debug_symbol_t *
debugsym_new_cobol_fd(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_COBOL_FD;

	return sym;
}

void
debugsym_cobol_set_picture(debug_symbol_t *sym, const char *picture)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->cobol.picture_clause = debugsym_strdup(picture);
}

void
debugsym_cobol_set_level(debug_symbol_t *sym, int level)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->cobol.level_number = level;
}

/*
 * Zig Language Support
 */

debug_symbol_t *
debugsym_new_zig_comptime(const char *name)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_CONSTANT;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_ZIG_COMPTIME;

	return sym;
}

debug_symbol_t *
debugsym_new_zig_error_union(const char *name, const char *error_set)
{
	debug_symbol_t *sym = debugsym_new_symbol();

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(name);
	sym->language_tag = LANGTAG_ZIG_ERROR_UNION;

	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->zig.error_set = debugsym_strdup(error_set);

	return sym;
}

debug_type_t *
debugsym_zig_optional_type(debug_type_t *child)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_UNION;  /* Optional is a tagged union */
	type->base_type = child;
	type->size = child->size + 1;  /* Null tag + value */

	return type;
}

debug_type_t *
debugsym_zig_error_union_type(debug_type_t *payload, const char *error_set)
{
	debug_type_t *type = debugsym_new_type();

	type->encoding = DBGTYPE_UNION;  /* Error union is a tagged union */
	type->base_type = payload;
	type->name = debugsym_strdup(error_set);

	return type;
}

void
debugsym_zig_set_comptime(debug_symbol_t *sym)
{
	if (!sym->lang_attrs)
		sym->lang_attrs = calloc(1, sizeof(language_attributes_t));

	sym->lang_attrs->zig.is_comptime = 1;
}

/*
 * Emit language-specific information to debug output
 * This is called during finalization to write language metadata
 */
void
debugsym_emit_language_info(void)
{
	const language_info_t *info = debugsym_language_info(current_language);

	if (!info)
		return;

	/* Language information is embedded in the compilation unit DIE */
	/* This is handled by the format-specific backends */
}
