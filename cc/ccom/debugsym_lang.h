/*
 * Language-Specific Debug Symbol Support
 *
 * This file provides language-specific extensions for the universal
 * debug symbol system, supporting 22+ programming languages across
 * all debug formats.
 */

#ifndef DEBUGSYM_LANG_H
#define DEBUGSYM_LANG_H

/*
 * Language Enumeration
 * Covers all major programming languages from 1950s to 2020s
 */
typedef enum {
	/* C Family */
	LANG_C89 = 0x0001,
	LANG_C99 = 0x000c,
	LANG_C11 = 0x001d,
	LANG_C17 = 0x002c,
	LANG_C23 = 0x0040,  /* Future C23 */

	/* C++ Family */
	LANG_CPP = 0x0004,
	LANG_CPP_03 = 0x0019,
	LANG_CPP_11 = 0x001a,
	LANG_CPP_14 = 0x0021,
	LANG_CPP_17 = 0x002a,
	LANG_CPP_20 = 0x002b,
	LANG_CPP_23 = 0x0041,  /* Future C++23 */

	/* Objective-C Family */
	LANG_OBJC = 0x0010,
	LANG_OBJCPP = 0x0011,

	/* Fortran Family */
	LANG_FORTRAN77 = 0x0007,
	LANG_FORTRAN90 = 0x0008,
	LANG_FORTRAN95 = 0x000e,
	LANG_FORTRAN03 = 0x0022,
	LANG_FORTRAN08 = 0x0023,
	LANG_FORTRAN18 = 0x002d,

	/* Pascal Family */
	LANG_PASCAL83 = 0x0009,
	LANG_MODULA2 = 0x000a,
	LANG_MODULA3 = 0x0017,
	LANG_OBERON = 0x0042,  /* Custom */

	/* Ada Family */
	LANG_ADA83 = 0x0003,
	LANG_ADA95 = 0x000d,
	LANG_ADA05 = 0x002e,
	LANG_ADA12 = 0x002f,

	/* COBOL Family */
	LANG_COBOL74 = 0x0005,
	LANG_COBOL85 = 0x0006,
	LANG_COBOL02 = 0x0043,  /* Custom */

	/* Modern Systems Languages */
	LANG_RUST = 0x001c,
	LANG_GO = 0x0016,
	LANG_D = 0x0013,
	LANG_ZIG = 0x0027,
	LANG_CRYSTAL = 0x0028,

	/* Functional Languages */
	LANG_HASKELL = 0x0018,
	LANG_OCAML = 0x001b,
	LANG_PROLOG = 0x0044,  /* Custom */

	/* Other */
	LANG_DART = 0x0045,  /* Custom */
	LANG_KOTLIN = 0x0026,
	LANG_JAVA = 0x000b,
	LANG_CSHARP = 0x0046,  /* Custom */

	/* Marker */
	LANG_UNKNOWN = 0x0000
} debug_language_t;

/*
 * Language-Specific Type Tags
 * These extend the base debug_symbol_kind_t
 */
typedef enum {
	/* C++ Specific */
	LANGTAG_CPP_CLASS = 0x1000,
	LANGTAG_CPP_NAMESPACE,
	LANGTAG_CPP_TEMPLATE_TYPE_PARAM,
	LANGTAG_CPP_TEMPLATE_VALUE_PARAM,
	LANGTAG_CPP_INHERITANCE,
	LANGTAG_CPP_FRIEND,
	LANGTAG_CPP_PTR_TO_MEMBER,
	LANGTAG_CPP_REFERENCE,
	LANGTAG_CPP_RVALUE_REFERENCE,
	LANGTAG_CPP_VTABLE,
	LANGTAG_CPP_LAMBDA,

	/* Fortran Specific */
	LANGTAG_FORTRAN_MODULE = 0x2000,
	LANGTAG_FORTRAN_SUBMODULE,
	LANGTAG_FORTRAN_COMMON_BLOCK,
	LANGTAG_FORTRAN_NAMELIST,
	LANGTAG_FORTRAN_ARRAY_SECTION,
	LANGTAG_FORTRAN_COARRAY,

	/* Go Specific */
	LANGTAG_GO_INTERFACE = 0x3000,
	LANGTAG_GO_CHANNEL,
	LANGTAG_GO_SLICE,
	LANGTAG_GO_MAP,
	LANGTAG_GO_GOROUTINE,
	LANGTAG_GO_DEFER,

	/* Rust Specific */
	LANGTAG_RUST_TRAIT = 0x4000,
	LANGTAG_RUST_LIFETIME,
	LANGTAG_RUST_IMPL,
	LANGTAG_RUST_ENUM_VARIANT,
	LANGTAG_RUST_MACRO,
	LANGTAG_RUST_UNSAFE_BLOCK,

	/* Objective-C Specific */
	LANGTAG_OBJC_PROTOCOL = 0x5000,
	LANGTAG_OBJC_CATEGORY,
	LANGTAG_OBJC_PROPERTY,
	LANGTAG_OBJC_SELECTOR,
	LANGTAG_OBJC_BLOCK,

	/* Ada Specific */
	LANGTAG_ADA_PACKAGE = 0x6000,
	LANGTAG_ADA_PACKAGE_BODY,
	LANGTAG_ADA_TASK,
	LANGTAG_ADA_PROTECTED,
	LANGTAG_ADA_GENERIC,
	LANGTAG_ADA_DISCRIMINANT,

	/* Pascal/Modula Specific */
	LANGTAG_PASCAL_SET = 0x7000,
	LANGTAG_PASCAL_SUBRANGE,
	LANGTAG_MODULA_MODULE,
	LANGTAG_MODULA_OPAQUE_TYPE,
	LANGTAG_OBERON_TYPE_EXTENSION,

	/* Haskell Specific */
	LANGTAG_HASKELL_TYPECLASS = 0x8000,
	LANGTAG_HASKELL_INSTANCE,
	LANGTAG_HASKELL_THUNK,
	LANGTAG_HASKELL_NEWTYPE,

	/* OCaml Specific */
	LANGTAG_OCAML_VARIANT = 0x9000,
	LANGTAG_OCAML_MODULE,
	LANGTAG_OCAML_FUNCTOR,
	LANGTAG_OCAML_GADT,

	/* COBOL Specific */
	LANGTAG_COBOL_PARAGRAPH = 0xa000,
	LANGTAG_COBOL_SECTION,
	LANGTAG_COBOL_FD,
	LANGTAG_COBOL_PICTURE,
	LANGTAG_COBOL_OCCURS,

	/* D Specific */
	LANGTAG_D_TEMPLATE = 0xb000,
	LANGTAG_D_MIXIN,
	LANGTAG_D_UNITTEST,
	LANGTAG_D_CONTRACT,

	/* Zig Specific */
	LANGTAG_ZIG_COMPTIME = 0xc000,
	LANGTAG_ZIG_ERROR_UNION,
	LANGTAG_ZIG_OPTIONAL,
	LANGTAG_ZIG_PACKED_STRUCT,

	/* Crystal Specific */
	LANGTAG_CRYSTAL_UNION = 0xd000,
	LANGTAG_CRYSTAL_MACRO,
	LANGTAG_CRYSTAL_FIBER,

	/* Dart Specific */
	LANGTAG_DART_ASYNC = 0xe000,
	LANGTAG_DART_ISOLATE,
	LANGTAG_DART_MIXIN,
	LANGTAG_DART_EXTENSION
} debug_language_tag_t;

/*
 * Language-Specific Attributes
 */
typedef struct {
	/* C++ */
	struct {
		int virtual_function;
		int pure_virtual;
		int const_method;
		int static_method;
		int explicit_constructor;
		int deleted_function;
		int defaulted_function;
		int noexcept;
		char *mangled_name;
		void *vtable_ptr;
	} cpp;

	/* Fortran */
	struct {
		int array_lower_bound[7];  /* Up to 7 dimensions */
		int array_upper_bound[7];
		int array_stride[7];
		int dimensions;
		int allocatable;
		int pointer;
		int target;
		int intent;  /* IN, OUT, INOUT */
		int optional;
		int save;
	} fortran;

	/* Go */
	struct {
		int is_goroutine;
		int channel_direction;  /* Send, Receive, Both */
		int slice_capacity;
		char *interface_name;
		int embedded;
	} go;

	/* Rust */
	struct {
		char *lifetime;
		int is_mut;
		int is_ref;
		int ownership_moved;
		char *trait_name;
		int unsafe_block;
	} rust;

	/* Objective-C */
	struct {
		char *selector;
		int is_class_method;
		int is_instance_method;
		int is_property;
		int property_readonly;
		int property_atomic;
		char *protocol_name;
	} objc;

	/* Ada */
	struct {
		int is_task;
		int is_protected;
		int is_generic;
		char *package_name;
		int discriminant_count;
		char **discriminant_names;
	} ada;

	/* Haskell */
	struct {
		int is_lazy;
		int is_strict;
		char *typeclass;
		int higher_kinded;
	} haskell;

	/* COBOL */
	struct {
		int level_number;
		char *picture_clause;
		int occurs_count;
		int redefines;
	} cobol;

	/* Zig */
	struct {
		int is_comptime;
		int is_error_union;
		char *error_set;
		int is_optional;
	} zig;

} language_attributes_t;

/*
 * Language Support Information
 */
typedef struct {
	debug_language_t language;
	const char *name;
	const char *description;
	int has_classes;
	int has_templates;
	int has_namespaces;
	int has_exceptions;
	int has_generics;
	int has_modules;
	int has_gc;
	int has_concurrency;
} language_info_t;

/* Function Prototypes */

/* Language detection and selection */
void debugsym_set_language(debug_language_t lang);
debug_language_t debugsym_get_language(void);
const char *debugsym_language_name(debug_language_t lang);
const language_info_t *debugsym_language_info(debug_language_t lang);

/* Language-specific symbol creation */
debug_symbol_t *debugsym_new_cpp_class(const char *name);
debug_symbol_t *debugsym_new_cpp_namespace(const char *name);
debug_symbol_t *debugsym_new_cpp_template(const char *name);

debug_symbol_t *debugsym_new_fortran_module(const char *name);
debug_symbol_t *debugsym_new_fortran_common(const char *name);
debug_symbol_t *debugsym_new_fortran_array(const char *name, int dimensions);

debug_symbol_t *debugsym_new_go_interface(const char *name);
debug_symbol_t *debugsym_new_go_channel(const char *name, int direction);
debug_symbol_t *debugsym_new_go_slice(const char *name);

debug_symbol_t *debugsym_new_rust_trait(const char *name);
debug_symbol_t *debugsym_new_rust_lifetime(const char *name);

debug_symbol_t *debugsym_new_objc_protocol(const char *name);
debug_symbol_t *debugsym_new_objc_category(const char *class_name, const char *category_name);
debug_symbol_t *debugsym_new_objc_property(const char *name);

debug_symbol_t *debugsym_new_ada_package(const char *name);
debug_symbol_t *debugsym_new_ada_task(const char *name);

debug_symbol_t *debugsym_new_haskell_typeclass(const char *name);
debug_symbol_t *debugsym_new_ocaml_variant(const char *name);

debug_symbol_t *debugsym_new_cobol_paragraph(const char *name);
debug_symbol_t *debugsym_new_cobol_fd(const char *name);

debug_symbol_t *debugsym_new_zig_comptime(const char *name);
debug_symbol_t *debugsym_new_zig_error_union(const char *name, const char *error_set);

/* Language-specific type creation */
debug_type_t *debugsym_cpp_class_type(const char *name);
debug_type_t *debugsym_cpp_reference_type(debug_type_t *base);
debug_type_t *debugsym_cpp_rvalue_reference_type(debug_type_t *base);

debug_type_t *debugsym_fortran_array_type(debug_type_t *base, int dims, int *lower, int *upper);
debug_type_t *debugsym_go_slice_type(debug_type_t *element);
debug_type_t *debugsym_go_map_type(debug_type_t *key, debug_type_t *value);
debug_type_t *debugsym_go_channel_type(debug_type_t *element, int direction);

debug_type_t *debugsym_rust_option_type(debug_type_t *inner);
debug_type_t *debugsym_rust_result_type(debug_type_t *ok, debug_type_t *err);

debug_type_t *debugsym_pascal_set_type(debug_type_t *base);
debug_type_t *debugsym_pascal_subrange_type(debug_type_t *base, int min, int max);

debug_type_t *debugsym_zig_optional_type(debug_type_t *child);
debug_type_t *debugsym_zig_error_union_type(debug_type_t *payload, const char *error_set);

/* Language-specific attribute setters */
void debugsym_cpp_set_virtual(debug_symbol_t *sym, int is_virtual);
void debugsym_cpp_set_mangled_name(debug_symbol_t *sym, const char *mangled);

void debugsym_fortran_set_array_bounds(debug_symbol_t *sym, int dim, int lower, int upper);
void debugsym_fortran_set_intent(debug_symbol_t *sym, int intent);

void debugsym_go_set_goroutine(debug_symbol_t *sym);
void debugsym_rust_set_lifetime(debug_symbol_t *sym, const char *lifetime);
void debugsym_objc_set_selector(debug_symbol_t *sym, const char *selector);
void debugsym_ada_set_package(debug_symbol_t *sym, const char *package);

void debugsym_cobol_set_picture(debug_symbol_t *sym, const char *picture);
void debugsym_cobol_set_level(debug_symbol_t *sym, int level);

void debugsym_zig_set_comptime(debug_symbol_t *sym);

/* Emit language-specific extensions to debug formats */
void debugsym_emit_language_info(void);

/* Language table */
extern const language_info_t language_table[];

#endif /* DEBUGSYM_LANG_H */
