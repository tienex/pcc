/*
 * C# Version-Specific Features Implementation
 * Support for C# 1.0 through C# 12.0
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "cs_pass1.h"
#include "cs_version.h"

/* ========== C# 2.0 Features ========== */

/* Generic Type Support */
struct cs_generic_instantiation {
	struct cs_type_info *generic_def;
	struct cs_type_info **type_args;
	int type_arg_count;
	struct cs_type_info *instantiated_type;
};

struct cs_generic_instantiation *cs_generic_instantiate(
    struct cs_type_info *generic_type,
    struct cs_type_info **type_args,
    int arg_count) {

	if (!CS_HAS_GENERICS()) {
		fprintf(stderr, "Error: Generics not available in C# %s\n",
		        cs_version_to_string(cs_language_version));
		return NULL;
	}

	struct cs_generic_instantiation *inst =
	    calloc(1, sizeof(struct cs_generic_instantiation));
	inst->generic_def = generic_type;
	inst->type_args = type_args;
	inst->type_arg_count = arg_count;

	/* Generate instantiated type */
	/* In real implementation, would create new type with substituted parameters */
	inst->instantiated_type = cs_type_create(CS_TYPE_GENERIC);

	return inst;
}

/* Nullable Type Support */
P1ND *cs_nullable_has_value(P1ND *nullable_expr) {
	if (!CS_HAS_NULLABLE()) {
		fprintf(stderr, "Error: Nullable types not available\n");
		return NULL;
	}

	/* Generate: nullable.HasValue */
	return NULL;  /* Placeholder */
}

P1ND *cs_nullable_get_value(P1ND *nullable_expr) {
	if (!CS_HAS_NULLABLE()) {
		fprintf(stderr, "Error: Nullable types not available\n");
		return NULL;
	}

	/* Generate: nullable.Value */
	return NULL;  /* Placeholder */
}

/* Iterator Support (yield return/break) */
struct cs_iterator {
	char *method_name;
	struct cs_type_info *element_type;
	P1ND **yield_points;
	int yield_count;
	int has_yield_break;
};

struct cs_iterator *cs_iterator_create(const char *method_name,
                                        struct cs_type_info *elem_type) {
	if (!CS_HAS(CS_FEAT_ITERATORS)) {
		fprintf(stderr, "Error: Iterators not available\n");
		return NULL;
	}

	struct cs_iterator *iter = calloc(1, sizeof(struct cs_iterator));
	iter->method_name = strdup(method_name);
	iter->element_type = elem_type;
	iter->yield_points = NULL;
	iter->yield_count = 0;
	iter->has_yield_break = 0;

	return iter;
}

P1ND *cs_iterator_yield_return(struct cs_iterator *iter, P1ND *value) {
	/* Transform method into state machine */
	/* yield return value becomes state transition */
	return NULL;  /* Placeholder */
}

P1ND *cs_iterator_yield_break(struct cs_iterator *iter) {
	iter->has_yield_break = 1;
	return NULL;  /* Placeholder */
}

/* Anonymous Methods (C# 2.0) */
struct cs_anonymous_method {
	char **param_names;
	struct cs_type_info **param_types;
	int param_count;
	P1ND *body;
	char *generated_name;
};

struct cs_anonymous_method *cs_anon_method_create(void) {
	if (!CS_HAS(CS_FEAT_ANONYMOUS_METHODS)) {
		fprintf(stderr, "Error: Anonymous methods not available\n");
		return NULL;
	}

	struct cs_anonymous_method *method =
	    calloc(1, sizeof(struct cs_anonymous_method));

	static int anon_counter = 0;
	char buf[64];
	snprintf(buf, sizeof(buf), "<>__AnonMethod%d", anon_counter++);
	method->generated_name = strdup(buf);

	return method;
}

/* ========== C# 4.0 Features ========== */

/* Dynamic Type Support */
struct cs_dynamic_operation {
	enum {
		CS_DYN_INVOKE,
		CS_DYN_GET_MEMBER,
		CS_DYN_SET_MEMBER,
		CS_DYN_GET_INDEX,
		CS_DYN_SET_INDEX,
		CS_DYN_BINARY_OP,
		CS_DYN_UNARY_OP,
	} op_type;
	P1ND *target;
	char *member_name;
	P1ND **args;
	int arg_count;
};

P1ND *cs_dynamic_invoke(P1ND *target, const char *method_name,
                        P1ND **args, int arg_count) {
	if (!CS_HAS_DYNAMIC()) {
		fprintf(stderr, "Error: Dynamic binding not available\n");
		return NULL;
	}

	/* Generate call to DLR (Dynamic Language Runtime) */
	/* Would emit CallSite<> with appropriate binder */
	return NULL;  /* Placeholder */
}

/* Named and Optional Parameters */
struct cs_named_arg {
	char *name;
	P1ND *value;
};

P1ND *cs_call_with_named_args(P1ND *method,
                               struct cs_named_arg *named_args,
                               int named_count,
                               P1ND **positional_args,
                               int positional_count) {
	if (!CS_HAS(CS_FEAT_NAMED_ARGS)) {
		fprintf(stderr, "Error: Named arguments not available\n");
		return NULL;
	}

	/* Reorder arguments based on parameter names */
	/* Fill in optional parameters with default values */
	return NULL;  /* Placeholder */
}

/* ========== C# 5.0 Features ========== */

/* Async/Await Support */
struct cs_async_method {
	char *method_name;
	struct cs_type_info *return_type;
	P1ND **await_points;
	int await_count;
	int is_async;
};

struct cs_async_method *cs_async_method_create(const char *name) {
	if (!CS_HAS_ASYNC()) {
		fprintf(stderr, "Error: async/await not available\n");
		return NULL;
	}

	struct cs_async_method *method =
	    calloc(1, sizeof(struct cs_async_method));
	method->method_name = strdup(name);
	method->is_async = 1;
	method->await_points = NULL;
	method->await_count = 0;

	return method;
}

P1ND *cs_await_expression(P1ND *awaitable_expr) {
	if (!CS_HAS_ASYNC()) {
		fprintf(stderr, "Error: await not available\n");
		return NULL;
	}

	/* Transform to state machine with continuation */
	/* Generate: awaiter = expr.GetAwaiter(); */
	/*           if (!awaiter.IsCompleted) { ... suspend ... } */
	/*           result = awaiter.GetResult(); */
	return NULL;  /* Placeholder */
}

/* ========== C# 6.0 Features ========== */

/* Null-Conditional Operator (?.) */
P1ND *cs_null_conditional(P1ND *object, const char *member) {
	if (!CS_HAS_NULL_CONDITIONAL()) {
		fprintf(stderr, "Error: Null-conditional operator not available\n");
		return NULL;
	}

	/* Transform: obj?.Member */
	/* To: (obj != null) ? obj.Member : null */
	return NULL;  /* Placeholder */
}

/* String Interpolation */
struct cs_interpolation {
	char **parts;
	P1ND **expressions;
	int part_count;
};

P1ND *cs_string_interpolation(struct cs_interpolation *interp) {
	if (!CS_HAS(CS_FEAT_STRING_INTERPOLATION)) {
		fprintf(stderr, "Error: String interpolation not available\n");
		return NULL;
	}

	/* Transform: $"Hello {name}, you are {age} years old" */
	/* To: string.Format("Hello {0}, you are {1} years old", name, age) */
	return NULL;  /* Placeholder */
}

/* Expression-Bodied Members */
P1ND *cs_expression_bodied_member(struct cs_symbol *member, P1ND *expr) {
	if (!CS_HAS(CS_FEAT_EXPRESSION_BODIED)) {
		fprintf(stderr, "Error: Expression-bodied members not available\n");
		return NULL;
	}

	/* Transform: public int Square(int x) => x * x; */
	/* To: public int Square(int x) { return x * x; } */
	return NULL;  /* Placeholder */
}

/* nameof Operator */
P1ND *cs_nameof_operator(P1ND *expr) {
	if (!CS_HAS(CS_FEAT_NAMEOF)) {
		fprintf(stderr, "Error: nameof operator not available\n");
		return NULL;
	}

	/* Get name of expression at compile time */
	/* nameof(person.Age) => "Age" */
	return NULL;  /* Placeholder */
}

/* ========== C# 7.0 Features ========== */

/* Tuple Support */
struct cs_tuple {
	P1ND **elements;
	char **element_names;
	int element_count;
};

struct cs_tuple *cs_tuple_create(P1ND **elements, char **names, int count) {
	if (!CS_HAS_TUPLES()) {
		fprintf(stderr, "Error: Tuples not available\n");
		return NULL;
	}

	struct cs_tuple *tuple = calloc(1, sizeof(struct cs_tuple));
	tuple->elements = elements;
	tuple->element_names = names;
	tuple->element_count = count;

	return tuple;
}

/* Pattern Matching */
enum cs_pattern_type {
	CS_PAT_CONSTANT,
	CS_PAT_TYPE,
	CS_PAT_VAR,
	CS_PAT_DISCARD,
	CS_PAT_PROPERTY,
	CS_PAT_POSITIONAL,
	CS_PAT_RECURSIVE,
};

struct cs_pattern {
	enum cs_pattern_type type;
	union {
		P1ND *constant_value;
		struct cs_type_info *type_pattern;
		char *var_name;
		struct {
			char **property_names;
			struct cs_pattern **property_patterns;
			int property_count;
		} property;
	} data;
};

P1ND *cs_pattern_match(P1ND *expr, struct cs_pattern *pattern) {
	if (!CS_HAS_PATTERN_MATCH()) {
		fprintf(stderr, "Error: Pattern matching not available\n");
		return NULL;
	}

	/* Generate pattern matching code based on pattern type */
	return NULL;  /* Placeholder */
}

/* Out Variables */
P1ND *cs_out_var_declaration(struct cs_type_info *type, const char *name) {
	if (!CS_HAS(CS_FEAT_OUT_VARS)) {
		fprintf(stderr, "Error: Out variables not available\n");
		return NULL;
	}

	/* int.TryParse(str, out int result) */
	/* Declares 'result' inline */
	return NULL;  /* Placeholder */
}

/* Local Functions */
P1ND *cs_local_function(const char *name, P1ND *body) {
	if (!CS_HAS(CS_FEAT_LOCAL_FUNCTIONS)) {
		fprintf(stderr, "Error: Local functions not available\n");
		return NULL;
	}

	/* Function defined inside another function */
	/* Has access to enclosing scope */
	return NULL;  /* Placeholder */
}

/* Ref Returns */
P1ND *cs_ref_return(P1ND *ref_expr) {
	if (!CS_HAS(CS_FEAT_REF_RETURNS)) {
		fprintf(stderr, "Error: Ref returns not available\n");
		return NULL;
	}

	/* ref int Find(int[] arr) => ref arr[0]; */
	return NULL;  /* Placeholder */
}

/* ========== C# 8.0 Features ========== */

/* Nullable Reference Types */
void cs_nullable_refs_enable(void) {
	if (!CS_HAS(CS_FEAT_NULLABLE_REFS)) {
		fprintf(stderr, "Error: Nullable reference types not available\n");
		return;
	}

	/* Enable nullable reference type checking */
	/* string? can be null, string cannot */
}

/* Switch Expressions */
P1ND *cs_switch_expression(P1ND *value, P1ND **arms, int arm_count) {
	if (!CS_HAS(CS_FEAT_SWITCH_EXPRESSIONS)) {
		fprintf(stderr, "Error: Switch expressions not available\n");
		return NULL;
	}

	/* var result = value switch {
	 *     1 => "one",
	 *     2 => "two",
	 *     _ => "other"
	 * }; */
	return NULL;  /* Placeholder */
}

/* Default Interface Methods */
P1ND *cs_default_interface_method(struct cs_symbol *iface,
                                   struct cs_symbol *method) {
	if (!CS_HAS(CS_FEAT_DEFAULT_INTERFACE)) {
		fprintf(stderr, "Error: Default interface methods not available\n");
		return NULL;
	}

	/* interface ILogger {
	 *     void Log(string msg) => Console.WriteLine(msg);
	 * } */
	return NULL;  /* Placeholder */
}

/* Ranges and Indices */
P1ND *cs_range_expression(P1ND *start, P1ND *end) {
	if (!CS_HAS(CS_FEAT_RANGES_INDICES)) {
		fprintf(stderr, "Error: Ranges not available\n");
		return NULL;
	}

	/* var slice = arr[1..5]; */
	/* var fromEnd = arr[^1]; */
	return NULL;  /* Placeholder */
}

/* ========== C# 9.0 Features ========== */

/* Records */
struct cs_record {
	char *name;
	struct cs_symbol **properties;
	int property_count;
	int is_record_struct;
};

struct cs_record *cs_record_create(const char *name, int is_struct) {
	if (!CS_HAS_RECORDS()) {
		fprintf(stderr, "Error: Records not available\n");
		return NULL;
	}

	struct cs_record *rec = calloc(1, sizeof(struct cs_record));
	rec->name = strdup(name);
	rec->is_record_struct = is_struct;

	/* Records automatically generate:
	 * - Value equality
	 * - ToString()
	 * - Deconstruct()
	 * - With expressions */

	return rec;
}

/* Init-Only Setters */
P1ND *cs_init_only_property(struct cs_symbol *prop) {
	if (!CS_HAS(CS_FEAT_INIT_ONLY)) {
		fprintf(stderr, "Error: Init-only setters not available\n");
		return NULL;
	}

	/* public string Name { get; init; } */
	/* Can only be set in object initializer or constructor */
	return NULL;  /* Placeholder */
}

/* Top-Level Statements */
void cs_top_level_statements_enable(void) {
	if (!CS_HAS(CS_FEAT_TOP_LEVEL_STMTS)) {
		fprintf(stderr, "Error: Top-level statements not available\n");
		return;
	}

	/* No Main() method needed, statements at top level */
}

/* With Expressions */
P1ND *cs_with_expression(P1ND *record_expr, P1ND **property_assignments,
                         int assignment_count) {
	if (!CS_HAS_RECORDS()) {
		fprintf(stderr, "Error: With expressions require records\n");
		return NULL;
	}

	/* var person2 = person1 with { Age = 31 }; */
	/* Creates copy with modified properties */
	return NULL;  /* Placeholder */
}

/* ========== C# 10.0 Features ========== */

/* Global Using Directives */
void cs_global_using(const char *namespace_name) {
	if (!CS_HAS(CS_FEAT_GLOBAL_USING)) {
		fprintf(stderr, "Error: Global using not available\n");
		return;
	}

	/* global using System; */
	/* Applies to all files in project */
}

/* File-Scoped Namespaces */
void cs_file_scoped_namespace(const char *namespace_name) {
	if (!CS_HAS(CS_FEAT_FILE_SCOPED_NS)) {
		fprintf(stderr, "Error: File-scoped namespaces not available\n");
		return;
	}

	/* namespace MyNamespace; */
	/* (no braces, applies to whole file) */
}

/* Record Structs */
struct cs_record *cs_record_struct_create(const char *name) {
	if (!CS_HAS(CS_FEAT_RECORD_STRUCTS)) {
		fprintf(stderr, "Error: Record structs not available\n");
		return NULL;
	}

	return cs_record_create(name, 1);
}

/* ========== C# 11.0 Features ========== */

/* Raw String Literals */
P1ND *cs_raw_string_literal(const char *content) {
	if (!CS_HAS(CS_FEAT_RAW_STRINGS)) {
		fprintf(stderr, "Error: Raw string literals not available\n");
		return NULL;
	}

	/* """
	 * This is a raw string
	 * with "quotes" and newlines
	 * """ */
	return NULL;  /* Placeholder */
}

/* Generic Attributes */
P1ND *cs_generic_attribute(struct cs_type_info *attr_type,
                           struct cs_type_info **type_args,
                           int arg_count) {
	if (!CS_HAS(CS_FEAT_GENERIC_ATTRS)) {
		fprintf(stderr, "Error: Generic attributes not available\n");
		return NULL;
	}

	/* [MyAttribute<int>] */
	return NULL;  /* Placeholder */
}

/* Required Members */
P1ND *cs_required_member(struct cs_symbol *member) {
	if (!CS_HAS(CS_FEAT_REQUIRED_MEMBERS)) {
		fprintf(stderr, "Error: Required members not available\n");
		return NULL;
	}

	/* required public string Name { get; set; } */
	/* Must be initialized in object initializer or constructor */
	return NULL;  /* Placeholder */
}

/* ========== C# 12.0 Features ========== */

/* Primary Constructors */
struct cs_primary_constructor {
	char *class_name;
	char **param_names;
	struct cs_type_info **param_types;
	int param_count;
};

struct cs_primary_constructor *cs_primary_ctor_create(const char *class_name,
                                                       char **params,
                                                       struct cs_type_info **types,
                                                       int count) {
	if (!CS_HAS(CS_FEAT_PRIMARY_CTORS)) {
		fprintf(stderr, "Error: Primary constructors not available\n");
		return NULL;
	}

	struct cs_primary_constructor *ctor =
	    calloc(1, sizeof(struct cs_primary_constructor));
	ctor->class_name = strdup(class_name);
	ctor->param_names = params;
	ctor->param_types = types;
	ctor->param_count = count;

	/* class Person(string Name, int Age); */
	/* Parameters become constructor parameters and can be used in members */

	return ctor;
}

/* Collection Expressions */
P1ND *cs_collection_expression(P1ND **elements, int count) {
	if (cs_language_version < CS_VERSION_120) {
		fprintf(stderr, "Error: Collection expressions not available\n");
		return NULL;
	}

	/* int[] numbers = [1, 2, 3, 4, 5]; */
	/* List<string> names = ["Alice", "Bob"]; */
	return NULL;  /* Placeholder */
}

/* ========== Feature Availability Reporting ========== */

void cs_print_available_features(void) {
	printf("C# %s Features:\n", cs_version_to_string(cs_language_version));
	printf("================\n\n");

	if (CS_HAS_GENERICS())
		printf("✓ Generics (C# 2.0)\n");
	if (CS_HAS_NULLABLE())
		printf("✓ Nullable types (C# 2.0)\n");
	if (CS_HAS(CS_FEAT_ITERATORS))
		printf("✓ Iterators with yield (C# 2.0)\n");
	if (CS_HAS_LINQ())
		printf("✓ LINQ queries (C# 3.0)\n");
	if (CS_HAS_LAMBDA())
		printf("✓ Lambda expressions (C# 3.0)\n");
	if (CS_HAS(CS_FEAT_EXTENSION_METHODS))
		printf("✓ Extension methods (C# 3.0)\n");
	if (CS_HAS_DYNAMIC())
		printf("✓ Dynamic binding (C# 4.0)\n");
	if (CS_HAS(CS_FEAT_NAMED_ARGS))
		printf("✓ Named/optional parameters (C# 4.0)\n");
	if (CS_HAS_ASYNC())
		printf("✓ async/await (C# 5.0)\n");
	if (CS_HAS_NULL_CONDITIONAL())
		printf("✓ Null-conditional operators ?. (C# 6.0)\n");
	if (CS_HAS(CS_FEAT_STRING_INTERPOLATION))
		printf("✓ String interpolation $\"\" (C# 6.0)\n");
	if (CS_HAS(CS_FEAT_NAMEOF))
		printf("✓ nameof operator (C# 6.0)\n");
	if (CS_HAS_TUPLES())
		printf("✓ Tuples (C# 7.0)\n");
	if (CS_HAS_PATTERN_MATCH())
		printf("✓ Pattern matching (C# 7.0)\n");
	if (CS_HAS(CS_FEAT_OUT_VARS))
		printf("✓ Out variables (C# 7.0)\n");
	if (CS_HAS(CS_FEAT_LOCAL_FUNCTIONS))
		printf("✓ Local functions (C# 7.0)\n");
	if (CS_HAS(CS_FEAT_BINARY_LITERALS))
		printf("✓ Binary literals 0b (C# 7.0)\n");
	if (CS_HAS(CS_FEAT_DIGIT_SEPARATORS))
		printf("✓ Digit separators _ (C# 7.0)\n");
	if (CS_HAS(CS_FEAT_NULLABLE_REFS))
		printf("✓ Nullable reference types (C# 8.0)\n");
	if (CS_HAS(CS_FEAT_RANGES_INDICES))
		printf("✓ Ranges and indices .. ^ (C# 8.0)\n");
	if (CS_HAS(CS_FEAT_SWITCH_EXPRESSIONS))
		printf("✓ Switch expressions (C# 8.0)\n");
	if (CS_HAS_RECORDS())
		printf("✓ Records (C# 9.0)\n");
	if (CS_HAS(CS_FEAT_INIT_ONLY))
		printf("✓ Init-only setters (C# 9.0)\n");
	if (CS_HAS(CS_FEAT_TOP_LEVEL_STMTS))
		printf("✓ Top-level statements (C# 9.0)\n");
	if (CS_HAS(CS_FEAT_GLOBAL_USING))
		printf("✓ Global using directives (C# 10.0)\n");
	if (CS_HAS(CS_FEAT_FILE_SCOPED_NS))
		printf("✓ File-scoped namespaces (C# 10.0)\n");
	if (CS_HAS(CS_FEAT_RAW_STRINGS))
		printf("✓ Raw string literals (C# 11.0)\n");
	if (CS_HAS(CS_FEAT_REQUIRED_MEMBERS))
		printf("✓ Required members (C# 11.0)\n");
	if (CS_HAS(CS_FEAT_PRIMARY_CTORS))
		printf("✓ Primary constructors (C# 12.0)\n");

	printf("\n");
}
