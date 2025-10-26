/*
 * C# Type System Implementation
 * Handles type creation, inference, and compatibility
 */

#include <stdlib.h>
#include <string.h>
#include "cs_pass1.h"

/* Global state */
int cs_arc_enabled = 1;  /* ARC enabled by default for C# */
int cs_language_version = CS_VERSION_30;
struct cs_symbol *cs_current_scope = NULL;

/* Symbol table scope stack */
static struct cs_scope {
	struct cs_symbol *symbols;
	struct cs_scope *parent;
	int level;
} *scope_stack = NULL;

/* Initialize C# compiler */
void cs_init(void) {
	cs_arc_enabled = 1;
	cs_language_version = CS_VERSION_30;
	cs_current_scope = NULL;
	scope_stack = NULL;

	/* Initialize root scope */
	cs_sym_push_scope();

	/* Initialize ARC for C# */
	cs_arc_init();
}

void cs_cleanup(void) {
	while (scope_stack) {
		cs_sym_pop_scope();
	}
}

/* ========== Symbol Table Implementation ========== */

void cs_sym_push_scope(void) {
	struct cs_scope *new_scope = calloc(1, sizeof(struct cs_scope));
	new_scope->parent = scope_stack;
	new_scope->level = scope_stack ? scope_stack->level + 1 : 0;
	new_scope->symbols = NULL;
	scope_stack = new_scope;
}

void cs_sym_pop_scope(void) {
	if (!scope_stack)
		return;

	struct cs_scope *old_scope = scope_stack;
	scope_stack = scope_stack->parent;

	/* Free symbols in this scope */
	struct cs_symbol *sym = old_scope->symbols;
	while (sym) {
		struct cs_symbol *next = sym->next;
		free(sym->name);
		if (sym->type_info)
			free(sym->type_info);
		free(sym);
		sym = next;
	}

	free(old_scope);
}

struct cs_symbol *cs_sym_lookup(const char *name) {
	struct cs_scope *scope = scope_stack;

	while (scope) {
		struct cs_symbol *sym = scope->symbols;
		while (sym) {
			if (strcmp(sym->name, name) == 0)
				return sym;
			sym = sym->next;
		}
		scope = scope->parent;
	}

	return NULL;
}

struct cs_symbol *cs_sym_insert(const char *name, enum cs_type_kind type) {
	if (!scope_stack)
		cs_sym_push_scope();

	struct cs_symbol *sym = calloc(1, sizeof(struct cs_symbol));
	sym->name = strdup(name);
	sym->type = type;
	sym->modifiers = 0;
	sym->parent = NULL;
	sym->type_info = NULL;
	sym->line = 0;
	sym->column = 0;

	/* Determine if reference type (for ARC) */
	sym->is_reference_type = cs_type_is_reference_kind(type);
	sym->arc_qualifier = 0;  /* Default: strong */

	/* Add to current scope */
	sym->next = scope_stack->symbols;
	scope_stack->symbols = sym;

	/* Notify ARC if reference type */
	if (cs_arc_enabled && sym->is_reference_type) {
		cs_arc_var_declared(sym);
	}

	return sym;
}

/* ========== Type System Implementation ========== */

static int cs_type_is_reference_kind(enum cs_type_kind kind) {
	switch (kind) {
	case CS_TYPE_STRING:
	case CS_TYPE_OBJECT:
	case CS_TYPE_CLASS:
	case CS_TYPE_INTERFACE:
	case CS_TYPE_DELEGATE:
	case CS_TYPE_ARRAY:
	case CS_TYPE_ANONYMOUS:
		return 1;
	default:
		return 0;
	}
}

struct cs_type_info *cs_type_create(enum cs_type_kind kind) {
	struct cs_type_info *type = calloc(1, sizeof(struct cs_type_info));
	type->kind = kind;
	type->name = NULL;
	type->members = NULL;
	type->type_params = NULL;
	type->base_type = NULL;
	type->interfaces = NULL;
	type->interface_count = 0;

	/* Set default sizes (architecture-neutral logical sizes) */
	switch (kind) {
	case CS_TYPE_BOOL:
		type->size = 1;
		type->alignment = 1;
		type->is_blittable = 1;
		break;
	case CS_TYPE_BYTE:
	case CS_TYPE_SBYTE:
		type->size = 1;
		type->alignment = 1;
		type->is_blittable = 1;
		break;
	case CS_TYPE_SHORT:
	case CS_TYPE_USHORT:
	case CS_TYPE_CHAR:
		type->size = 2;
		type->alignment = 2;
		type->is_blittable = 1;
		break;
	case CS_TYPE_INT:
	case CS_TYPE_UINT:
	case CS_TYPE_FLOAT:
		type->size = 4;
		type->alignment = 4;
		type->is_blittable = 1;
		break;
	case CS_TYPE_LONG:
	case CS_TYPE_ULONG:
	case CS_TYPE_DOUBLE:
		type->size = 8;
		type->alignment = 8;
		type->is_blittable = 1;
		break;
	case CS_TYPE_DECIMAL:
		type->size = 16;
		type->alignment = 8;
		type->is_blittable = 1;
		break;
	case CS_TYPE_STRING:
	case CS_TYPE_OBJECT:
	case CS_TYPE_CLASS:
	case CS_TYPE_INTERFACE:
	case CS_TYPE_DELEGATE:
	case CS_TYPE_ARRAY:
		/* Reference types: size is pointer size (architecture-dependent) */
		type->size = 0;  /* 0 indicates reference type */
		type->alignment = 0;
		type->is_blittable = 0;
		break;
	default:
		type->size = 0;
		type->alignment = 0;
		type->is_blittable = 0;
		break;
	}

	return type;
}

int cs_type_compatible(struct cs_type_info *t1, struct cs_type_info *t2) {
	if (!t1 || !t2)
		return 0;

	/* Exact match */
	if (t1->kind == t2->kind)
		return 1;

	/* Object is compatible with everything (base class) */
	if (t1->kind == CS_TYPE_OBJECT || t2->kind == CS_TYPE_OBJECT)
		return 1;

	/* Check inheritance chain */
	struct cs_type_info *base = t2->base_type;
	while (base) {
		if (base->kind == t1->kind)
			return 1;
		base = base->base_type;
	}

	/* Check interface implementation */
	for (int i = 0; i < t2->interface_count; i++) {
		if (t2->interfaces[i]->kind == t1->kind)
			return 1;
	}

	/* Numeric type promotions */
	if ((t1->kind == CS_TYPE_INT && t2->kind == CS_TYPE_LONG) ||
	    (t1->kind == CS_TYPE_FLOAT && t2->kind == CS_TYPE_DOUBLE))
		return 1;

	return 0;
}

int cs_type_is_reference(struct cs_type_info *type) {
	return type && type->size == 0;
}

int cs_type_is_value(struct cs_type_info *type) {
	return type && type->size > 0;
}

/* Type inference for 'var' keyword (C# 3.0) */
struct cs_type_info *cs_type_infer(P1ND *expr) {
	if (!expr)
		return NULL;

	/* This would analyze the expression and determine its type */
	/* Simplified implementation - real version would be more complex */

	/* For demonstration, return int type */
	return cs_type_create(CS_TYPE_INT);
}

/* ========== Lambda Expression Support ========== */

struct cs_lambda *cs_lambda_create(void) {
	struct cs_lambda *lambda = calloc(1, sizeof(struct cs_lambda));
	lambda->param_names = NULL;
	lambda->param_count = 0;
	lambda->param_types = NULL;
	lambda->return_type = NULL;
	lambda->body = NULL;
	lambda->is_expression = 1;
	return lambda;
}

P1ND *cs_lambda_compile(struct cs_lambda *lambda) {
	if (!lambda)
		return NULL;

	/* Generate delegate type and method body */
	/* This would create an anonymous method that implements the lambda */
	/* Simplified - real implementation would be more complex */

	return NULL;  /* Placeholder */
}

/* ========== LINQ Query Support ========== */

struct cs_query *cs_query_create(void) {
	struct cs_query *query = calloc(1, sizeof(struct cs_query));
	return query;
}

P1ND *cs_query_compile(struct cs_query *query) {
	if (!query)
		return NULL;

	/* Compile LINQ query to method chain calls */
	/* This would transform: from x in data where x > 10 select x */
	/* Into: data.Where(x => x > 10).Select(x => x) */

	return NULL;  /* Placeholder */
}

/* ========== Anonymous Type Support ========== */

struct cs_anonymous_type *cs_anon_type_create(void) {
	struct cs_anonymous_type *type = calloc(1,
	                                        sizeof(struct cs_anonymous_type));
	type->field_names = NULL;
	type->field_types = NULL;
	type->field_count = 0;
	type->generated_name = NULL;
	return type;
}

char *cs_anon_type_generate_name(struct cs_anonymous_type *type) {
	/* Generate unique name like: <>f__AnonymousType0`2 */
	static int anon_type_counter = 0;
	char buffer[128];
	snprintf(buffer, sizeof(buffer), "<>f__AnonymousType%d`%d",
	         anon_type_counter++, type->field_count);
	return strdup(buffer);
}

/* ========== Extension Method Support ========== */

int cs_is_extension_method(struct cs_symbol *method) {
	if (!method)
		return 0;
	/* Check if method is marked as extension method */
	/* Would check for [Extension] attribute and static modifier */
	return (method->modifiers & CS_MOD_STATIC) != 0;
}

P1ND *cs_resolve_extension_call(P1ND *receiver, const char *method_name) {
	/* Search for extension methods in using namespaces */
	/* Transform receiver.Method(args) to StaticClass.Method(receiver, args) */
	return NULL;  /* Placeholder */
}

/* ========== Object Initializer Support ========== */

P1ND *cs_handle_object_initializer(P1ND *obj, P1ND *init_list) {
	/* Generate property/field assignments */
	/* Transform: new Person { Name = "Alice", Age = 30 } */
	/* Into: temp = new Person(); temp.Name = "Alice"; temp.Age = 30; */
	return NULL;  /* Placeholder */
}

P1ND *cs_handle_collection_initializer(P1ND *collection, P1ND *init_list) {
	/* Generate Add() calls */
	/* Transform: new List<int> { 1, 2, 3 } */
	/* Into: temp = new List<int>(); temp.Add(1); temp.Add(2); temp.Add(3); */
	return NULL;  /* Placeholder */
}

/* ========== Auto-Property Support ========== */

P1ND *cs_generate_auto_property(struct cs_symbol *prop) {
	/* Generate backing field and accessor methods */
	/* public string Name { get; set; } */
	/* Becomes: */
	/* private string <Name>k__BackingField; */
	/* public string get_Name() { return <Name>k__BackingField; } */
	/* public void set_Name(string value) { <Name>k__BackingField = value; } */
	return NULL;  /* Placeholder */
}

/* ========== Nullable Type Support ========== */

struct cs_type_info *cs_make_nullable(struct cs_type_info *base_type) {
	if (!base_type || !cs_type_is_value(base_type))
		return NULL;

	struct cs_type_info *nullable = cs_type_create(CS_TYPE_NULLABLE);
	nullable->base_type = base_type;
	nullable->size = base_type->size + 1;  /* Value + hasValue flag */
	nullable->alignment = base_type->alignment;
	nullable->is_blittable = 0;  /* Nullable<T> is not blittable */

	return nullable;
}

int cs_is_nullable(struct cs_type_info *type) {
	return type && type->kind == CS_TYPE_NULLABLE;
}

/* ========== Partial Type Support ========== */

static struct cs_symbol **partial_types = NULL;
static int partial_type_count = 0;

void cs_register_partial_type(struct cs_symbol *type) {
	partial_types = realloc(partial_types,
	                        (partial_type_count + 1) *
	                        sizeof(struct cs_symbol *));
	partial_types[partial_type_count++] = type;
}

void cs_merge_partial_types(void) {
	/* Merge all partial type declarations into single type */
	/* This would combine members from all partial declarations */
	/* with the same name and namespace */

	for (int i = 0; i < partial_type_count; i++) {
		/* Merge logic here */
	}
}
