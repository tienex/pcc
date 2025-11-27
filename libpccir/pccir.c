/*
 * Copyright (c) 2025 PCC Project
 *
 * PCC Intermediate Representation Library - Implementation
 */

#include "pccir.h"
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/*
 * Internal Helpers
 */

static int next_node_id = 1;

static char *strdup_safe(const char *s) {
	if (!s) return NULL;
	return strdup(s);
}

/*
 * Module Operations
 */

ir_module_t *ir_module_create(const char *name, const char *language) {
	ir_module_t *mod = calloc(1, sizeof(ir_module_t));
	if (!mod) return NULL;

	mod->name = strdup_safe(name);
	mod->source_language = strdup_safe(language);
	mod->functions = NULL;
	mod->num_functions = 0;
	mod->globals = NULL;
	mod->num_globals = 0;
	mod->types = NULL;
	mod->num_types = 0;
	mod->imports.names = NULL;
	mod->imports.count = 0;
	mod->exports.names = NULL;
	mod->exports.count = 0;
	mod->metadata = NULL;

	return mod;
}

void ir_module_destroy(ir_module_t *mod) {
	if (!mod) return;

	free((void *)mod->name);
	free((void *)mod->source_language);
	free((void *)mod->target_triple);

	for (size_t i = 0; i < mod->num_functions; i++) {
		ir_function_destroy(mod->functions[i]);
	}
	free(mod->functions);

	for (size_t i = 0; i < mod->num_globals; i++) {
		ir_node_destroy(mod->globals[i]);
	}
	free(mod->globals);

	for (size_t i = 0; i < mod->num_types; i++) {
		free(mod->types[i]);
	}
	free(mod->types);

	for (size_t i = 0; i < mod->imports.count; i++) {
		free((void *)mod->imports.names[i]);
	}
	free(mod->imports.names);

	for (size_t i = 0; i < mod->exports.count; i++) {
		free((void *)mod->exports.names[i]);
	}
	free(mod->exports.names);

	free(mod);
}

int ir_module_add_function(ir_module_t *mod, ir_function_t *func) {
	if (!mod || !func) return -1;

	mod->functions = realloc(mod->functions, (mod->num_functions + 1) * sizeof(ir_function_t *));
	if (!mod->functions) return -1;

	mod->functions[mod->num_functions++] = func;
	return 0;
}

int ir_module_add_global(ir_module_t *mod, ir_node_t *global) {
	if (!mod || !global) return -1;

	mod->globals = realloc(mod->globals, (mod->num_globals + 1) * sizeof(ir_node_t *));
	if (!mod->globals) return -1;

	mod->globals[mod->num_globals++] = global;
	return 0;
}

int ir_module_add_type(ir_module_t *mod, ir_type_t *type) {
	if (!mod || !type) return -1;

	mod->types = realloc(mod->types, (mod->num_types + 1) * sizeof(ir_type_t *));
	if (!mod->types) return -1;

	mod->types[mod->num_types++] = type;
	return 0;
}

int ir_module_add_import(ir_module_t *mod, const char *name) {
	if (!mod || !name) return -1;

	mod->imports.names = realloc(mod->imports.names,
	                             (mod->imports.count + 1) * sizeof(const char *));
	if (!mod->imports.names) return -1;

	mod->imports.names[mod->imports.count++] = strdup(name);
	return 0;
}

int ir_module_add_export(ir_module_t *mod, const char *name) {
	if (!mod || !name) return -1;

	mod->exports.names = realloc(mod->exports.names,
	                             (mod->exports.count + 1) * sizeof(const char *));
	if (!mod->exports.names) return -1;

	mod->exports.names[mod->exports.count++] = strdup(name);
	return 0;
}

void ir_module_set_target(ir_module_t *mod, const char *triple) {
	if (!mod) return;
	free((void *)mod->target_triple);
	mod->target_triple = strdup_safe(triple);
}

void ir_module_set_metadata(ir_module_t *mod, void *metadata) {
	if (!mod) return;
	mod->metadata = metadata;
}

/*
 * Function Operations
 */

ir_function_t *ir_function_create(const char *name, ir_type_t *type) {
	ir_function_t *func = calloc(1, sizeof(ir_function_t));
	if (!func) return NULL;

	func->name = strdup_safe(name);
	func->mangled_name = NULL;
	func->type = type;
	func->storage = IR_STORAGE_AUTO;
	func->linkage = IR_LINKAGE_EXTERNAL;
	func->callconv = IR_CC_C;
	func->params = NULL;
	func->num_params = 0;
	func->blocks = NULL;
	func->num_blocks = 0;
	func->is_inline = 0;
	func->is_noreturn = 0;
	func->is_pure = 0;
	func->is_const = 0;

	return func;
}

void ir_function_destroy(ir_function_t *func) {
	if (!func) return;

	free((void *)func->name);
	free((void *)func->mangled_name);

	for (size_t i = 0; i < func->num_params; i++) {
		ir_node_destroy(func->params[i]);
	}
	free(func->params);

	for (size_t i = 0; i < func->num_blocks; i++) {
		ir_block_destroy(func->blocks[i]);
	}
	free(func->blocks);

	free(func);
}

int ir_function_add_param(ir_function_t *func, ir_node_t *param) {
	if (!func || !param) return -1;

	func->params = realloc(func->params, (func->num_params + 1) * sizeof(ir_node_t *));
	if (!func->params) return -1;

	func->params[func->num_params++] = param;
	return 0;
}

int ir_function_add_block(ir_function_t *func, ir_block_t *block) {
	if (!func || !block) return -1;

	func->blocks = realloc(func->blocks, (func->num_blocks + 1) * sizeof(ir_block_t *));
	if (!func->blocks) return -1;

	func->blocks[func->num_blocks++] = block;
	return 0;
}

void ir_function_set_mangled_name(ir_function_t *func, const char *name) {
	if (!func) return;
	free((void *)func->mangled_name);
	func->mangled_name = strdup_safe(name);
}

void ir_function_set_inline(ir_function_t *func, int is_inline) {
	if (func) func->is_inline = is_inline;
}

void ir_function_set_noreturn(ir_function_t *func, int is_noreturn) {
	if (func) func->is_noreturn = is_noreturn;
}

void ir_function_set_pure(ir_function_t *func, int is_pure) {
	if (func) func->is_pure = is_pure;
}

void ir_function_set_const(ir_function_t *func, int is_const) {
	if (func) func->is_const = is_const;
}

/*
 * Block Operations
 */

ir_block_t *ir_block_create(const char *label) {
	ir_block_t *block = calloc(1, sizeof(ir_block_t));
	if (!block) return NULL;

	block->label = strdup_safe(label);
	block->instructions = NULL;
	block->num_instructions = 0;
	block->next = NULL;

	return block;
}

void ir_block_destroy(ir_block_t *block) {
	if (!block) return;

	free((void *)block->label);

	for (size_t i = 0; i < block->num_instructions; i++) {
		ir_node_destroy(block->instructions[i]);
	}
	free(block->instructions);

	free(block);
}

int ir_block_add_instruction(ir_block_t *block, ir_node_t *instr) {
	if (!block || !instr) return -1;

	block->instructions = realloc(block->instructions,
	                              (block->num_instructions + 1) * sizeof(ir_node_t *));
	if (!block->instructions) return -1;

	block->instructions[block->num_instructions++] = instr;
	return 0;
}

/*
 * Node Operations
 */

ir_node_t *ir_node_create(ir_node_type_t type, ir_type_t *dtype) {
	ir_node_t *node = calloc(1, sizeof(ir_node_t));
	if (!node) return NULL;

	node->type = type;
	node->dtype = dtype;
	node->data = NULL;
	node->next = NULL;
	node->id = next_node_id++;
	node->name = NULL;
	node->line = 0;
	node->column = 0;

	return node;
}

void ir_node_destroy(ir_node_t *node) {
	if (!node) return;

	free((void *)node->name);
	free(node->data);
	free(node);
}

void ir_node_set_name(ir_node_t *node, const char *name) {
	if (!node) return;
	free((void *)node->name);
	node->name = strdup_safe(name);
}

void ir_node_set_location(ir_node_t *node, int line, int column) {
	if (!node) return;
	node->line = line;
	node->column = column;
}

/*
 * Instruction Builders
 */

typedef struct {
	ir_binary_op_t op;
	ir_node_t *left;
	ir_node_t *right;
} ir_binary_data_t;

typedef struct {
	ir_unary_op_t op;
	ir_node_t *operand;
} ir_unary_data_t;

typedef struct {
	ir_node_t *func;
	ir_node_t **args;
	size_t num_args;
} ir_call_data_t;

ir_node_t *ir_build_binary_op(ir_binary_op_t op, ir_node_t *left, ir_node_t *right) {
	if (!left || !right) return NULL;

	ir_node_t *node = ir_node_create(IR_BINARY_OP, left->dtype);
	if (!node) return NULL;

	ir_binary_data_t *data = malloc(sizeof(ir_binary_data_t));
	data->op = op;
	data->left = left;
	data->right = right;
	node->data = data;

	return node;
}

ir_node_t *ir_build_unary_op(ir_unary_op_t op, ir_node_t *operand) {
	if (!operand) return NULL;

	ir_node_t *node = ir_node_create(IR_UNARY_OP, operand->dtype);
	if (!node) return NULL;

	ir_unary_data_t *data = malloc(sizeof(ir_unary_data_t));
	data->op = op;
	data->operand = operand;
	node->data = data;

	return node;
}

ir_node_t *ir_build_call(ir_node_t *func, ir_node_t **args, size_t num_args) {
	if (!func) return NULL;

	ir_node_t *node = ir_node_create(IR_CALL, func->dtype);
	if (!node) return NULL;

	ir_call_data_t *data = malloc(sizeof(ir_call_data_t));
	data->func = func;
	data->args = args;
	data->num_args = num_args;
	node->data = data;

	return node;
}

ir_node_t *ir_build_cast(ir_type_t *type, ir_node_t *value) {
	if (!type || !value) return NULL;

	ir_node_t *node = ir_node_create(IR_CAST, type);
	if (!node) return NULL;

	node->data = value;
	return node;
}

ir_node_t *ir_build_load(ir_node_t *ptr) {
	if (!ptr) return NULL;

	ir_node_t *node = ir_node_create(IR_LOAD, ptr->dtype);
	if (!node) return NULL;

	node->data = ptr;
	return node;
}

ir_node_t *ir_build_store(ir_node_t *ptr, ir_node_t *value) {
	if (!ptr || !value) return NULL;

	ir_node_t *node = ir_node_create(IR_STORE, ir_type_void());
	if (!node) return NULL;

	ir_binary_data_t *data = malloc(sizeof(ir_binary_data_t));
	data->left = ptr;
	data->right = value;
	node->data = data;

	return node;
}

ir_node_t *ir_build_alloca(ir_type_t *type, ir_node_t *count) {
	if (!type) return NULL;

	ir_node_t *node = ir_node_create(IR_ALLOCA, ir_type_ptr(type));
	if (!node) return NULL;

	node->data = count;
	return node;
}

ir_node_t *ir_build_return(ir_node_t *value) {
	ir_node_t *node = ir_node_create(IR_RETURN, ir_type_void());
	if (!node) return NULL;

	node->data = value;
	return node;
}

/*
 * Literal Builders
 */

ir_node_t *ir_build_int(ir_type_t *type, int64_t value) {
	if (!type) return NULL;

	ir_node_t *node = ir_node_create(IR_INT_LITERAL, type);
	if (!node) return NULL;

	int64_t *data = malloc(sizeof(int64_t));
	*data = value;
	node->data = data;

	return node;
}

ir_node_t *ir_build_float(ir_type_t *type, double value) {
	if (!type) return NULL;

	ir_node_t *node = ir_node_create(IR_FLOAT_LITERAL, type);
	if (!node) return NULL;

	double *data = malloc(sizeof(double));
	*data = value;
	node->data = data;

	return node;
}

ir_node_t *ir_build_string(const char *value) {
	if (!value) return NULL;

	ir_type_t *type = ir_type_ptr(ir_type_int(8, 0));
	ir_node_t *node = ir_node_create(IR_STRING_LITERAL, type);
	if (!node) return NULL;

	node->data = strdup(value);
	return node;
}

ir_node_t *ir_build_null(ir_type_t *type) {
	if (!type) return NULL;

	ir_node_t *node = ir_node_create(IR_NULL_LITERAL, type);
	return node;
}

/*
 * Type System
 */

static ir_type_t *void_type_singleton = NULL;

ir_type_t *ir_type_void(void) {
	if (!void_type_singleton) {
		void_type_singleton = calloc(1, sizeof(ir_type_t));
		void_type_singleton->kind = IR_TYPE_VOID;
		void_type_singleton->size = 0;
		void_type_singleton->align = 1;
	}
	return void_type_singleton;
}

ir_type_t *ir_type_int(int bits, int is_signed) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_INT;
	type->size = (bits + 7) / 8;
	type->align = type->size;
	type->u.primitive.bits = bits;
	type->u.primitive.is_signed = is_signed;

	return type;
}

ir_type_t *ir_type_float(int bits) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_FLOAT;
	type->size = (bits + 7) / 8;
	type->align = type->size;
	type->u.primitive.bits = bits;

	return type;
}

ir_type_t *ir_type_ptr(ir_type_t *pointee) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_PTR;
	type->size = sizeof(void *);
	type->align = sizeof(void *);
	type->u.ptr.pointee = pointee;

	return type;
}

ir_type_t *ir_type_array(ir_type_t *element, size_t length) {
	if (!element) return NULL;

	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_ARRAY;
	type->size = element->size * length;
	type->align = element->align;
	type->u.array.element = element;
	type->u.array.length = length;

	return type;
}

ir_type_t *ir_type_struct(const char *tag, ir_node_t **members, size_t num_members) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_STRUCT;
	type->u.aggregate.tag = strdup_safe(tag);
	type->u.aggregate.members = members;
	type->u.aggregate.num_members = num_members;

	/* Calculate size and alignment */
	size_t offset = 0;
	size_t max_align = 1;
	for (size_t i = 0; i < num_members; i++) {
		ir_type_t *member_type = members[i]->dtype;
		size_t align = member_type->align;
		if (align > max_align) max_align = align;

		/* Align offset */
		offset = (offset + align - 1) & ~(align - 1);
		offset += member_type->size;
	}

	type->size = (offset + max_align - 1) & ~(max_align - 1);
	type->align = max_align;

	return type;
}

ir_type_t *ir_type_union(const char *tag, ir_node_t **members, size_t num_members) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_UNION;
	type->u.aggregate.tag = strdup_safe(tag);
	type->u.aggregate.members = members;
	type->u.aggregate.num_members = num_members;

	/* Calculate size and alignment (max of all members) */
	size_t max_size = 0;
	size_t max_align = 1;
	for (size_t i = 0; i < num_members; i++) {
		ir_type_t *member_type = members[i]->dtype;
		if (member_type->size > max_size) max_size = member_type->size;
		if (member_type->align > max_align) max_align = member_type->align;
	}

	type->size = (max_size + max_align - 1) & ~(max_align - 1);
	type->align = max_align;

	return type;
}

ir_type_t *ir_type_function(ir_type_t *return_type, ir_type_t **param_types,
                            size_t num_params, int is_variadic) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_FUNCTION;
	type->size = sizeof(void *);  /* Function pointer size */
	type->align = sizeof(void *);
	type->u.function.return_type = return_type;
	type->u.function.param_types = param_types;
	type->u.function.num_params = num_params;
	type->u.function.is_variadic = is_variadic;
	type->u.function.callconv = IR_CC_C;

	return type;
}

ir_type_t *ir_type_generic(const char *name, ir_type_t **constraints, size_t num_constraints) {
	ir_type_t *type = calloc(1, sizeof(ir_type_t));
	if (!type) return NULL;

	type->kind = IR_TYPE_GENERIC;
	type->size = 0;  /* Unknown until instantiated */
	type->align = 0;
	type->u.generic.name = strdup_safe(name);
	type->u.generic.constraints = constraints;
	type->u.generic.num_constraints = num_constraints;

	return type;
}

ir_type_t *ir_type_qualified(ir_type_t *type, ir_qualifier_t quals) {
	if (!type) return NULL;

	ir_type_t *qualified = malloc(sizeof(ir_type_t));
	memcpy(qualified, type, sizeof(ir_type_t));
	qualified->quals |= quals;

	return qualified;
}

size_t ir_type_sizeof(ir_type_t *type) {
	return type ? type->size : 0;
}

size_t ir_type_alignof(ir_type_t *type) {
	return type ? type->align : 1;
}

int ir_type_equals(ir_type_t *a, ir_type_t *b) {
	if (a == b) return 1;
	if (!a || !b) return 0;
	if (a->kind != b->kind) return 0;

	switch (a->kind) {
	case IR_TYPE_VOID:
		return 1;
	case IR_TYPE_INT:
	case IR_TYPE_FLOAT:
		return a->u.primitive.bits == b->u.primitive.bits &&
		       a->u.primitive.is_signed == b->u.primitive.is_signed;
	case IR_TYPE_PTR:
		return ir_type_equals(a->u.ptr.pointee, b->u.ptr.pointee);
	case IR_TYPE_ARRAY:
		return a->u.array.length == b->u.array.length &&
		       ir_type_equals(a->u.array.element, b->u.array.element);
	default:
		return 0;
	}
}

int ir_type_compatible(ir_type_t *a, ir_type_t *b) {
	/* For now, same as equals */
	return ir_type_equals(a, b);
}

/*
 * Serialization and Printing (continued in next part...)
 */

/*
 * Printing
 */

static const char *ir_opcode_name(ir_node_type_t type) {
	switch (type) {
	case IR_MODULE: return "module";
	case IR_FUNCTION: return "function";
	case IR_VARIABLE: return "variable";
	case IR_TYPE_DEF: return "typedef";
	case IR_CONSTANT: return "constant";
	case IR_IMPORT: return "import";
	case IR_EXPORT: return "export";
	case IR_BLOCK: return "block";
	case IR_IF: return "if";
	case IR_WHILE: return "while";
	case IR_FOR: return "for";
	case IR_SWITCH: return "switch";
	case IR_CASE: return "case";
	case IR_RETURN: return "return";
	case IR_GOTO: return "goto";
	case IR_LABEL: return "label";
	case IR_BREAK: return "break";
	case IR_CONTINUE: return "continue";
	case IR_EXPR_STMT: return "expr";
	case IR_BINARY_OP: return "binop";
	case IR_UNARY_OP: return "unop";
	case IR_CALL: return "call";
	case IR_CAST: return "cast";
	case IR_LOAD: return "load";
	case IR_STORE: return "store";
	case IR_ALLOCA: return "alloca";
	case IR_PHI: return "phi";
	case IR_SELECT: return "select";
	case IR_EXTRACT: return "extract";
	case IR_INSERT: return "insert";
	case IR_INT_LITERAL: return "int";
	case IR_FLOAT_LITERAL: return "float";
	case IR_STRING_LITERAL: return "string";
	case IR_NULL_LITERAL: return "null";
	default: return "unknown";
	}
}

static void ir_print_type(ir_type_t *type, FILE *f) {
	if (!type) {
		fprintf(f, "void");
		return;
	}

	switch (type->kind) {
	case IR_TYPE_VOID:
		fprintf(f, "void");
		break;
	case IR_TYPE_INT:
		fprintf(f, "%si%d", type->u.primitive.is_signed ? "" : "u", type->u.primitive.bits);
		break;
	case IR_TYPE_FLOAT:
		fprintf(f, "f%d", type->u.primitive.bits);
		break;
	case IR_TYPE_PTR:
		ir_print_type(type->u.ptr.pointee, f);
		fprintf(f, "*");
		break;
	case IR_TYPE_ARRAY:
		ir_print_type(type->u.array.element, f);
		fprintf(f, "[%zu]", type->u.array.length);
		break;
	case IR_TYPE_STRUCT:
		fprintf(f, "struct %s", type->u.aggregate.tag ? type->u.aggregate.tag : "(anonymous)");
		break;
	case IR_TYPE_UNION:
		fprintf(f, "union %s", type->u.aggregate.tag ? type->u.aggregate.tag : "(anonymous)");
		break;
	case IR_TYPE_FUNCTION:
		ir_print_type(type->u.function.return_type, f);
		fprintf(f, " (");
		for (size_t i = 0; i < type->u.function.num_params; i++) {
			if (i > 0) fprintf(f, ", ");
			ir_print_type(type->u.function.param_types[i], f);
		}
		if (type->u.function.is_variadic) fprintf(f, ", ...");
		fprintf(f, ")");
		break;
	default:
		fprintf(f, "???");
		break;
	}
}

static void ir_print_node(ir_node_t *node, FILE *f, int indent) {
	if (!node) return;

	for (int i = 0; i < indent; i++) fprintf(f, "  ");

	fprintf(f, "%%%d = %s ", node->id, ir_opcode_name(node->type));
	if (node->dtype) {
		ir_print_type(node->dtype, f);
		fprintf(f, " ");
	}

	switch (node->type) {
	case IR_INT_LITERAL:
		fprintf(f, "%lld", *(int64_t *)node->data);
		break;
	case IR_FLOAT_LITERAL:
		fprintf(f, "%f", *(double *)node->data);
		break;
	case IR_STRING_LITERAL:
		fprintf(f, "\"%s\"", (char *)node->data);
		break;
	case IR_LOAD:
	case IR_RETURN:
		if (node->data) {
			fprintf(f, "%%%d", ((ir_node_t *)node->data)->id);
		}
		break;
	case IR_BINARY_OP: {
		ir_binary_data_t *data = node->data;
		fprintf(f, "%%%d, %%%d", data->left->id, data->right->id);
		break;
	}
	case IR_UNARY_OP: {
		ir_unary_data_t *data = node->data;
		fprintf(f, "%%%d", data->operand->id);
		break;
	}
	case IR_CALL: {
		ir_call_data_t *data = node->data;
		fprintf(f, "@%s(", data->func->name ? data->func->name : "???");
		for (size_t i = 0; i < data->num_args; i++) {
			if (i > 0) fprintf(f, ", ");
			fprintf(f, "%%%d", data->args[i]->id);
		}
		fprintf(f, ")");
		break;
	}
	default:
		if (node->name) fprintf(f, "%s", node->name);
		break;
	}

	fprintf(f, "\n");
}

static void ir_print_block(ir_block_t *block, FILE *f) {
	if (!block) return;

	fprintf(f, "%s:\n", block->label ? block->label : "entry");

	for (size_t i = 0; i < block->num_instructions; i++) {
		ir_print_node(block->instructions[i], f, 1);
	}

	fprintf(f, "\n");
}

static void ir_print_function(ir_function_t *func, FILE *f) {
	if (!func) return;

	fprintf(f, "define ");
	if (func->type && func->type->kind == IR_TYPE_FUNCTION) {
		ir_print_type(func->type->u.function.return_type, f);
	}
	fprintf(f, " @%s(", func->name);

	for (size_t i = 0; i < func->num_params; i++) {
		if (i > 0) fprintf(f, ", ");
		ir_print_type(func->params[i]->dtype, f);
		if (func->params[i]->name) {
			fprintf(f, " %%%s", func->params[i]->name);
		}
	}

	fprintf(f, ") {\n");

	for (size_t i = 0; i < func->num_blocks; i++) {
		ir_print_block(func->blocks[i], f);
	}

	fprintf(f, "}\n\n");
}

void ir_module_print(ir_module_t *mod, FILE *f) {
	if (!mod || !f) return;

	fprintf(f, "; Module: %s\n", mod->name ? mod->name : "(unnamed)");
	if (mod->source_language) {
		fprintf(f, "; Language: %s\n", mod->source_language);
	}
	if (mod->target_triple) {
		fprintf(f, "; Target: %s\n", mod->target_triple);
	}
	fprintf(f, "\n");

	/* Print imports */
	if (mod->imports.count > 0) {
		fprintf(f, "; Imports\n");
		for (size_t i = 0; i < mod->imports.count; i++) {
			fprintf(f, "import \"%s\"\n", mod->imports.names[i]);
		}
		fprintf(f, "\n");
	}

	/* Print exports */
	if (mod->exports.count > 0) {
		fprintf(f, "; Exports\n");
		for (size_t i = 0; i < mod->exports.count; i++) {
			fprintf(f, "export \"%s\"\n", mod->exports.names[i]);
		}
		fprintf(f, "\n");
	}

	/* Print globals */
	if (mod->num_globals > 0) {
		fprintf(f, "; Global Variables\n");
		for (size_t i = 0; i < mod->num_globals; i++) {
			fprintf(f, "@%s = global ", mod->globals[i]->name ? mod->globals[i]->name : "???");
			ir_print_type(mod->globals[i]->dtype, f);
			fprintf(f, "\n");
		}
		fprintf(f, "\n");
	}

	/* Print functions */
	fprintf(f, "; Functions\n");
	for (size_t i = 0; i < mod->num_functions; i++) {
		ir_print_function(mod->functions[i], f);
	}
}

int ir_module_write_text(ir_module_t *mod, FILE *f) {
	ir_module_print(mod, f);
	return 0;
}

/*
 * Binary Serialization
 */

int ir_module_write(ir_module_t *mod, const char *filename) {
	if (!mod || !filename) return -1;

	FILE *f = fopen(filename, "wb");
	if (!f) return -1;

	/* Write header */
	fwrite("PCCIR", 5, 1, f);
	uint16_t version = (PCCIR_VERSION_MAJOR << 8) | PCCIR_VERSION_MINOR;
	fwrite(&version, sizeof(version), 1, f);

	/* Write module name */
	uint32_t len = mod->name ? strlen(mod->name) : 0;
	fwrite(&len, sizeof(len), 1, f);
	if (len > 0) fwrite(mod->name, 1, len, f);

	/* Write language */
	len = mod->source_language ? strlen(mod->source_language) : 0;
	fwrite(&len, sizeof(len), 1, f);
	if (len > 0) fwrite(mod->source_language, 1, len, f);

	/* Write counts */
	fwrite(&mod->num_functions, sizeof(mod->num_functions), 1, f);
	fwrite(&mod->num_globals, sizeof(mod->num_globals), 1, f);
	fwrite(&mod->num_types, sizeof(mod->num_types), 1, f);

	/* TODO: Write detailed structure */

	fclose(f);
	return 0;
}

ir_module_t *ir_module_read(const char *filename) {
	if (!filename) return NULL;

	FILE *f = fopen(filename, "rb");
	if (!f) return NULL;

	/* Read header */
	char magic[5];
	fread(magic, 1, 5, f);
	if (memcmp(magic, "PCCIR", 5) != 0) {
		fclose(f);
		return NULL;
	}

	uint16_t version;
	fread(&version, sizeof(version), 1, f);

	/* Read module name */
	uint32_t len;
	fread(&len, sizeof(len), 1, f);
	char *name = NULL;
	if (len > 0) {
		name = malloc(len + 1);
		fread(name, 1, len, f);
		name[len] = '\0';
	}

	/* Read language */
	fread(&len, sizeof(len), 1, f);
	char *lang = NULL;
	if (len > 0) {
		lang = malloc(len + 1);
		fread(lang, 1, len, f);
		lang[len] = '\0';
	}

	ir_module_t *mod = ir_module_create(name, lang);
	free(name);
	free(lang);

	/* Read counts */
	fread(&mod->num_functions, sizeof(mod->num_functions), 1, f);
	fread(&mod->num_globals, sizeof(mod->num_globals), 1, f);
	fread(&mod->num_types, sizeof(mod->num_types), 1, f);

	/* TODO: Read detailed structure */

	fclose(f);
	return mod;
}

uint8_t *ir_module_serialize(ir_module_t *mod, size_t *out_size) {
	/* Simplified implementation */
	*out_size = 1024;  /* Placeholder */
	return calloc(1, *out_size);
}

ir_module_t *ir_module_deserialize(const uint8_t *buffer, size_t size) {
	/* Simplified implementation */
	(void)buffer;
	(void)size;
	return NULL;
}

/*
 * Verification
 */

int ir_module_verify(ir_module_t *mod, char **error_msg) {
	if (!mod) {
		if (error_msg) *error_msg = strdup("Module is NULL");
		return -1;
	}

	/* Verify all functions */
	for (size_t i = 0; i < mod->num_functions; i++) {
		if (ir_function_verify(mod->functions[i], error_msg) < 0) {
			return -1;
		}
	}

	if (error_msg) *error_msg = NULL;
	return 0;
}

int ir_function_verify(ir_function_t *func, char **error_msg) {
	if (!func) {
		if (error_msg) *error_msg = strdup("Function is NULL");
		return -1;
	}

	if (!func->name) {
		if (error_msg) *error_msg = strdup("Function has no name");
		return -1;
	}

	if (error_msg) *error_msg = NULL;
	return 0;
}

int ir_type_verify(ir_type_t *type, char **error_msg) {
	if (!type) {
		if (error_msg) *error_msg = strdup("Type is NULL");
		return -1;
	}

	if (error_msg) *error_msg = NULL;
	return 0;
}

/*
 * Optimization Passes
 */

struct ir_pass {
	const char *name;
	int (*run_on_module)(ir_module_t *);
	int (*run_on_function)(ir_function_t *);
};

ir_pass_t *ir_pass_create(const char *name) {
	ir_pass_t *pass = calloc(1, sizeof(ir_pass_t));
	if (!pass) return NULL;

	pass->name = strdup_safe(name);
	return pass;
}

void ir_pass_destroy(ir_pass_t *pass) {
	if (!pass) return;
	free((void *)pass->name);
	free(pass);
}

int ir_pass_run_on_module(ir_pass_t *pass, ir_module_t *mod) {
	if (!pass || !mod) return -1;
	if (pass->run_on_module) {
		return pass->run_on_module(mod);
	}
	return 0;
}

int ir_pass_run_on_function(ir_pass_t *pass, ir_function_t *func) {
	if (!pass || !func) return -1;
	if (pass->run_on_function) {
		return pass->run_on_function(func);
	}
	return 0;
}

/* Stub implementations for standard passes */
ir_pass_t *ir_pass_dead_code_elimination(void) {
	return ir_pass_create("dce");
}

ir_pass_t *ir_pass_constant_folding(void) {
	return ir_pass_create("constfold");
}

ir_pass_t *ir_pass_constant_propagation(void) {
	return ir_pass_create("constprop");
}

ir_pass_t *ir_pass_common_subexpression_elimination(void) {
	return ir_pass_create("cse");
}

ir_pass_t *ir_pass_inline_functions(void) {
	return ir_pass_create("inline");
}

ir_pass_t *ir_pass_loop_optimization(void) {
	return ir_pass_create("loop-opt");
}

/*
 * Generics/Templates
 */

ir_function_t *ir_generic_instantiate_function(ir_function_t *template_func,
                                               ir_type_t **type_args, size_t num_args) {
	(void)template_func;
	(void)type_args;
	(void)num_args;
	/* TODO: Implement generic instantiation */
	return NULL;
}

ir_type_t *ir_generic_instantiate_type(ir_type_t *template_type,
                                       ir_type_t **type_args, size_t num_args) {
	(void)template_type;
	(void)type_args;
	(void)num_args;
	/* TODO: Implement generic type instantiation */
	return NULL;
}

/*
 * Cross-Language Support
 */

void ir_module_set_mangling(ir_module_t *mod, const char *scheme) {
	/* Stored in metadata */
	(void)mod;
	(void)scheme;
}

char *ir_mangle_name(const char *name, ir_type_t *type, const char *scheme) {
	/* Simplified - just return copy for now */
	(void)type;
	(void)scheme;
	return strdup_safe(name);
}

char *ir_demangle_name(const char *mangled, const char *scheme) {
	/* Simplified - just return copy for now */
	(void)scheme;
	return strdup_safe(mangled);
}

/*
 * Precompiled Headers
 */

int ir_create_pch(ir_module_t *mod, const char *filename) {
	return ir_module_write(mod, filename);
}

ir_module_t *ir_load_pch(const char *filename) {
	return ir_module_read(filename);
}

int ir_merge_pch(ir_module_t *dest, ir_module_t *src) {
	if (!dest || !src) return -1;

	/* Merge functions */
	for (size_t i = 0; i < src->num_functions; i++) {
		ir_module_add_function(dest, src->functions[i]);
	}

	/* Merge globals */
	for (size_t i = 0; i < src->num_globals; i++) {
		ir_module_add_global(dest, src->globals[i]);
	}

	/* Merge types */
	for (size_t i = 0; i < src->num_types; i++) {
		ir_module_add_type(dest, src->types[i]);
	}

	return 0;
}
