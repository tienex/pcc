/*
 * Copyright (c) 2025 PCC Project
 *
 * PCC Intermediate Representation Library
 *
 * Generate and manipulate PCC IR for cross-language modules,
 * precompiled headers, and generic/template instantiation
 */

#ifndef _PCC_PCCIR_H_
#define _PCC_PCCIR_H_

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * IR Version
 */
#define PCCIR_VERSION_MAJOR 1
#define PCCIR_VERSION_MINOR 0

/*
 * Node Types
 */
typedef enum {
	/* Declarations */
	IR_MODULE,
	IR_FUNCTION,
	IR_VARIABLE,
	IR_TYPE_DEF,
	IR_CONSTANT,
	IR_IMPORT,
	IR_EXPORT,

	/* Statements */
	IR_BLOCK,
	IR_IF,
	IR_WHILE,
	IR_FOR,
	IR_SWITCH,
	IR_CASE,
	IR_RETURN,
	IR_GOTO,
	IR_LABEL,
	IR_BREAK,
	IR_CONTINUE,
	IR_EXPR_STMT,

	/* Expressions */
	IR_BINARY_OP,
	IR_UNARY_OP,
	IR_CALL,
	IR_CAST,
	IR_LOAD,
	IR_STORE,
	IR_ALLOCA,
	IR_PHI,
	IR_SELECT,
	IR_EXTRACT,
	IR_INSERT,

	/* Literals */
	IR_INT_LITERAL,
	IR_FLOAT_LITERAL,
	IR_STRING_LITERAL,
	IR_NULL_LITERAL,

	/* Types */
	IR_TYPE_VOID,
	IR_TYPE_INT,
	IR_TYPE_FLOAT,
	IR_TYPE_PTR,
	IR_TYPE_ARRAY,
	IR_TYPE_STRUCT,
	IR_TYPE_UNION,
	IR_TYPE_FUNCTION,
	IR_TYPE_ENUM,
	IR_TYPE_GENERIC
} ir_node_type_t;

/*
 * Binary Operators
 */
typedef enum {
	IR_OP_ADD, IR_OP_SUB, IR_OP_MUL, IR_OP_DIV, IR_OP_MOD,
	IR_OP_AND, IR_OP_OR, IR_OP_XOR, IR_OP_SHL, IR_OP_SHR,
	IR_OP_EQ, IR_OP_NE, IR_OP_LT, IR_OP_LE, IR_OP_GT, IR_OP_GE,
	IR_OP_LAND, IR_OP_LOR
} ir_binary_op_t;

/*
 * Unary Operators
 */
typedef enum {
	IR_OP_NEG, IR_OP_NOT, IR_OP_LNOT, IR_OP_DEREF, IR_OP_ADDR
} ir_unary_op_t;

/*
 * Storage Classes
 */
typedef enum {
	IR_STORAGE_AUTO,
	IR_STORAGE_STATIC,
	IR_STORAGE_EXTERN,
	IR_STORAGE_REGISTER,
	IR_STORAGE_THREAD_LOCAL
} ir_storage_t;

/*
 * Linkage Types
 */
typedef enum {
	IR_LINKAGE_INTERNAL,
	IR_LINKAGE_EXTERNAL,
	IR_LINKAGE_WEAK,
	IR_LINKAGE_COMMON
} ir_linkage_t;

/*
 * Calling Conventions
 */
typedef enum {
	IR_CC_C,
	IR_CC_STDCALL,
	IR_CC_FASTCALL,
	IR_CC_PASCAL,
	IR_CC_FORTRAN,
	IR_CC_SYSTEM_V,
	IR_CC_WIN64
} ir_callconv_t;

/*
 * Type Qualifiers
 */
typedef enum {
	IR_QUAL_NONE = 0,
	IR_QUAL_CONST = 1 << 0,
	IR_QUAL_VOLATILE = 1 << 1,
	IR_QUAL_RESTRICT = 1 << 2
} ir_qualifier_t;

/*
 * IR Node
 */
typedef struct ir_node ir_node_t;
typedef struct ir_type ir_type_t;
typedef struct ir_module ir_module_t;
typedef struct ir_function ir_function_t;
typedef struct ir_block ir_block_t;

struct ir_node {
	ir_node_type_t type;
	ir_type_t *dtype;  /* Data type */
	void *data;        /* Type-specific data */
	ir_node_t *next;   /* Linked list */
	int id;            /* Unique node ID */
	const char *name;  /* Optional name */
	int line, column;  /* Source location */
};

/*
 * Type System
 */
struct ir_type {
	ir_node_type_t kind;
	size_t size;
	size_t align;
	ir_qualifier_t quals;
	union {
		struct {  /* Integer/Float */
			int bits;
			int is_signed;
		} primitive;
		struct {  /* Pointer */
			ir_type_t *pointee;
		} ptr;
		struct {  /* Array */
			ir_type_t *element;
			size_t length;
		} array;
		struct {  /* Struct/Union */
			const char *tag;
			ir_node_t **members;
			size_t num_members;
		} aggregate;
		struct {  /* Function */
			ir_type_t *return_type;
			ir_type_t **param_types;
			size_t num_params;
			int is_variadic;
			ir_callconv_t callconv;
		} function;
		struct {  /* Generic/Template */
			const char *name;
			ir_type_t **constraints;
			size_t num_constraints;
		} generic;
	} u;
};

/*
 * Module
 */
struct ir_module {
	const char *name;
	const char *source_language;  /* "c", "c++", "pascal", etc. */
	const char *target_triple;

	ir_function_t **functions;
	size_t num_functions;

	ir_node_t **globals;
	size_t num_globals;

	ir_type_t **types;
	size_t num_types;

	struct {
		const char **names;
		size_t count;
	} imports;

	struct {
		const char **names;
		size_t count;
	} exports;

	void *metadata;  /* Language-specific metadata */
};

/*
 * Function
 */
struct ir_function {
	const char *name;
	const char *mangled_name;
	ir_type_t *type;
	ir_storage_t storage;
	ir_linkage_t linkage;
	ir_callconv_t callconv;

	ir_node_t **params;
	size_t num_params;

	ir_block_t **blocks;
	size_t num_blocks;

	int is_inline;
	int is_noreturn;
	int is_pure;
	int is_const;
};

/*
 * Basic Block
 */
struct ir_block {
	const char *label;
	ir_node_t **instructions;
	size_t num_instructions;
	ir_block_t *next;
};

/*
 * Module Operations
 */

/* Create module */
ir_module_t *ir_module_create(const char *name, const char *language);

/* Destroy module */
void ir_module_destroy(ir_module_t *mod);

/* Add function */
int ir_module_add_function(ir_module_t *mod, ir_function_t *func);

/* Add global variable */
int ir_module_add_global(ir_module_t *mod, ir_node_t *global);

/* Add type definition */
int ir_module_add_type(ir_module_t *mod, ir_type_t *type);

/* Add import */
int ir_module_add_import(ir_module_t *mod, const char *name);

/* Add export */
int ir_module_add_export(ir_module_t *mod, const char *name);

/* Set target triple */
void ir_module_set_target(ir_module_t *mod, const char *triple);

/* Set metadata */
void ir_module_set_metadata(ir_module_t *mod, void *metadata);

/*
 * Function Operations
 */

/* Create function */
ir_function_t *ir_function_create(const char *name, ir_type_t *type);

/* Destroy function */
void ir_function_destroy(ir_function_t *func);

/* Add parameter */
int ir_function_add_param(ir_function_t *func, ir_node_t *param);

/* Add basic block */
int ir_function_add_block(ir_function_t *func, ir_block_t *block);

/* Set mangled name */
void ir_function_set_mangled_name(ir_function_t *func, const char *name);

/* Set attributes */
void ir_function_set_inline(ir_function_t *func, int is_inline);
void ir_function_set_noreturn(ir_function_t *func, int is_noreturn);
void ir_function_set_pure(ir_function_t *func, int is_pure);
void ir_function_set_const(ir_function_t *func, int is_const);

/*
 * Block Operations
 */

/* Create block */
ir_block_t *ir_block_create(const char *label);

/* Destroy block */
void ir_block_destroy(ir_block_t *block);

/* Add instruction */
int ir_block_add_instruction(ir_block_t *block, ir_node_t *instr);

/*
 * Node Creation
 */

/* Create node */
ir_node_t *ir_node_create(ir_node_type_t type, ir_type_t *dtype);

/* Destroy node */
void ir_node_destroy(ir_node_t *node);

/* Set name */
void ir_node_set_name(ir_node_t *node, const char *name);

/* Set source location */
void ir_node_set_location(ir_node_t *node, int line, int column);

/*
 * Instruction Builders
 */

/* Binary operation */
ir_node_t *ir_build_binary_op(ir_binary_op_t op, ir_node_t *left, ir_node_t *right);

/* Unary operation */
ir_node_t *ir_build_unary_op(ir_unary_op_t op, ir_node_t *operand);

/* Function call */
ir_node_t *ir_build_call(ir_node_t *func, ir_node_t **args, size_t num_args);

/* Cast */
ir_node_t *ir_build_cast(ir_type_t *type, ir_node_t *value);

/* Load */
ir_node_t *ir_build_load(ir_node_t *ptr);

/* Store */
ir_node_t *ir_build_store(ir_node_t *ptr, ir_node_t *value);

/* Alloca */
ir_node_t *ir_build_alloca(ir_type_t *type, ir_node_t *count);

/* Return */
ir_node_t *ir_build_return(ir_node_t *value);

/* Branch */
ir_node_t *ir_build_branch(ir_block_t *target);

/* Conditional branch */
ir_node_t *ir_build_cond_branch(ir_node_t *cond, ir_block_t *true_block, ir_block_t *false_block);

/* Phi node */
ir_node_t *ir_build_phi(ir_type_t *type, ir_node_t **values, ir_block_t **blocks, size_t count);

/* Select */
ir_node_t *ir_build_select(ir_node_t *cond, ir_node_t *true_val, ir_node_t *false_val);

/*
 * Literal Builders
 */

/* Integer literal */
ir_node_t *ir_build_int(ir_type_t *type, int64_t value);

/* Float literal */
ir_node_t *ir_build_float(ir_type_t *type, double value);

/* String literal */
ir_node_t *ir_build_string(const char *value);

/* Null literal */
ir_node_t *ir_build_null(ir_type_t *type);

/*
 * Type System
 */

/* Create void type */
ir_type_t *ir_type_void(void);

/* Create integer type */
ir_type_t *ir_type_int(int bits, int is_signed);

/* Create float type */
ir_type_t *ir_type_float(int bits);

/* Create pointer type */
ir_type_t *ir_type_ptr(ir_type_t *pointee);

/* Create array type */
ir_type_t *ir_type_array(ir_type_t *element, size_t length);

/* Create struct type */
ir_type_t *ir_type_struct(const char *tag, ir_node_t **members, size_t num_members);

/* Create union type */
ir_type_t *ir_type_union(const char *tag, ir_node_t **members, size_t num_members);

/* Create function type */
ir_type_t *ir_type_function(ir_type_t *return_type, ir_type_t **param_types,
                            size_t num_params, int is_variadic);

/* Create generic type */
ir_type_t *ir_type_generic(const char *name, ir_type_t **constraints, size_t num_constraints);

/* Add type qualifier */
ir_type_t *ir_type_qualified(ir_type_t *type, ir_qualifier_t quals);

/* Get type size */
size_t ir_type_sizeof(ir_type_t *type);

/* Get type alignment */
size_t ir_type_alignof(ir_type_t *type);

/* Check type equality */
int ir_type_equals(ir_type_t *a, ir_type_t *b);

/* Check type compatibility */
int ir_type_compatible(ir_type_t *a, ir_type_t *b);

/*
 * Serialization
 */

/* Write module to file (binary) */
int ir_module_write(ir_module_t *mod, const char *filename);

/* Read module from file (binary) */
ir_module_t *ir_module_read(const char *filename);

/* Write module to buffer */
uint8_t *ir_module_serialize(ir_module_t *mod, size_t *out_size);

/* Read module from buffer */
ir_module_t *ir_module_deserialize(const uint8_t *buffer, size_t size);

/* Write module to text (assembly-like) */
int ir_module_write_text(ir_module_t *mod, FILE *f);

/* Print module (for debugging) */
void ir_module_print(ir_module_t *mod, FILE *f);

/*
 * Verification
 */

/* Verify module */
int ir_module_verify(ir_module_t *mod, char **error_msg);

/* Verify function */
int ir_function_verify(ir_function_t *func, char **error_msg);

/* Verify type */
int ir_type_verify(ir_type_t *type, char **error_msg);

/*
 * Optimization
 */

typedef struct ir_pass ir_pass_t;

/* Create optimization pass */
ir_pass_t *ir_pass_create(const char *name);

/* Destroy pass */
void ir_pass_destroy(ir_pass_t *pass);

/* Run pass on module */
int ir_pass_run_on_module(ir_pass_t *pass, ir_module_t *mod);

/* Run pass on function */
int ir_pass_run_on_function(ir_pass_t *pass, ir_function_t *func);

/*
 * Standard Passes
 */
ir_pass_t *ir_pass_dead_code_elimination(void);
ir_pass_t *ir_pass_constant_folding(void);
ir_pass_t *ir_pass_constant_propagation(void);
ir_pass_t *ir_pass_common_subexpression_elimination(void);
ir_pass_t *ir_pass_inline_functions(void);
ir_pass_t *ir_pass_loop_optimization(void);

/*
 * Generics/Templates Support
 */

/* Create generic instantiation */
ir_function_t *ir_generic_instantiate_function(ir_function_t *template_func,
                                               ir_type_t **type_args, size_t num_args);

/* Create generic type instantiation */
ir_type_t *ir_generic_instantiate_type(ir_type_t *template_type,
                                       ir_type_t **type_args, size_t num_args);

/*
 * Cross-Language Support
 */

/* Set name mangling scheme */
void ir_module_set_mangling(ir_module_t *mod, const char *scheme);  /* "c", "cxx", "pascal", "pcc" */

/* Mangle name */
char *ir_mangle_name(const char *name, ir_type_t *type, const char *scheme);

/* Demangle name */
char *ir_demangle_name(const char *mangled, const char *scheme);

/*
 * Precompiled Headers
 */

/* Create precompiled header */
int ir_create_pch(ir_module_t *mod, const char *filename);

/* Load precompiled header */
ir_module_t *ir_load_pch(const char *filename);

/* Merge precompiled headers */
int ir_merge_pch(ir_module_t *dest, ir_module_t *src);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_PCCIR_H_ */
