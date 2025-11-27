/*
 * C# 3.0 Compiler - Pass 1 Header
 * Architecture and Endian Neutral Design
 */

#ifndef _CS_PASS1_H_
#define _CS_PASS1_H_

#include "../../mip/pass1.h"
#include "../../cc/ccom/arc.h"

/* C# Language Version */
#define CS_VERSION_30 3

/* C# Type System */
enum cs_type_kind {
	CS_TYPE_VOID,
	CS_TYPE_BOOL,
	CS_TYPE_BYTE,
	CS_TYPE_SBYTE,
	CS_TYPE_SHORT,
	CS_TYPE_USHORT,
	CS_TYPE_INT,
	CS_TYPE_UINT,
	CS_TYPE_LONG,
	CS_TYPE_ULONG,
	CS_TYPE_FLOAT,
	CS_TYPE_DOUBLE,
	CS_TYPE_DECIMAL,
	CS_TYPE_CHAR,
	CS_TYPE_STRING,
	CS_TYPE_OBJECT,
	CS_TYPE_CLASS,
	CS_TYPE_STRUCT,
	CS_TYPE_INTERFACE,
	CS_TYPE_ENUM,
	CS_TYPE_DELEGATE,
	CS_TYPE_ARRAY,
	CS_TYPE_NULLABLE,
	CS_TYPE_GENERIC,
	CS_TYPE_VAR,          /* C# 3.0: var keyword */
	CS_TYPE_LAMBDA,       /* C# 3.0: lambda expressions */
	CS_TYPE_ANONYMOUS,    /* C# 3.0: anonymous types */
};

/* C# Modifiers */
#define CS_MOD_PUBLIC       (1 << 0)
#define CS_MOD_PRIVATE      (1 << 1)
#define CS_MOD_PROTECTED    (1 << 2)
#define CS_MOD_INTERNAL     (1 << 3)
#define CS_MOD_STATIC       (1 << 4)
#define CS_MOD_READONLY     (1 << 5)
#define CS_MOD_CONST        (1 << 6)
#define CS_MOD_VIRTUAL      (1 << 7)
#define CS_MOD_OVERRIDE     (1 << 8)
#define CS_MOD_ABSTRACT     (1 << 9)
#define CS_MOD_SEALED       (1 << 10)
#define CS_MOD_PARTIAL      (1 << 11)
#define CS_MOD_UNSAFE       (1 << 12)
#define CS_MOD_VOLATILE     (1 << 13)
#define CS_MOD_EXTERN       (1 << 14)

/* C# AST Node Types */
enum cs_node_type {
	CS_NODE_NAMESPACE,
	CS_NODE_USING,
	CS_NODE_CLASS,
	CS_NODE_STRUCT,
	CS_NODE_INTERFACE,
	CS_NODE_ENUM,
	CS_NODE_DELEGATE,
	CS_NODE_METHOD,
	CS_NODE_PROPERTY,
	CS_NODE_FIELD,
	CS_NODE_CONSTRUCTOR,
	CS_NODE_DESTRUCTOR,
	CS_NODE_EVENT,
	CS_NODE_INDEXER,
	CS_NODE_OPERATOR,
	CS_NODE_LAMBDA,
	CS_NODE_QUERY,        /* LINQ query expression */
	CS_NODE_ANONYMOUS,    /* Anonymous type */
	CS_NODE_EXTENSION,    /* Extension method */
	CS_NODE_AUTO_PROPERTY,/* Auto-implemented property */
	CS_NODE_OBJECT_INIT,  /* Object initializer */
	CS_NODE_COLLECTION_INIT, /* Collection initializer */
};

/* C# Symbol Table Entry */
struct cs_symbol {
	char *name;
	enum cs_type_kind type;
	unsigned int modifiers;
	struct cs_symbol *next;
	struct cs_symbol *parent;
	void *type_info;      /* Additional type information */
	int line;
	int column;

	/* ARC integration for reference types */
	int is_reference_type;
	int arc_qualifier;    /* __strong, __weak, etc. */
};

/* C# Generic Type Parameter */
struct cs_generic_param {
	char *name;
	struct cs_generic_param *constraints;
	struct cs_generic_param *next;
};

/* C# Type Information */
struct cs_type_info {
	enum cs_type_kind kind;
	char *name;
	struct cs_symbol *members;
	struct cs_generic_param *type_params;
	struct cs_type_info *base_type;
	struct cs_type_info **interfaces;
	int interface_count;

	/* For endian neutrality */
	int size;             /* Size in bytes (architecture neutral) */
	int alignment;        /* Alignment requirements */
	int is_blittable;     /* Can be memcpy'd across boundaries */
};

/* C# Lambda Expression */
struct cs_lambda {
	char **param_names;
	int param_count;
	struct cs_type_info **param_types;
	struct cs_type_info *return_type;
	P1ND *body;
	int is_expression;    /* Expression lambda vs statement lambda */
};

/* C# LINQ Query */
struct cs_query {
	char *range_var;
	P1ND *data_source;
	P1ND *where_clause;
	P1ND *select_clause;
	P1ND *orderby_clause;
	P1ND *join_clause;
	P1ND *group_clause;
};

/* C# Anonymous Type */
struct cs_anonymous_type {
	char **field_names;
	struct cs_type_info **field_types;
	int field_count;
	char *generated_name;  /* Compiler-generated name */
};

/* Function Prototypes */

/* Initialization */
void cs_init(void);
void cs_cleanup(void);

/* Symbol Table */
struct cs_symbol *cs_sym_lookup(const char *name);
struct cs_symbol *cs_sym_insert(const char *name, enum cs_type_kind type);
void cs_sym_push_scope(void);
void cs_sym_pop_scope(void);

/* Type System */
struct cs_type_info *cs_type_create(enum cs_type_kind kind);
int cs_type_compatible(struct cs_type_info *t1, struct cs_type_info *t2);
int cs_type_is_reference(struct cs_type_info *type);
int cs_type_is_value(struct cs_type_info *type);
struct cs_type_info *cs_type_infer(P1ND *expr); /* For 'var' keyword */

/* Lambda Expressions */
struct cs_lambda *cs_lambda_create(void);
P1ND *cs_lambda_compile(struct cs_lambda *lambda);

/* LINQ */
struct cs_query *cs_query_create(void);
P1ND *cs_query_compile(struct cs_query *query);

/* Anonymous Types */
struct cs_anonymous_type *cs_anon_type_create(void);
char *cs_anon_type_generate_name(struct cs_anonymous_type *type);

/* ARC Integration for C# Reference Types */
void cs_arc_init(void);
P1ND *cs_arc_handle_assign(P1ND *dest, P1ND *src);
P1ND *cs_arc_handle_return(P1ND *expr);
void cs_arc_var_declared(struct cs_symbol *var);
P1ND *cs_arc_scope_cleanup(void);

/* Extension Methods */
int cs_is_extension_method(struct cs_symbol *method);
P1ND *cs_resolve_extension_call(P1ND *receiver, const char *method_name);

/* Object and Collection Initializers */
P1ND *cs_handle_object_initializer(P1ND *obj, P1ND *init_list);
P1ND *cs_handle_collection_initializer(P1ND *collection, P1ND *init_list);

/* Auto-implemented Properties */
P1ND *cs_generate_auto_property(struct cs_symbol *prop);

/* Partial Types */
void cs_register_partial_type(struct cs_symbol *type);
void cs_merge_partial_types(void);

/* Nullable Types */
struct cs_type_info *cs_make_nullable(struct cs_type_info *base_type);
int cs_is_nullable(struct cs_type_info *type);

/* Global State */
extern int cs_arc_enabled;
extern int cs_language_version;
extern struct cs_symbol *cs_current_scope;

#endif /* _CS_PASS1_H_ */
