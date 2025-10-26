/*	$Id$	*/
/*
 * Copyright (c) 2025 JavaScript Frontend
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * JavaScript Compiler Frontend - Main Header
 *
 * This file defines the AST structure, types, and interfaces for the
 * JavaScript frontend that compiles JavaScript/TypeScript/CoffeeScript/
 * LiveScript code to PCC IR, which can then target x86, ARM, WASM, etc.
 */

#ifndef _JSPASS1_H
#define _JSPASS1_H

#include "config.h"
#include <sys/types.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

/* Include PCC infrastructure */
#include "manifest.h"
#include "external.h"

/*
 * JavaScript Language Features
 * These flags control which language features are enabled
 */

/* ECMAScript Versions */
#define JS_ES3		0x0001		/* ECMAScript 3 (1999) */
#define JS_ES5		0x0002		/* ECMAScript 5 (2009) */
#define JS_ES6		0x0004		/* ECMAScript 2015 (ES6) */
#define JS_ES2016	0x0008		/* ECMAScript 2016 */
#define JS_ES2017	0x0010		/* ECMAScript 2017 (async/await) */
#define JS_ES2018	0x0020		/* ECMAScript 2018 */
#define JS_ES2019	0x0040		/* ECMAScript 2019 */
#define JS_ES2020	0x0080		/* ECMAScript 2020 (BigInt, ??, ?.) */
#define JS_ES2021	0x0100		/* ECMAScript 2021 */
#define JS_ES2022	0x0200		/* ECMAScript 2022 */
#define JS_ES2023	0x0400		/* ECMAScript 2023 */
#define JS_ES2024	0x0800		/* ECMAScript 2024 */
#define JS_ES2025	0x1000		/* ECMAScript 2025 */

/* Language Extensions */
#define JS_TYPESCRIPT	0x10000		/* TypeScript features */
#define JS_ACTIONSCRIPT	0x20000		/* ActionScript features */
#define JS_COFFEESCRIPT	0x40000		/* CoffeeScript features */
#define JS_LIVESCRIPT	0x80000		/* LiveScript features */

/* Feature flags */
#define JS_STRICT_MODE	0x100000	/* 'use strict' */
#define JS_MODULES	0x200000	/* ES6 modules */
#define JS_JSX		0x400000	/* JSX syntax (React) */

extern int js_lang_features;		/* Current language feature set */

/*
 * JavaScript Types
 * JavaScript is dynamically typed, but we track types for optimization
 * and type inference. TypeScript annotations provide explicit types.
 */

/* Base JavaScript types */
typedef enum {
	JS_TYPE_UNDEFINED,	/* undefined */
	JS_TYPE_NULL,		/* null */
	JS_TYPE_BOOLEAN,	/* boolean (true/false) */
	JS_TYPE_NUMBER,		/* Number (IEEE 754 double) */
	JS_TYPE_BIGINT,		/* BigInt (ES2020+) */
	JS_TYPE_STRING,		/* String (UTF-16) */
	JS_TYPE_SYMBOL,		/* Symbol (ES6+) */
	JS_TYPE_OBJECT,		/* Object (including arrays, functions) */
	JS_TYPE_FUNCTION,	/* Function (callable object) */
	JS_TYPE_ARRAY,		/* Array (special object) */
	JS_TYPE_ANY,		/* any (TypeScript: unknown type) */
	JS_TYPE_VOID,		/* void (TypeScript: no return value) */
	JS_TYPE_NEVER,		/* never (TypeScript: unreachable) */
	JS_TYPE_UNKNOWN,	/* unknown (TypeScript: type-safe any) */

	/* Inferred/specialized types for optimization */
	JS_TYPE_INT32,		/* 32-bit signed integer */
	JS_TYPE_UINT32,		/* 32-bit unsigned integer */
	JS_TYPE_FLOAT64,	/* Explicit double (for optimization) */

	/* Composite types */
	JS_TYPE_UNION,		/* Union type (TypeScript: A | B) */
	JS_TYPE_INTERSECTION,	/* Intersection type (TypeScript: A & B) */
	JS_TYPE_TUPLE,		/* Tuple (TypeScript: [string, number]) */

	/* Special types */
	JS_TYPE_GENERIC,	/* Generic type parameter (TypeScript) */
	JS_TYPE_PROMISE,	/* Promise<T> (ES6+) */
	JS_TYPE_ITERATOR,	/* Iterator/Iterable */
	JS_TYPE_GENERATOR,	/* Generator function */

} js_type_t;

/*
 * JavaScript AST Node Types
 * Represents all JavaScript syntactic constructs
 */

typedef enum {
	/* Literals */
	JS_NODE_LITERAL,	/* Literal value (number, string, bool, null) */
	JS_NODE_IDENTIFIER,	/* Variable/function name */
	JS_NODE_THIS,		/* this keyword */
	JS_NODE_SUPER,		/* super keyword (ES6) */

	/* Expressions */
	JS_NODE_BINARY_OP,	/* Binary operation (+, -, *, /, etc.) */
	JS_NODE_UNARY_OP,	/* Unary operation (!, -, +, ~, typeof, etc.) */
	JS_NODE_ASSIGN,		/* Assignment (=, +=, -=, etc.) */
	JS_NODE_UPDATE,		/* ++, -- */
	JS_NODE_LOGICAL,	/* &&, || */
	JS_NODE_CONDITIONAL,	/* ternary ?: */
	JS_NODE_CALL,		/* Function call */
	JS_NODE_NEW,		/* new expression */
	JS_NODE_MEMBER,		/* Member access (obj.prop, obj[prop]) */
	JS_NODE_ARRAY_EXPR,	/* Array literal [...] */
	JS_NODE_OBJECT_EXPR,	/* Object literal {...} */
	JS_NODE_FUNCTION_EXPR,	/* Function expression */
	JS_NODE_ARROW_FUNC,	/* Arrow function (ES6) */
	JS_NODE_CLASS_EXPR,	/* Class expression (ES6) */
	JS_NODE_TEMPLATE_LIT,	/* Template literal `...` (ES6) */
	JS_NODE_TAGGED_TEMPLATE, /* Tagged template func`...` (ES6) */
	JS_NODE_SEQUENCE,	/* Comma expression */
	JS_NODE_SPREAD,		/* Spread operator ... (ES6) */
	JS_NODE_YIELD,		/* yield expression (ES6) */
	JS_NODE_AWAIT,		/* await expression (ES2017) */
	JS_NODE_IMPORT,		/* import() expression */
	JS_NODE_META_PROPERTY,	/* new.target, import.meta */
	JS_NODE_CHAIN,		/* Optional chaining ?. (ES2020) */
	JS_NODE_COALESCE,	/* Nullish coalescing ?? (ES2020) */

	/* Statements */
	JS_NODE_BLOCK,		/* Block statement {...} */
	JS_NODE_EMPTY,		/* Empty statement ; */
	JS_NODE_EXPR_STMT,	/* Expression statement */
	JS_NODE_IF,		/* if statement */
	JS_NODE_SWITCH,		/* switch statement */
	JS_NODE_WHILE,		/* while loop */
	JS_NODE_DO_WHILE,	/* do-while loop */
	JS_NODE_FOR,		/* for loop */
	JS_NODE_FOR_IN,		/* for-in loop */
	JS_NODE_FOR_OF,		/* for-of loop (ES6) */
	JS_NODE_BREAK,		/* break statement */
	JS_NODE_CONTINUE,	/* continue statement */
	JS_NODE_RETURN,		/* return statement */
	JS_NODE_THROW,		/* throw statement */
	JS_NODE_TRY,		/* try-catch-finally */
	JS_NODE_WITH,		/* with statement (deprecated) */
	JS_NODE_LABELED,	/* Labeled statement */
	JS_NODE_DEBUGGER,	/* debugger statement */

	/* Declarations */
	JS_NODE_VAR_DECL,	/* var declaration */
	JS_NODE_LET_DECL,	/* let declaration (ES6) */
	JS_NODE_CONST_DECL,	/* const declaration (ES6) */
	JS_NODE_FUNC_DECL,	/* Function declaration */
	JS_NODE_CLASS_DECL,	/* Class declaration (ES6) */
	JS_NODE_IMPORT_DECL,	/* import declaration (ES6) */
	JS_NODE_EXPORT_DECL,	/* export declaration (ES6) */

	/* Patterns (destructuring) */
	JS_NODE_ARRAY_PATTERN,	/* Array destructuring [a, b] = ... */
	JS_NODE_OBJECT_PATTERN,	/* Object destructuring {a, b} = ... */
	JS_NODE_REST_ELEMENT,	/* Rest element ...rest */

	/* Class members */
	JS_NODE_METHOD,		/* Class method */
	JS_NODE_PROPERTY,	/* Class property */
	JS_NODE_ACCESSOR,	/* Getter/setter */
	JS_NODE_CONSTRUCTOR,	/* Constructor */
	JS_NODE_STATIC_BLOCK,	/* Static initialization block (ES2022) */

	/* TypeScript-specific */
	JS_NODE_TS_TYPE_ANNOTATION,	/* Type annotation : type */
	JS_NODE_TS_INTERFACE,	/* Interface declaration */
	JS_NODE_TS_TYPE_ALIAS,	/* Type alias */
	JS_NODE_TS_ENUM,	/* Enum declaration */
	JS_NODE_TS_NAMESPACE,	/* Namespace */
	JS_NODE_TS_TYPE_PARAM,	/* Type parameter <T> */
	JS_NODE_TS_DECORATOR,	/* Decorator @decorator */

	/* Program structure */
	JS_NODE_PROGRAM,	/* Top-level program */
	JS_NODE_MODULE,		/* ES6 module */

} js_node_type_t;

/*
 * JavaScript Operators
 */

typedef enum {
	/* Arithmetic */
	JS_OP_ADD,		/* + */
	JS_OP_SUB,		/* - */
	JS_OP_MUL,		/* * */
	JS_OP_DIV,		/* / */
	JS_OP_MOD,		/* % */
	JS_OP_EXP,		/* ** (ES2016) */

	/* Bitwise */
	JS_OP_BIT_AND,		/* & */
	JS_OP_BIT_OR,		/* | */
	JS_OP_BIT_XOR,		/* ^ */
	JS_OP_BIT_NOT,		/* ~ */
	JS_OP_LSHIFT,		/* << */
	JS_OP_RSHIFT,		/* >> */
	JS_OP_URSHIFT,		/* >>> */

	/* Logical */
	JS_OP_AND,		/* && */
	JS_OP_OR,		/* || */
	JS_OP_NOT,		/* ! */
	JS_OP_NULLISH,		/* ?? (ES2020) */

	/* Comparison */
	JS_OP_EQ,		/* == */
	JS_OP_NE,		/* != */
	JS_OP_STRICT_EQ,	/* === */
	JS_OP_STRICT_NE,	/* !== */
	JS_OP_LT,		/* < */
	JS_OP_LE,		/* <= */
	JS_OP_GT,		/* > */
	JS_OP_GE,		/* >= */

	/* Unary */
	JS_OP_TYPEOF,		/* typeof */
	JS_OP_VOID,		/* void */
	JS_OP_DELETE,		/* delete */
	JS_OP_PLUS,		/* unary + */
	JS_OP_MINUS,		/* unary - */
	JS_OP_INC,		/* ++ */
	JS_OP_DEC,		/* -- */

	/* Assignment */
	JS_OP_ASSIGN,		/* = */
	JS_OP_ADD_ASSIGN,	/* += */
	JS_OP_SUB_ASSIGN,	/* -= */
	JS_OP_MUL_ASSIGN,	/* *= */
	JS_OP_DIV_ASSIGN,	/* /= */
	JS_OP_MOD_ASSIGN,	/* %= */
	JS_OP_EXP_ASSIGN,	/* **= (ES2016) */
	JS_OP_LSHIFT_ASSIGN,	/* <<= */
	JS_OP_RSHIFT_ASSIGN,	/* >>= */
	JS_OP_URSHIFT_ASSIGN,	/* >>>= */
	JS_OP_AND_ASSIGN,	/* &= */
	JS_OP_OR_ASSIGN,	/* |= */
	JS_OP_XOR_ASSIGN,	/* ^= */
	JS_OP_LOGICAL_AND_ASSIGN,	/* &&= (ES2021) */
	JS_OP_LOGICAL_OR_ASSIGN,	/* ||= (ES2021) */
	JS_OP_NULLISH_ASSIGN,	/* ??= (ES2021) */

	/* Other */
	JS_OP_INSTANCEOF,	/* instanceof */
	JS_OP_IN,		/* in */
	JS_OP_COMMA,		/* , */
	JS_OP_OPTIONAL_CHAIN,	/* ?. (ES2020) */
	JS_OP_PIPELINE,		/* |> (ES2025 proposal) */

} js_operator_t;

/*
 * JavaScript AST Node
 * Universal node structure for JavaScript syntax trees
 */

struct js_node;
struct js_type_info;
struct js_symbol;

typedef struct js_node {
	js_node_type_t type;		/* Node type */
	int lineno;			/* Source line number */
	int column;			/* Source column */
	char *filename;			/* Source filename */

	/* Type information (from inference or annotations) */
	struct js_type_info *typeinfo;

	/* Value (for literals) */
	union {
		double number;		/* Number literal */
		char *string;		/* String literal */
		int boolean;		/* Boolean literal */
		uint64_t bigint;	/* BigInt literal (ES2020) */
	} value;

	/* Identifier name */
	char *name;

	/* Operator (for operator nodes) */
	js_operator_t op;

	/* Children (most nodes have 0-3 children) */
	struct js_node *left;		/* Left child / first child */
	struct js_node *right;		/* Right child / second child */
	struct js_node *extra;		/* Third child (for ternary, etc.) */

	/* List of children (for blocks, arrays, objects, params, etc.) */
	struct js_node **children;
	int n_children;
	int children_capacity;

	/* Symbol table entry (for identifiers, declarations) */
	struct js_symbol *sym;

	/* Function-specific */
	struct js_node **params;	/* Function parameters */
	int n_params;
	struct js_node *body;		/* Function body */
	int is_generator;		/* Generator function? */
	int is_async;			/* Async function? */
	int is_arrow;			/* Arrow function? */

	/* Class-specific */
	struct js_node *superclass;	/* Extends clause */
	struct js_node **methods;	/* Class methods */
	int n_methods;

	/* Module-specific */
	struct js_node **imports;	/* Import declarations */
	int n_imports;
	struct js_node **exports;	/* Export declarations */
	int n_exports;

	/* Flags */
	unsigned int flags;
	#define JS_NODE_FLAG_STRICT	0x01	/* Strict mode */
	#define JS_NODE_FLAG_COMPUTED	0x02	/* Computed property */
	#define JS_NODE_FLAG_OPTIONAL	0x04	/* Optional (?) */
	#define JS_NODE_FLAG_SPREAD	0x08	/* Spread element */
	#define JS_NODE_FLAG_STATIC	0x10	/* Static member */
	#define JS_NODE_FLAG_PRIVATE	0x20	/* Private member (#) */
	#define JS_NODE_FLAG_GETTER	0x40	/* Getter */
	#define JS_NODE_FLAG_SETTER	0x80	/* Setter */

} js_node_t;

/*
 * Type information for type inference and checking
 */

typedef struct js_type_info {
	js_type_t base_type;		/* Base type */

	/* For union/intersection types */
	struct js_type_info **union_types;
	int n_union_types;

	/* For arrays, tuples */
	struct js_type_info *element_type;

	/* For functions */
	struct js_type_info **param_types;
	int n_param_types;
	struct js_type_info *return_type;

	/* For objects */
	struct js_property_type **properties;
	int n_properties;

	/* For generics */
	char **type_params;
	int n_type_params;

	/* TypeScript-specific */
	char *ts_type_string;		/* Original TypeScript type string */

	/* Inferred constraints */
	int is_nullable;		/* Can be null/undefined? */
	int is_readonly;		/* ReadOnly property? */

} js_type_info_t;

typedef struct js_property_type {
	char *name;			/* Property name */
	js_type_info_t *type;		/* Property type */
	int is_optional;		/* Optional property? */
	int is_readonly;		/* ReadOnly property? */
} js_property_type_t;

/*
 * Symbol Table
 * Tracks variables, functions, classes, and their scopes
 */

typedef enum {
	JS_SCOPE_GLOBAL,	/* Global scope */
	JS_SCOPE_FUNCTION,	/* Function scope (var) */
	JS_SCOPE_BLOCK,		/* Block scope (let/const) */
	JS_SCOPE_MODULE,	/* Module scope (ES6) */
	JS_SCOPE_CLASS,		/* Class scope */
	JS_SCOPE_WITH,		/* With statement scope (deprecated) */
	JS_SCOPE_CATCH,		/* Catch block scope */
} js_scope_type_t;

typedef struct js_scope {
	js_scope_type_t type;		/* Scope type */
	struct js_scope *parent;	/* Parent scope */
	struct js_symbol **symbols;	/* Symbols in this scope */
	int n_symbols;
	int symbols_capacity;
	int is_strict;			/* Strict mode? */
} js_scope_t;

typedef enum {
	JS_SYM_VAR,		/* var variable */
	JS_SYM_LET,		/* let variable */
	JS_SYM_CONST,		/* const variable */
	JS_SYM_FUNCTION,	/* function */
	JS_SYM_CLASS,		/* class */
	JS_SYM_PARAM,		/* function parameter */
	JS_SYM_IMPORT,		/* imported binding */
	JS_SYM_CATCH,		/* catch parameter */
	JS_SYM_TYPE,		/* TypeScript type */
	JS_SYM_INTERFACE,	/* TypeScript interface */
	JS_SYM_NAMESPACE,	/* TypeScript namespace */
} js_symbol_kind_t;

typedef struct js_symbol {
	char *name;			/* Symbol name */
	js_symbol_kind_t kind;		/* Symbol kind */
	js_type_info_t *type;		/* Type information */
	js_scope_t *scope;		/* Declaring scope */
	struct js_node *decl_node;	/* Declaration node */

	/* For code generation */
	int pcc_regno;			/* Mapped PCC register */
	int stack_offset;		/* Stack offset (if spilled) */
	int is_captured;		/* Captured by closure? */

	/* Usage tracking */
	int n_reads;			/* Number of reads */
	int n_writes;			/* Number of writes */
	int first_use_line;		/* First use line number */

	/* Flags */
	unsigned int flags;
	#define JS_SYM_FLAG_HOISTED	0x01	/* Hoisted (var/function) */
	#define JS_SYM_FLAG_EXPORTED	0x02	/* Exported */
	#define JS_SYM_FLAG_BUILTIN	0x04	/* Built-in */
	#define JS_SYM_FLAG_USED	0x08	/* Used */

} js_symbol_t;

/*
 * Compiler Context
 * Global state for the JavaScript compiler
 */

typedef struct js_compiler_ctx {
	/* Source file info */
	char *filename;
	FILE *input;
	int lineno;
	int column;

	/* Language features */
	int lang_features;		/* JS_ES*, JS_TYPESCRIPT, etc. */
	int strict_mode;		/* Global strict mode? */
	int is_module;			/* ES6 module? */

	/* AST */
	js_node_t *ast_root;		/* Root of AST */

	/* Symbol tables */
	js_scope_t *current_scope;	/* Current scope */
	js_scope_t *global_scope;	/* Global scope */

	/* Type checking */
	int enable_type_checking;	/* Enable type checking? */
	int n_type_errors;		/* Number of type errors */

	/* Code generation */
	FILE *output;			/* Output file (PCC IR) */
	int optimize_level;		/* Optimization level (0-3) */

	/* Error tracking */
	int n_errors;			/* Number of errors */
	int n_warnings;			/* Number of warnings */

	/* Runtime support */
	int need_gc;			/* Need garbage collector? */
	int need_object_runtime;	/* Need object system runtime? */
	int need_bigint_runtime;	/* Need BigInt runtime? */

} js_compiler_ctx_t;

/*
 * Function prototypes
 */

/* Lexer (jscan.l) */
extern int jslex(void);
extern void js_init_lexer(FILE *input);

/* Parser (jsgram.y) */
extern int jsparse(void);
extern js_node_t *js_parse_file(const char *filename);

/* AST construction (jstree.c) */
extern js_node_t *js_node_create(js_node_type_t type);
extern js_node_t *js_node_literal_number(double value);
extern js_node_t *js_node_literal_string(const char *value);
extern js_node_t *js_node_literal_boolean(int value);
extern js_node_t *js_node_identifier(const char *name);
extern js_node_t *js_node_binary_op(js_operator_t op, js_node_t *left, js_node_t *right);
extern js_node_t *js_node_unary_op(js_operator_t op, js_node_t *operand);
extern void js_node_add_child(js_node_t *parent, js_node_t *child);
extern void js_node_free(js_node_t *node);

/* Type inference (jstype.c) */
extern void js_type_check(js_node_t *node, js_scope_t *scope);
extern js_type_info_t *js_infer_type(js_node_t *node);
extern int js_types_compatible(js_type_info_t *t1, js_type_info_t *t2);
extern const char *js_type_to_string(js_type_info_t *type);

/* Symbol table (jssymbol.c) */
extern js_scope_t *js_scope_create(js_scope_type_t type, js_scope_t *parent);
extern js_symbol_t *js_symbol_create(const char *name, js_symbol_kind_t kind);
extern void js_scope_add_symbol(js_scope_t *scope, js_symbol_t *sym);
extern js_symbol_t *js_scope_lookup(js_scope_t *scope, const char *name);
extern js_symbol_t *js_scope_lookup_local(js_scope_t *scope, const char *name);

/* PCC IR emission (jsemit.c) */
extern void js_emit_program(js_node_t *program, FILE *output);
extern void js_emit_function(js_node_t *func, FILE *output);
extern void js_emit_expression(js_node_t *expr, FILE *output);

/* Built-ins (jsbuiltin.c) */
extern void js_register_builtins(js_scope_t *global_scope);
extern int js_is_builtin_function(const char *name);

/* Runtime support (jsruntime.c) */
extern void js_emit_runtime_init(FILE *output);
extern void js_emit_runtime_cleanup(FILE *output);

/* Error handling */
extern void js_error(const char *fmt, ...);
extern void js_warning(const char *fmt, ...);
extern void js_error_at(int line, int col, const char *fmt, ...);

/* Compiler context */
extern js_compiler_ctx_t *js_compiler_ctx;
extern void js_compiler_init(void);
extern void js_compiler_cleanup(void);

/* Utilities */
extern char *js_strdup(const char *s);
extern void *js_malloc(size_t size);
extern void *js_realloc(void *ptr, size_t size);
extern void js_free(void *ptr);

#endif /* _JSPASS1_H */
