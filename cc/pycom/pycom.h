/*
 * Python Compiler Frontend for PCC
 * Header file with data structures and declarations
 */

#ifndef PYCOM_H
#define PYCOM_H

#include "config.h"
#include "manifest.h"
#include "node.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* Token types */
typedef enum {
    TOK_EOF = 0,
    TOK_NEWLINE,
    TOK_INDENT,
    TOK_DEDENT,

    /* Literals */
    TOK_NUMBER,
    TOK_STRING,
    TOK_IDENTIFIER,

    /* Keywords */
    TOK_DEF,
    TOK_CLASS,
    TOK_IF,
    TOK_ELIF,
    TOK_ELSE,
    TOK_WHILE,
    TOK_FOR,
    TOK_IN,
    TOK_RETURN,
    TOK_PASS,
    TOK_BREAK,
    TOK_CONTINUE,
    TOK_IMPORT,
    TOK_FROM,
    TOK_AS,
    TOK_AND,
    TOK_OR,
    TOK_NOT,
    TOK_TRUE,
    TOK_FALSE,
    TOK_NONE,
    TOK_LAMBDA,
    TOK_TRY,
    TOK_EXCEPT,
    TOK_FINALLY,
    TOK_RAISE,
    TOK_WITH,
    TOK_YIELD,
    TOK_ASSERT,
    TOK_GLOBAL,
    TOK_NONLOCAL,
    TOK_DEL,
    TOK_IS,
    TOK_PRINT,          /* Python 2 print statement */
    TOK_EXEC,           /* Python 2 exec statement */
    TOK_XRANGE,         /* Python 2 xrange */

    /* Operators */
    TOK_PLUS,           /* + */
    TOK_MINUS,          /* - */
    TOK_STAR,           /* * */
    TOK_SLASH,          /* / */
    TOK_PERCENT,        /* % */
    TOK_POWER,          /* ** */
    TOK_FLOOR_DIV,      /* // */
    TOK_EQ,             /* == */
    TOK_NE,             /* != */
    TOK_LT,             /* < */
    TOK_LE,             /* <= */
    TOK_GT,             /* > */
    TOK_GE,             /* >= */
    TOK_ASSIGN,         /* = */
    TOK_PLUS_ASSIGN,    /* += */
    TOK_MINUS_ASSIGN,   /* -= */
    TOK_STAR_ASSIGN,    /* *= */
    TOK_SLASH_ASSIGN,   /* /= */
    TOK_LSHIFT,         /* << */
    TOK_RSHIFT,         /* >> */
    TOK_AMPERSAND,      /* & */
    TOK_PIPE,           /* | */
    TOK_CARET,          /* ^ */
    TOK_TILDE,          /* ~ */

    /* Delimiters */
    TOK_LPAREN,         /* ( */
    TOK_RPAREN,         /* ) */
    TOK_LBRACKET,       /* [ */
    TOK_RBRACKET,       /* ] */
    TOK_LBRACE,         /* { */
    TOK_RBRACE,         /* } */
    TOK_COMMA,          /* , */
    TOK_COLON,          /* : */
    TOK_SEMICOLON,      /* ; */
    TOK_DOT,            /* . */
    TOK_ARROW,          /* -> */
} TokenType;

/* Token structure */
typedef struct Token {
    TokenType type;
    char *value;
    int line;
    int column;
    union {
        long long int_val;
        double float_val;
    } literal;
} Token;

/* Python version */
typedef enum {
    PYTHON_VERSION_2 = 2,
    PYTHON_VERSION_3 = 3
} PythonVersion;

/* Lexer state */
typedef struct Lexer {
    const char *source;
    size_t pos;
    size_t line;
    size_t column;
    int *indent_stack;
    int indent_count;
    int indent_capacity;
    Token current;
    Token lookahead;
    int pending_dedents;
    PythonVersion version;
} Lexer;

/* AST Node types */
typedef enum {
    AST_MODULE,
    AST_FUNCTION_DEF,
    AST_CLASS_DEF,
    AST_RETURN,
    AST_IF,
    AST_WHILE,
    AST_FOR,
    AST_ASSIGN,
    AST_EXPR_STMT,
    AST_PASS,
    AST_BREAK,
    AST_CONTINUE,
    AST_PRINT,          /* Python 2 print statement */
    AST_EXEC,           /* Python 2 exec statement */

    /* Expressions */
    AST_BINOP,
    AST_UNOP,
    AST_CALL,
    AST_NAME,
    AST_NUMBER,
    AST_STRING,
    AST_LIST,
    AST_DICT,
    AST_SUBSCRIPT,
    AST_ATTRIBUTE,
    AST_COMPARE,
} ASTNodeType;

/* AST Node structure */
typedef struct ASTNode {
    ASTNodeType type;
    int line;

    union {
        struct {
            char *name;
            struct ASTNode **params;
            int param_count;
            struct ASTNode **body;
            int body_count;
        } func_def;

        struct {
            char *name;
            struct ASTNode **bases;
            int base_count;
            struct ASTNode **body;
            int body_count;
        } class_def;

        struct {
            struct ASTNode *expr;
        } return_stmt;

        struct {
            struct ASTNode *test;
            struct ASTNode **body;
            int body_count;
            struct ASTNode **orelse;
            int orelse_count;
        } if_stmt;

        struct {
            struct ASTNode *test;
            struct ASTNode **body;
            int body_count;
        } while_stmt;

        struct {
            char *target;
            struct ASTNode *iter;
            struct ASTNode **body;
            int body_count;
        } for_stmt;

        struct {
            struct ASTNode **targets;
            int target_count;
            struct ASTNode *value;
        } assign;

        struct {
            TokenType op;
            struct ASTNode *left;
            struct ASTNode *right;
        } binop;

        struct {
            TokenType op;
            struct ASTNode *operand;
        } unop;

        struct {
            struct ASTNode *func;
            struct ASTNode **args;
            int arg_count;
        } call;

        struct {
            char *id;
        } name;

        struct {
            long long value;
        } number;

        struct {
            char *value;
        } string;

        struct {
            struct ASTNode **elements;
            int element_count;
        } list;

        struct {
            struct ASTNode *object;
            struct ASTNode *index;
        } subscript;

        struct {
            struct ASTNode *object;
            char *attr;
        } attribute;

        struct {
            struct ASTNode **values;
            int value_count;
            int newline;  /* 1 for newline, 0 for no newline */
        } print_stmt;

        struct {
            struct ASTNode *code;
        } exec_stmt;
    } data;
} ASTNode;

/* Symbol table entry */
typedef struct Symbol {
    char *name;
    TWORD type;
    int offset;
    int is_function;
    int is_parameter;
    struct Symbol *next;
} Symbol;

/* Symbol table */
typedef struct SymbolTable {
    Symbol *symbols;
    struct SymbolTable *parent;
    int current_offset;
} SymbolTable;

/* Parser state */
typedef struct Parser {
    Lexer *lexer;
    Token current;
    SymbolTable *symtab;
    int label_count;
    int temp_count;
    PythonVersion version;
} Parser;

/* Function declarations */

/* Lexer functions */
Lexer *lexer_new(const char *source, PythonVersion version);
void lexer_free(Lexer *lexer);
Token lexer_next_token(Lexer *lexer);
Token lexer_peek_token(Lexer *lexer);

/* Parser functions */
Parser *parser_new(Lexer *lexer);
void parser_free(Parser *parser);
ASTNode *parse_module(Parser *parser);
ASTNode *parse_statement(Parser *parser);
ASTNode *parse_expression(Parser *parser);

/* AST functions */
ASTNode *ast_new(ASTNodeType type);
void ast_free(ASTNode *node);

/* Symbol table functions */
SymbolTable *symtab_new(SymbolTable *parent);
void symtab_free(SymbolTable *symtab);
Symbol *symtab_lookup(SymbolTable *symtab, const char *name);
Symbol *symtab_insert(SymbolTable *symtab, const char *name, TWORD type);

/* IR generation functions */
NODE *codegen(ASTNode *ast, SymbolTable *symtab);
NODE *codegen_stmt(ASTNode *stmt, SymbolTable *symtab);
NODE *codegen_expr(ASTNode *expr, SymbolTable *symtab);

/* Utility functions */
void error(const char *fmt, ...);
void *xmalloc(size_t size);
void *xrealloc(void *ptr, size_t size);
char *xstrdup(const char *str);

/* Global variables */
extern FILE *output_file;
extern int error_count;
extern int line_number;
extern PythonVersion python_version;

#endif /* PYCOM_H */
