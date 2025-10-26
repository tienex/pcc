/*
 * Python Parser Implementation
 * Converts tokens into Abstract Syntax Tree (AST)
 */

#include "pycom.h"
#include <stdlib.h>
#include <string.h>

/* Forward declarations */
static ASTNode *parse_stmt(Parser *parser);
static ASTNode *parse_expr(Parser *parser);
static ASTNode *parse_primary(Parser *parser);

Parser *parser_new(Lexer *lexer) {
    Parser *parser = xmalloc(sizeof(Parser));
    parser->lexer = lexer;
    parser->current = lexer_next_token(lexer);
    parser->symtab = symtab_new(NULL);
    parser->label_count = 0;
    parser->temp_count = 0;
    parser->version = lexer->version;
    return parser;
}

void parser_free(Parser *parser) {
    if (parser) {
        symtab_free(parser->symtab);
        free(parser);
    }
}

static void parser_advance(Parser *parser) {
    if (parser->current.value) {
        free(parser->current.value);
    }
    parser->current = lexer_next_token(parser->lexer);
}

static int parser_match(Parser *parser, TokenType type) {
    return parser->current.type == type;
}

static void parser_expect(Parser *parser, TokenType type) {
    if (parser->current.type != type) {
        error("Expected token type %d but got %d at line %d",
              type, parser->current.type, parser->current.line);
    }
    parser_advance(parser);
}

static void parser_skip_newlines(Parser *parser) {
    while (parser->current.type == TOK_NEWLINE) {
        parser_advance(parser);
    }
}

ASTNode *ast_new(ASTNodeType type) {
    ASTNode *node = xmalloc(sizeof(ASTNode));
    memset(node, 0, sizeof(ASTNode));
    node->type = type;
    return node;
}

void ast_free(ASTNode *node) {
    if (!node) return;

    /* Free type-specific data */
    switch (node->type) {
        case AST_FUNCTION_DEF:
            free(node->data.func_def.name);
            for (int i = 0; i < node->data.func_def.param_count; i++) {
                ast_free(node->data.func_def.params[i]);
            }
            free(node->data.func_def.params);
            for (int i = 0; i < node->data.func_def.body_count; i++) {
                ast_free(node->data.func_def.body[i]);
            }
            free(node->data.func_def.body);
            break;

        case AST_BINOP:
            ast_free(node->data.binop.left);
            ast_free(node->data.binop.right);
            break;

        case AST_UNOP:
            ast_free(node->data.unop.operand);
            break;

        case AST_NAME:
            free(node->data.name.id);
            break;

        case AST_STRING:
            free(node->data.string.value);
            break;

        case AST_ASSIGN:
            for (int i = 0; i < node->data.assign.target_count; i++) {
                ast_free(node->data.assign.targets[i]);
            }
            free(node->data.assign.targets);
            ast_free(node->data.assign.value);
            break;

        case AST_RETURN:
            ast_free(node->data.return_stmt.expr);
            break;

        case AST_IF:
            ast_free(node->data.if_stmt.test);
            for (int i = 0; i < node->data.if_stmt.body_count; i++) {
                ast_free(node->data.if_stmt.body[i]);
            }
            free(node->data.if_stmt.body);
            for (int i = 0; i < node->data.if_stmt.orelse_count; i++) {
                ast_free(node->data.if_stmt.orelse[i]);
            }
            free(node->data.if_stmt.orelse);
            break;

        case AST_WHILE:
            ast_free(node->data.while_stmt.test);
            for (int i = 0; i < node->data.while_stmt.body_count; i++) {
                ast_free(node->data.while_stmt.body[i]);
            }
            free(node->data.while_stmt.body);
            break;

        case AST_PRINT:
            for (int i = 0; i < node->data.print_stmt.value_count; i++) {
                ast_free(node->data.print_stmt.values[i]);
            }
            free(node->data.print_stmt.values);
            break;

        case AST_EXEC:
            ast_free(node->data.exec_stmt.code);
            break;

        default:
            break;
    }

    free(node);
}

/* Parse primary expressions */
static ASTNode *parse_primary(Parser *parser) {
    ASTNode *node = NULL;

    if (parser_match(parser, TOK_NUMBER)) {
        node = ast_new(AST_NUMBER);
        node->data.number.value = parser->current.literal.int_val;
        parser_advance(parser);
        return node;
    }

    if (parser_match(parser, TOK_STRING)) {
        node = ast_new(AST_STRING);
        node->data.string.value = xstrdup(parser->current.value);
        parser_advance(parser);
        return node;
    }

    if (parser_match(parser, TOK_IDENTIFIER)) {
        node = ast_new(AST_NAME);
        node->data.name.id = xstrdup(parser->current.value);
        parser_advance(parser);

        /* Check for function call */
        if (parser_match(parser, TOK_LPAREN)) {
            ASTNode *call = ast_new(AST_CALL);
            call->data.call.func = node;
            parser_advance(parser); /* ( */

            /* Parse arguments */
            int arg_count = 0;
            int arg_capacity = 4;
            ASTNode **args = xmalloc(sizeof(ASTNode*) * arg_capacity);

            while (!parser_match(parser, TOK_RPAREN) && !parser_match(parser, TOK_EOF)) {
                if (arg_count >= arg_capacity) {
                    arg_capacity *= 2;
                    args = xrealloc(args, sizeof(ASTNode*) * arg_capacity);
                }
                args[arg_count++] = parse_expr(parser);

                if (parser_match(parser, TOK_COMMA)) {
                    parser_advance(parser);
                }
            }

            parser_expect(parser, TOK_RPAREN);

            call->data.call.args = args;
            call->data.call.arg_count = arg_count;
            return call;
        }

        return node;
    }

    if (parser_match(parser, TOK_LPAREN)) {
        parser_advance(parser);
        node = parse_expr(parser);
        parser_expect(parser, TOK_RPAREN);
        return node;
    }

    if (parser_match(parser, TOK_TRUE)) {
        node = ast_new(AST_NUMBER);
        node->data.number.value = 1;
        parser_advance(parser);
        return node;
    }

    if (parser_match(parser, TOK_FALSE)) {
        node = ast_new(AST_NUMBER);
        node->data.number.value = 0;
        parser_advance(parser);
        return node;
    }

    if (parser_match(parser, TOK_NONE)) {
        node = ast_new(AST_NUMBER);
        node->data.number.value = 0;
        parser_advance(parser);
        return node;
    }

    error("Unexpected token in expression at line %d", parser->current.line);
    return NULL;
}

/* Parse unary expressions */
static ASTNode *parse_unary(Parser *parser) {
    if (parser_match(parser, TOK_MINUS) ||
        parser_match(parser, TOK_NOT) ||
        parser_match(parser, TOK_TILDE)) {
        ASTNode *node = ast_new(AST_UNOP);
        node->data.unop.op = parser->current.type;
        parser_advance(parser);
        node->data.unop.operand = parse_unary(parser);
        return node;
    }

    return parse_primary(parser);
}

/* Parse multiplicative expressions */
static ASTNode *parse_multiplicative(Parser *parser) {
    ASTNode *left = parse_unary(parser);

    while (parser_match(parser, TOK_STAR) ||
           parser_match(parser, TOK_SLASH) ||
           parser_match(parser, TOK_PERCENT) ||
           parser_match(parser, TOK_FLOOR_DIV)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_unary(parser);
        left = node;
    }

    return left;
}

/* Parse additive expressions */
static ASTNode *parse_additive(Parser *parser) {
    ASTNode *left = parse_multiplicative(parser);

    while (parser_match(parser, TOK_PLUS) ||
           parser_match(parser, TOK_MINUS)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_multiplicative(parser);
        left = node;
    }

    return left;
}

/* Parse shift expressions */
static ASTNode *parse_shift(Parser *parser) {
    ASTNode *left = parse_additive(parser);

    while (parser_match(parser, TOK_LSHIFT) ||
           parser_match(parser, TOK_RSHIFT)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_additive(parser);
        left = node;
    }

    return left;
}

/* Parse bitwise AND expressions */
static ASTNode *parse_bitwise_and(Parser *parser) {
    ASTNode *left = parse_shift(parser);

    while (parser_match(parser, TOK_AMPERSAND)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_shift(parser);
        left = node;
    }

    return left;
}

/* Parse bitwise XOR expressions */
static ASTNode *parse_bitwise_xor(Parser *parser) {
    ASTNode *left = parse_bitwise_and(parser);

    while (parser_match(parser, TOK_CARET)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_bitwise_and(parser);
        left = node;
    }

    return left;
}

/* Parse bitwise OR expressions */
static ASTNode *parse_bitwise_or(Parser *parser) {
    ASTNode *left = parse_bitwise_xor(parser);

    while (parser_match(parser, TOK_PIPE)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_bitwise_xor(parser);
        left = node;
    }

    return left;
}

/* Parse comparison expressions */
static ASTNode *parse_comparison(Parser *parser) {
    ASTNode *left = parse_bitwise_or(parser);

    while (parser_match(parser, TOK_EQ) ||
           parser_match(parser, TOK_NE) ||
           parser_match(parser, TOK_LT) ||
           parser_match(parser, TOK_LE) ||
           parser_match(parser, TOK_GT) ||
           parser_match(parser, TOK_GE)) {
        ASTNode *node = ast_new(AST_COMPARE);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_bitwise_or(parser);
        left = node;
    }

    return left;
}

/* Parse logical expressions */
static ASTNode *parse_logical(Parser *parser) {
    ASTNode *left = parse_comparison(parser);

    while (parser_match(parser, TOK_AND) ||
           parser_match(parser, TOK_OR)) {
        ASTNode *node = ast_new(AST_BINOP);
        node->data.binop.op = parser->current.type;
        parser_advance(parser);
        node->data.binop.left = left;
        node->data.binop.right = parse_comparison(parser);
        left = node;
    }

    return left;
}

/* Parse expression */
static ASTNode *parse_expr(Parser *parser) {
    return parse_logical(parser);
}

/* Parse assignment or expression statement */
static ASTNode *parse_expr_or_assign(Parser *parser) {
    ASTNode *left = parse_expr(parser);

    if (parser_match(parser, TOK_ASSIGN)) {
        ASTNode *node = ast_new(AST_ASSIGN);
        parser_advance(parser);

        /* Store target */
        node->data.assign.targets = xmalloc(sizeof(ASTNode*));
        node->data.assign.targets[0] = left;
        node->data.assign.target_count = 1;

        /* Parse value */
        node->data.assign.value = parse_expr(parser);

        return node;
    }

    /* Just an expression statement */
    ASTNode *stmt = ast_new(AST_EXPR_STMT);
    stmt->data.return_stmt.expr = left;
    return stmt;
}

/* Parse statement block (suite) */
static ASTNode **parse_suite(Parser *parser, int *count) {
    int capacity = 8;
    int stmt_count = 0;
    ASTNode **stmts = xmalloc(sizeof(ASTNode*) * capacity);

    parser_expect(parser, TOK_COLON);
    parser_skip_newlines(parser);
    parser_expect(parser, TOK_INDENT);

    while (!parser_match(parser, TOK_DEDENT) && !parser_match(parser, TOK_EOF)) {
        parser_skip_newlines(parser);
        if (parser_match(parser, TOK_DEDENT)) break;

        if (stmt_count >= capacity) {
            capacity *= 2;
            stmts = xrealloc(stmts, sizeof(ASTNode*) * capacity);
        }

        stmts[stmt_count++] = parse_stmt(parser);
        parser_skip_newlines(parser);
    }

    if (parser_match(parser, TOK_DEDENT)) {
        parser_advance(parser);
    }

    *count = stmt_count;
    return stmts;
}

/* Parse function definition */
static ASTNode *parse_function_def(Parser *parser) {
    ASTNode *node = ast_new(AST_FUNCTION_DEF);

    parser_expect(parser, TOK_DEF);

    if (!parser_match(parser, TOK_IDENTIFIER)) {
        error("Expected function name at line %d", parser->current.line);
    }

    node->data.func_def.name = xstrdup(parser->current.value);
    parser_advance(parser);

    parser_expect(parser, TOK_LPAREN);

    /* Parse parameters */
    int param_count = 0;
    int param_capacity = 4;
    ASTNode **params = xmalloc(sizeof(ASTNode*) * param_capacity);

    while (!parser_match(parser, TOK_RPAREN) && !parser_match(parser, TOK_EOF)) {
        if (!parser_match(parser, TOK_IDENTIFIER)) {
            error("Expected parameter name at line %d", parser->current.line);
        }

        if (param_count >= param_capacity) {
            param_capacity *= 2;
            params = xrealloc(params, sizeof(ASTNode*) * param_capacity);
        }

        ASTNode *param = ast_new(AST_NAME);
        param->data.name.id = xstrdup(parser->current.value);
        params[param_count++] = param;

        parser_advance(parser);

        if (parser_match(parser, TOK_COMMA)) {
            parser_advance(parser);
        }
    }

    parser_expect(parser, TOK_RPAREN);

    node->data.func_def.params = params;
    node->data.func_def.param_count = param_count;

    /* Parse function body */
    int body_count;
    node->data.func_def.body = parse_suite(parser, &body_count);
    node->data.func_def.body_count = body_count;

    return node;
}

/* Parse if statement */
static ASTNode *parse_if_stmt(Parser *parser) {
    ASTNode *node = ast_new(AST_IF);

    parser_expect(parser, TOK_IF);

    /* Parse condition */
    node->data.if_stmt.test = parse_expr(parser);

    /* Parse body */
    int body_count;
    node->data.if_stmt.body = parse_suite(parser, &body_count);
    node->data.if_stmt.body_count = body_count;

    /* Parse else clause */
    if (parser_match(parser, TOK_ELSE)) {
        parser_advance(parser);
        int orelse_count;
        node->data.if_stmt.orelse = parse_suite(parser, &orelse_count);
        node->data.if_stmt.orelse_count = orelse_count;
    }

    return node;
}

/* Parse while statement */
static ASTNode *parse_while_stmt(Parser *parser) {
    ASTNode *node = ast_new(AST_WHILE);

    parser_expect(parser, TOK_WHILE);

    /* Parse condition */
    node->data.while_stmt.test = parse_expr(parser);

    /* Parse body */
    int body_count;
    node->data.while_stmt.body = parse_suite(parser, &body_count);
    node->data.while_stmt.body_count = body_count;

    return node;
}

/* Parse return statement */
static ASTNode *parse_return_stmt(Parser *parser) {
    ASTNode *node = ast_new(AST_RETURN);

    parser_expect(parser, TOK_RETURN);

    if (!parser_match(parser, TOK_NEWLINE) && !parser_match(parser, TOK_EOF)) {
        node->data.return_stmt.expr = parse_expr(parser);
    }

    return node;
}

/* Parse Python 2 print statement */
static ASTNode *parse_print_stmt(Parser *parser) {
    ASTNode *node = ast_new(AST_PRINT);

    parser_expect(parser, TOK_PRINT);

    /* Parse values to print */
    int capacity = 4;
    int value_count = 0;
    ASTNode **values = xmalloc(sizeof(ASTNode*) * capacity);

    while (!parser_match(parser, TOK_NEWLINE) && !parser_match(parser, TOK_EOF)) {
        if (value_count >= capacity) {
            capacity *= 2;
            values = xrealloc(values, sizeof(ASTNode*) * capacity);
        }

        values[value_count++] = parse_expr(parser);

        if (parser_match(parser, TOK_COMMA)) {
            parser_advance(parser);
        } else {
            break;
        }
    }

    node->data.print_stmt.values = values;
    node->data.print_stmt.value_count = value_count;
    node->data.print_stmt.newline = 1;  /* Default: print with newline */

    return node;
}

/* Parse statement */
static ASTNode *parse_stmt(Parser *parser) {
    parser_skip_newlines(parser);

    if (parser_match(parser, TOK_DEF)) {
        return parse_function_def(parser);
    }

    if (parser_match(parser, TOK_IF)) {
        return parse_if_stmt(parser);
    }

    if (parser_match(parser, TOK_WHILE)) {
        return parse_while_stmt(parser);
    }

    if (parser_match(parser, TOK_RETURN)) {
        return parse_return_stmt(parser);
    }

    /* Python 2 print statement */
    if (parser_match(parser, TOK_PRINT) && parser->version == PYTHON_VERSION_2) {
        return parse_print_stmt(parser);
    }

    if (parser_match(parser, TOK_PASS)) {
        parser_advance(parser);
        return ast_new(AST_PASS);
    }

    if (parser_match(parser, TOK_BREAK)) {
        parser_advance(parser);
        return ast_new(AST_BREAK);
    }

    if (parser_match(parser, TOK_CONTINUE)) {
        parser_advance(parser);
        return ast_new(AST_CONTINUE);
    }

    /* Expression or assignment */
    return parse_expr_or_assign(parser);
}

/* Parse module (top-level) */
ASTNode *parse_module(Parser *parser) {
    ASTNode *module = ast_new(AST_MODULE);

    int capacity = 16;
    int stmt_count = 0;
    ASTNode **stmts = xmalloc(sizeof(ASTNode*) * capacity);

    parser_skip_newlines(parser);

    while (!parser_match(parser, TOK_EOF)) {
        if (stmt_count >= capacity) {
            capacity *= 2;
            stmts = xrealloc(stmts, sizeof(ASTNode*) * capacity);
        }

        stmts[stmt_count++] = parse_stmt(parser);
        parser_skip_newlines(parser);
    }

    module->data.func_def.body = stmts;
    module->data.func_def.body_count = stmt_count;

    return module;
}
