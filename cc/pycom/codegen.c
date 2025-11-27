/*
 * IR Code Generation
 * Converts Python AST to PCC IR (NODE tree)
 */

#include "pycom.h"
#include <stdlib.h>
#include <string.h>

/* Global variables for code generation */
static int next_label = 1;
static int next_temp = 1;
static SymbolTable *current_symtab = NULL;

/* Forward declarations */
static NODE *codegen_stmt(ASTNode *stmt);
static NODE *codegen_expr(ASTNode *expr);

/* Utility: Create a new label */
static int new_label(void) {
    return next_label++;
}

/* Utility: Create a new temporary */
static int new_temp(void) {
    return next_temp++;
}

/* Create an integer constant node */
static NODE *make_icon(long long value) {
    NODE *node = xmalloc(sizeof(NODE));
    memset(node, 0, sizeof(NODE));
    node->n_op = ICON;
    node->n_type = LONGLONG;
    setlval(node, value);
    return node;
}

/* Create a name node */
static NODE *make_name(const char *name) {
    NODE *node = xmalloc(sizeof(NODE));
    memset(node, 0, sizeof(NODE));
    node->n_op = NAME;
    node->n_type = LONGLONG;
    node->n_name = xstrdup(name);
    return node;
}

/* Create a binary operation node */
static NODE *make_binop(int op, NODE *left, NODE *right, TWORD type) {
    NODE *node = xmalloc(sizeof(NODE));
    memset(node, 0, sizeof(NODE));
    node->n_op = op;
    node->n_type = type;
    node->n_left = left;
    node->n_right = right;
    return node;
}

/* Create a unary operation node */
static NODE *make_unop(int op, NODE *operand, TWORD type) {
    NODE *node = xmalloc(sizeof(NODE));
    memset(node, 0, sizeof(NODE));
    node->n_op = op;
    node->n_type = type;
    node->n_left = operand;
    return node;
}

/* Convert Python token op to PCC node op */
static int token_to_node_op(TokenType op) {
    switch (op) {
        case TOK_PLUS: return PLUS;
        case TOK_MINUS: return MINUS;
        case TOK_STAR: return MUL;
        case TOK_SLASH: return DIV;
        case TOK_PERCENT: return MOD;
        case TOK_LSHIFT: return LS;
        case TOK_RSHIFT: return RS;
        case TOK_AMPERSAND: return AND;
        case TOK_PIPE: return OR;
        case TOK_CARET: return ER;  /* XOR */
        case TOK_EQ: return EQ;
        case TOK_NE: return NE;
        case TOK_LT: return LT;
        case TOK_LE: return LE;
        case TOK_GT: return GT;
        case TOK_GE: return GE;
        default:
            error("Unsupported operator: %d", op);
            return PLUS;
    }
}

/* Generate code for expression */
static NODE *codegen_expr(ASTNode *expr) {
    if (!expr) {
        return make_icon(0);
    }

    switch (expr->type) {
        case AST_NUMBER:
            return make_icon(expr->data.number.value);

        case AST_STRING: {
            /* For now, treat strings as pointers to static data */
            NODE *node = make_name(expr->data.string.value);
            node->n_type = PTR | CHAR;
            return node;
        }

        case AST_NAME: {
            Symbol *sym = symtab_lookup(current_symtab, expr->data.name.id);
            if (!sym) {
                /* Create symbol on first use (Python's implicit declaration) */
                sym = symtab_insert(current_symtab, expr->data.name.id, LONGLONG);
            }
            return make_name(expr->data.name.id);
        }

        case AST_BINOP: {
            NODE *left = codegen_expr(expr->data.binop.left);
            NODE *right = codegen_expr(expr->data.binop.right);
            int op = token_to_node_op(expr->data.binop.op);
            return make_binop(op, left, right, LONGLONG);
        }

        case AST_UNOP: {
            NODE *operand = codegen_expr(expr->data.unop.operand);
            int op;
            switch (expr->data.unop.op) {
                case TOK_MINUS:
                    op = UMINUS;
                    break;
                case TOK_TILDE:
                    op = COMPL;
                    break;
                case TOK_NOT:
                    /* Convert 'not x' to 'x == 0' */
                    return make_binop(EQ, operand, make_icon(0), LONGLONG);
                default:
                    error("Unsupported unary operator");
                    op = UMINUS;
            }
            return make_unop(op, operand, LONGLONG);
        }

        case AST_COMPARE: {
            NODE *left = codegen_expr(expr->data.binop.left);
            NODE *right = codegen_expr(expr->data.binop.right);
            int op = token_to_node_op(expr->data.binop.op);
            return make_binop(op, left, right, INT);
        }

        case AST_CALL: {
            /* Generate function call */
            NODE *func = codegen_expr(expr->data.call.func);

            /* Build argument list as CM (comma) nodes */
            NODE *args = NULL;
            for (int i = expr->data.call.arg_count - 1; i >= 0; i--) {
                NODE *arg = codegen_expr(expr->data.call.args[i]);
                if (args == NULL) {
                    args = arg;
                } else {
                    args = make_binop(CM, arg, args, LONGLONG);
                }
            }

            /* Create CALL node */
            NODE *call_node = xmalloc(sizeof(NODE));
            memset(call_node, 0, sizeof(NODE));
            call_node->n_op = CALL;
            call_node->n_type = LONGLONG;
            call_node->n_left = func;
            call_node->n_right = args;
            return call_node;
        }

        default:
            error("Unsupported expression type: %d", expr->type);
            return make_icon(0);
    }
}

/* Generate code for statement */
static NODE *codegen_stmt(ASTNode *stmt) {
    if (!stmt) return NULL;

    switch (stmt->type) {
        case AST_EXPR_STMT: {
            return codegen_expr(stmt->data.return_stmt.expr);
        }

        case AST_ASSIGN: {
            /* Generate assignment: target = value */
            NODE *value = codegen_expr(stmt->data.assign.value);
            NODE *target = codegen_expr(stmt->data.assign.targets[0]);

            /* Create assignment node */
            NODE *assign = xmalloc(sizeof(NODE));
            memset(assign, 0, sizeof(NODE));
            assign->n_op = ASSIGN;
            assign->n_type = LONGLONG;
            assign->n_left = target;
            assign->n_right = value;
            return assign;
        }

        case AST_RETURN: {
            NODE *expr = codegen_expr(stmt->data.return_stmt.expr);
            NODE *ret = xmalloc(sizeof(NODE));
            memset(ret, 0, sizeof(NODE));
            ret->n_op = RETURN;
            ret->n_type = LONGLONG;
            ret->n_left = expr;
            return ret;
        }

        case AST_IF: {
            /* Generate if statement:
             * if (test) { body } else { orelse }
             *
             * Translates to:
             *   if (!test) goto else_label
             *   body
             *   goto end_label
             * else_label:
             *   orelse
             * end_label:
             */
            int else_label = new_label();
            int end_label = new_label();

            /* Generate test condition */
            NODE *test = codegen_expr(stmt->data.if_stmt.test);

            /* Create conditional branch (if test == 0, goto else) */
            NODE *cond = make_binop(EQ, test, make_icon(0), INT);
            NODE *branch = xmalloc(sizeof(NODE));
            memset(branch, 0, sizeof(NODE));
            branch->n_op = CBRANCH;
            branch->n_left = cond;
            branch->n_label = else_label;

            /* For now, just return the conditional branch
             * Full implementation would need a sequence of statements
             */
            return branch;
        }

        case AST_WHILE: {
            /* Generate while statement:
             * while (test) { body }
             *
             * Translates to:
             * start_label:
             *   if (!test) goto end_label
             *   body
             *   goto start_label
             * end_label:
             */
            int start_label = new_label();
            int end_label = new_label();

            /* Generate test condition */
            NODE *test = codegen_expr(stmt->data.while_stmt.test);

            /* Create conditional branch */
            NODE *cond = make_binop(EQ, test, make_icon(0), INT);
            NODE *branch = xmalloc(sizeof(NODE));
            memset(branch, 0, sizeof(NODE));
            branch->n_op = CBRANCH;
            branch->n_left = cond;
            branch->n_label = end_label;

            return branch;
        }

        case AST_PASS:
            /* Pass statement does nothing */
            return NULL;

        case AST_BREAK: {
            /* Generate break (goto end of loop) */
            NODE *node = xmalloc(sizeof(NODE));
            memset(node, 0, sizeof(NODE));
            node->n_op = GOTO;
            node->n_label = 0; /* Would need to track loop labels */
            return node;
        }

        case AST_CONTINUE: {
            /* Generate continue (goto start of loop) */
            NODE *node = xmalloc(sizeof(NODE));
            memset(node, 0, sizeof(NODE));
            node->n_op = GOTO;
            node->n_label = 0; /* Would need to track loop labels */
            return node;
        }

        case AST_PRINT: {
            /* Python 2 print statement - convert to print() function call */
            NODE *func = make_name("print");

            /* Build argument list */
            NODE *args = NULL;
            for (int i = stmt->data.print_stmt.value_count - 1; i >= 0; i--) {
                NODE *arg = codegen_expr(stmt->data.print_stmt.values[i]);
                if (args == NULL) {
                    args = arg;
                } else {
                    args = make_binop(CM, arg, args, LONGLONG);
                }
            }

            /* Create CALL node */
            NODE *call_node = xmalloc(sizeof(NODE));
            memset(call_node, 0, sizeof(NODE));
            call_node->n_op = CALL;
            call_node->n_type = LONGLONG;
            call_node->n_left = func;
            call_node->n_right = args;
            return call_node;
        }

        case AST_EXEC: {
            /* Python 2 exec statement - not fully implemented */
            error("exec statement not yet supported");
            return NULL;
        }

        case AST_FUNCTION_DEF: {
            /* Function definitions are handled separately */
            return NULL;
        }

        default:
            error("Unsupported statement type: %d", stmt->type);
            return NULL;
    }
}

/* Generate code for function */
static void codegen_function(ASTNode *func_def) {
    if (func_def->type != AST_FUNCTION_DEF) {
        error("Expected function definition");
        return;
    }

    /* Create new symbol table for function scope */
    SymbolTable *func_symtab = symtab_new(current_symtab);
    SymbolTable *saved_symtab = current_symtab;
    current_symtab = func_symtab;

    /* Add parameters to symbol table */
    for (int i = 0; i < func_def->data.func_def.param_count; i++) {
        ASTNode *param = func_def->data.func_def.params[i];
        if (param->type == AST_NAME) {
            Symbol *sym = symtab_insert(func_symtab, param->data.name.id, LONGLONG);
            sym->is_parameter = 1;
        }
    }

    /* Generate code for function body */
    for (int i = 0; i < func_def->data.func_def.body_count; i++) {
        NODE *stmt_node = codegen_stmt(func_def->data.func_def.body[i]);
        if (stmt_node) {
            /* In a real implementation, we would emit these nodes */
            /* For now, we just generate them */
        }
    }

    /* Restore symbol table */
    current_symtab = saved_symtab;
    symtab_free(func_symtab);
}

/* Main code generation entry point */
NODE *codegen(ASTNode *ast, SymbolTable *symtab) {
    current_symtab = symtab;

    if (!ast) return NULL;

    if (ast->type == AST_MODULE) {
        /* Process top-level statements */
        for (int i = 0; i < ast->data.func_def.body_count; i++) {
            ASTNode *stmt = ast->data.func_def.body[i];

            if (stmt->type == AST_FUNCTION_DEF) {
                codegen_function(stmt);
            } else {
                NODE *node = codegen_stmt(stmt);
                if (node) {
                    /* Emit or process the node */
                }
            }
        }
    } else {
        return codegen_stmt(ast);
    }

    return NULL;
}
