/*
 * Python Compiler Main Driver
 */

#include "pycom.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Global output file */
FILE *output_file = NULL;

/* Usage information */
static void usage(const char *progname) {
    fprintf(stderr, "Usage: %s [options] <input.py>\n", progname);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -o <file>    Write output to <file>\n");
    fprintf(stderr, "  -v           Verbose mode\n");
    fprintf(stderr, "  -h           Show this help\n");
    exit(1);
}

/* Read entire file into string */
static char *read_file(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        error("Cannot open file: %s", filename);
        return NULL;
    }

    /* Get file size */
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    /* Allocate buffer */
    char *buffer = xmalloc(size + 1);

    /* Read file */
    size_t read_size = fread(buffer, 1, size, fp);
    buffer[read_size] = '\0';

    fclose(fp);
    return buffer;
}

/* Print AST (for debugging) */
static void print_ast(ASTNode *node, int indent) {
    if (!node) return;

    for (int i = 0; i < indent; i++) {
        printf("  ");
    }

    switch (node->type) {
        case AST_MODULE:
            printf("Module:\n");
            for (int i = 0; i < node->data.func_def.body_count; i++) {
                print_ast(node->data.func_def.body[i], indent + 1);
            }
            break;

        case AST_FUNCTION_DEF:
            printf("FunctionDef: %s\n", node->data.func_def.name);
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Parameters:\n");
            for (int i = 0; i < node->data.func_def.param_count; i++) {
                print_ast(node->data.func_def.params[i], indent + 2);
            }
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Body:\n");
            for (int i = 0; i < node->data.func_def.body_count; i++) {
                print_ast(node->data.func_def.body[i], indent + 2);
            }
            break;

        case AST_RETURN:
            printf("Return:\n");
            print_ast(node->data.return_stmt.expr, indent + 1);
            break;

        case AST_IF:
            printf("If:\n");
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Test:\n");
            print_ast(node->data.if_stmt.test, indent + 2);
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Body:\n");
            for (int i = 0; i < node->data.if_stmt.body_count; i++) {
                print_ast(node->data.if_stmt.body[i], indent + 2);
            }
            break;

        case AST_WHILE:
            printf("While:\n");
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Test:\n");
            print_ast(node->data.while_stmt.test, indent + 2);
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Body:\n");
            for (int i = 0; i < node->data.while_stmt.body_count; i++) {
                print_ast(node->data.while_stmt.body[i], indent + 2);
            }
            break;

        case AST_ASSIGN:
            printf("Assign:\n");
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Target:\n");
            print_ast(node->data.assign.targets[0], indent + 2);
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Value:\n");
            print_ast(node->data.assign.value, indent + 2);
            break;

        case AST_BINOP:
            printf("BinOp: op=%d\n", node->data.binop.op);
            print_ast(node->data.binop.left, indent + 1);
            print_ast(node->data.binop.right, indent + 1);
            break;

        case AST_UNOP:
            printf("UnOp: op=%d\n", node->data.unop.op);
            print_ast(node->data.unop.operand, indent + 1);
            break;

        case AST_CALL:
            printf("Call:\n");
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Function:\n");
            print_ast(node->data.call.func, indent + 2);
            for (int i = 0; i < indent + 1; i++) printf("  ");
            printf("Arguments:\n");
            for (int i = 0; i < node->data.call.arg_count; i++) {
                print_ast(node->data.call.args[i], indent + 2);
            }
            break;

        case AST_NAME:
            printf("Name: %s\n", node->data.name.id);
            break;

        case AST_NUMBER:
            printf("Number: %lld\n", node->data.number.value);
            break;

        case AST_STRING:
            printf("String: \"%s\"\n", node->data.string.value);
            break;

        case AST_PASS:
            printf("Pass\n");
            break;

        case AST_BREAK:
            printf("Break\n");
            break;

        case AST_CONTINUE:
            printf("Continue\n");
            break;

        case AST_EXPR_STMT:
            printf("ExprStmt:\n");
            print_ast(node->data.return_stmt.expr, indent + 1);
            break;

        default:
            printf("Unknown node type: %d\n", node->type);
            break;
    }
}

/* Main entry point */
int main(int argc, char **argv) {
    const char *input_file = NULL;
    const char *output_filename = NULL;
    int verbose = 0;

    /* Parse command-line arguments */
    int opt;
    while ((opt = getopt(argc, argv, "o:vh")) != -1) {
        switch (opt) {
            case 'o':
                output_filename = optarg;
                break;
            case 'v':
                verbose = 1;
                break;
            case 'h':
            default:
                usage(argv[0]);
        }
    }

    if (optind >= argc) {
        fprintf(stderr, "Error: No input file specified\n");
        usage(argv[0]);
    }

    input_file = argv[optind];

    /* Open output file */
    if (output_filename) {
        output_file = fopen(output_filename, "w");
        if (!output_file) {
            error("Cannot open output file: %s", output_filename);
            return 1;
        }
    } else {
        output_file = stdout;
    }

    if (verbose) {
        printf("Compiling: %s\n", input_file);
    }

    /* Read input file */
    char *source = read_file(input_file);
    if (!source) {
        return 1;
    }

    /* Lexical analysis */
    if (verbose) {
        printf("Lexing...\n");
    }
    Lexer *lexer = lexer_new(source);

    /* Parsing */
    if (verbose) {
        printf("Parsing...\n");
    }
    Parser *parser = parser_new(lexer);
    ASTNode *ast = parse_module(parser);

    if (error_count > 0) {
        fprintf(stderr, "Compilation failed with %d error(s)\n", error_count);
        return 1;
    }

    /* Print AST if verbose */
    if (verbose) {
        printf("\nAbstract Syntax Tree:\n");
        print_ast(ast, 0);
        printf("\n");
    }

    /* Code generation */
    if (verbose) {
        printf("Generating code...\n");
    }

    SymbolTable *global_symtab = symtab_new(NULL);
    NODE *ir = codegen(ast, global_symtab);

    if (error_count > 0) {
        fprintf(stderr, "Compilation failed with %d error(s)\n", error_count);
        return 1;
    }

    /* Output */
    fprintf(output_file, "; Python compiled to PCC IR\n");
    fprintf(output_file, "; Source: %s\n\n", input_file);

    if (verbose) {
        printf("Compilation successful\n");
    }

    /* Cleanup */
    ast_free(ast);
    parser_free(parser);
    lexer_free(lexer);
    symtab_free(global_symtab);
    free(source);

    if (output_file != stdout) {
        fclose(output_file);
    }

    return error_count > 0 ? 1 : 0;
}
