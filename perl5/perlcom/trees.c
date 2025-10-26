/*	$Id$	*/
/*
 * Perl 5 Compiler - Tree Building Functions
 *
 * This module handles abstract syntax tree construction
 * and manipulation for Perl 5 code.
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Tree node types */
typedef enum {
	NODE_SCALAR,
	NODE_ARRAY,
	NODE_HASH,
	NODE_BINOP,
	NODE_UNOP,
	NODE_CONST,
	NODE_CALL,
	NODE_IF,
	NODE_WHILE,
	NODE_FOR,
	NODE_FOREACH,
	NODE_SUB,
	NODE_BLOCK
} node_type_t;

/* Tree node structure */
typedef struct tree_node {
	node_type_t type;
	union {
		struct {
			char *name;
			int vartype;
		} var;
		struct {
			int op;
			struct tree_node *left;
			struct tree_node *right;
		} binop;
		struct {
			int op;
			struct tree_node *operand;
		} unop;
		struct {
			int type;
			union {
				long intval;
				double floatval;
				char *strval;
			} value;
		} constant;
		struct {
			char *name;
			struct tree_node *args;
		} call;
		struct {
			struct tree_node *cond;
			struct tree_node *then_part;
			struct tree_node *else_part;
		} ifstmt;
		struct {
			struct tree_node *cond;
			struct tree_node *body;
		} loop;
		struct {
			char *name;
			struct tree_node *params;
			struct tree_node *body;
		} sub;
		struct {
			struct tree_node *statements;
		} block;
	} u;
	struct tree_node *next;	/* For lists */
} tree_node_t;

/* Memory management */
static tree_node_t *free_list = NULL;

static tree_node_t *new_node(node_type_t type) {
	tree_node_t *node;

	if (free_list != NULL) {
		node = free_list;
		free_list = free_list->next;
	} else {
		node = malloc(sizeof(tree_node_t));
		if (node == NULL) {
			perl_fatal("Out of memory");
		}
	}

	memset(node, 0, sizeof(tree_node_t));
	node->type = type;
	return node;
}

void *perl_scalar_node(char *name) {
	tree_node_t *node = new_node(NODE_SCALAR);
	node->u.var.name = strdup(name);
	node->u.var.vartype = P_SCALAR;
	return node;
}

void *perl_array_node(char *name) {
	tree_node_t *node = new_node(NODE_ARRAY);
	node->u.var.name = strdup(name);
	node->u.var.vartype = P_ARRAY;
	return node;
}

void *perl_hash_node(char *name) {
	tree_node_t *node = new_node(NODE_HASH);
	node->u.var.name = strdup(name);
	node->u.var.vartype = P_HASH;
	return node;
}

void *perl_binop_node(int op, void *left, void *right) {
	tree_node_t *node = new_node(NODE_BINOP);
	node->u.binop.op = op;
	node->u.binop.left = (tree_node_t *)left;
	node->u.binop.right = (tree_node_t *)right;
	return node;
}

void *perl_unop_node(int op, void *operand) {
	tree_node_t *node = new_node(NODE_UNOP);
	node->u.unop.op = op;
	node->u.unop.operand = (tree_node_t *)operand;
	return node;
}

void *perl_const_node(int type, void *value) {
	tree_node_t *node = new_node(NODE_CONST);
	node->u.constant.type = type;

	switch (type) {
	case INT:
		node->u.constant.value.intval = *(long *)value;
		break;
	case FLOAT:
		node->u.constant.value.floatval = *(double *)value;
		break;
	default:
		node->u.constant.value.strval = strdup((char *)value);
		break;
	}

	return node;
}

void *perl_call_node(char *name, void *args) {
	tree_node_t *node = new_node(NODE_CALL);
	node->u.call.name = strdup(name);
	node->u.call.args = (tree_node_t *)args;
	return node;
}

void *perl_if_node(void *cond, void *then_part, void *else_part) {
	tree_node_t *node = new_node(NODE_IF);
	node->u.ifstmt.cond = (tree_node_t *)cond;
	node->u.ifstmt.then_part = (tree_node_t *)then_part;
	node->u.ifstmt.else_part = (tree_node_t *)else_part;
	return node;
}

void *perl_while_node(void *cond, void *body) {
	tree_node_t *node = new_node(NODE_WHILE);
	node->u.loop.cond = (tree_node_t *)cond;
	node->u.loop.body = (tree_node_t *)body;
	return node;
}

void *perl_for_node(void *init, void *cond, void *incr, void *body) {
	tree_node_t *node = new_node(NODE_FOR);
	/* Build composite structure */
	/* TODO: Implement properly */
	return node;
}

void *perl_foreach_node(void *var, void *list, void *body) {
	tree_node_t *node = new_node(NODE_FOREACH);
	/* Build composite structure */
	/* TODO: Implement properly */
	return node;
}

/* Tree traversal and printing (for debugging) */
static void print_tree_indent(tree_node_t *node, int indent) {
	int i;

	if (node == NULL) {
		return;
	}

	for (i = 0; i < indent; i++) {
		fprintf(stderr, "  ");
	}

	switch (node->type) {
	case NODE_SCALAR:
		fprintf(stderr, "SCALAR: $%s\n", node->u.var.name);
		break;
	case NODE_ARRAY:
		fprintf(stderr, "ARRAY: @%s\n", node->u.var.name);
		break;
	case NODE_HASH:
		fprintf(stderr, "HASH: %%%s\n", node->u.var.name);
		break;
	case NODE_BINOP:
		fprintf(stderr, "BINOP: %c\n", node->u.binop.op);
		print_tree_indent(node->u.binop.left, indent + 1);
		print_tree_indent(node->u.binop.right, indent + 1);
		break;
	case NODE_UNOP:
		fprintf(stderr, "UNOP: %c\n", node->u.unop.op);
		print_tree_indent(node->u.unop.operand, indent + 1);
		break;
	case NODE_CONST:
		fprintf(stderr, "CONST: ");
		if (node->u.constant.type == INT) {
			fprintf(stderr, "%ld\n", node->u.constant.value.intval);
		} else if (node->u.constant.type == FLOAT) {
			fprintf(stderr, "%f\n", node->u.constant.value.floatval);
		} else {
			fprintf(stderr, "\"%s\"\n", node->u.constant.value.strval);
		}
		break;
	case NODE_CALL:
		fprintf(stderr, "CALL: %s\n", node->u.call.name);
		print_tree_indent(node->u.call.args, indent + 1);
		break;
	case NODE_IF:
		fprintf(stderr, "IF:\n");
		print_tree_indent(node->u.ifstmt.cond, indent + 1);
		print_tree_indent(node->u.ifstmt.then_part, indent + 1);
		if (node->u.ifstmt.else_part) {
			print_tree_indent(node->u.ifstmt.else_part, indent + 1);
		}
		break;
	default:
		fprintf(stderr, "UNKNOWN NODE TYPE\n");
		break;
	}
}

void perl_print_tree(void *node) {
	print_tree_indent((tree_node_t *)node, 0);
}
