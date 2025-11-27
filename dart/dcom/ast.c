/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart AST construction and manipulation
 */

#include "pass1.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int yylineno;
extern int yycolumn;

Node *
make_node(NodeType type)
{
	Node *node = malloc(sizeof(Node));
	node->type = type;
	node->lineno = yylineno;
	node->column = yycolumn;
	node->next = NULL;
	node->symbol = NULL;
	memset(&node->data, 0, sizeof(node->data));
	return node;
}

Node *
make_binary_op(int op, Node *left, Node *right)
{
	Node *node = make_node(N_BINARY_OP);
	node->data.binary.left = left;
	node->data.binary.right = right;
	return node;
}

Node *
make_literal_int(int value)
{
	Node *node = make_node(N_LITERAL);
	node->data.intval = value;
	return node;
}

Node *
make_literal_double(double value)
{
	Node *node = make_node(N_LITERAL);
	node->data.doubleval = value;
	return node;
}

Node *
make_literal_string(char *value)
{
	Node *node = make_node(N_LITERAL);
	node->data.strval = strdup(value);
	return node;
}

Node *
make_identifier(char *name)
{
	Node *node = make_node(N_IDENTIFIER);
	node->data.strval = strdup(name);
	return node;
}

void
free_node(Node *node)
{
	if (node == NULL) {
		return;
	}

	switch (node->type) {
	case N_LITERAL:
	case N_IDENTIFIER:
		if (node->data.strval) {
			free(node->data.strval);
		}
		break;
	case N_BINARY_OP:
		free_node(node->data.binary.left);
		free_node(node->data.binary.right);
		break;
	default:
		if (node->data.child) {
			free_node(node->data.child);
		}
		break;
	}

	if (node->next) {
		free_node(node->next);
	}

	free(node);
}

void
print_node(Node *node, int indent)
{
	if (node == NULL) {
		return;
	}

	for (int i = 0; i < indent; i++) {
		printf("  ");
	}

	switch (node->type) {
	case N_PROGRAM:
		printf("PROGRAM\n");
		break;
	case N_IMPORT:
		printf("IMPORT\n");
		break;
	case N_CLASS:
		printf("CLASS\n");
		break;
	case N_FUNCTION:
		printf("FUNCTION\n");
		break;
	case N_METHOD:
		printf("METHOD\n");
		break;
	case N_VARIABLE:
		printf("VARIABLE\n");
		break;
	case N_PARAMETER:
		printf("PARAMETER\n");
		break;
	case N_BLOCK:
		printf("BLOCK\n");
		break;
	case N_IF:
		printf("IF\n");
		break;
	case N_FOR:
		printf("FOR\n");
		break;
	case N_WHILE:
		printf("WHILE\n");
		break;
	case N_RETURN:
		printf("RETURN\n");
		break;
	case N_CALL:
		printf("CALL\n");
		break;
	case N_BINARY_OP:
		printf("BINARY_OP\n");
		print_node(node->data.binary.left, indent + 1);
		print_node(node->data.binary.right, indent + 1);
		break;
	case N_LITERAL:
		if (node->data.strval) {
			printf("LITERAL: %s\n", node->data.strval);
		} else {
			printf("LITERAL: %d\n", node->data.intval);
		}
		break;
	case N_IDENTIFIER:
		printf("IDENTIFIER: %s\n", node->data.strval);
		break;
	default:
		printf("UNKNOWN\n");
		break;
	}

	if (node->next) {
		print_node(node->next, indent);
	}
}

void
print_ast(Node *root)
{
	printf("Abstract Syntax Tree:\n");
	print_node(root, 0);
}
