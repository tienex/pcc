/*
 * Copyright (c) 2025 PCC Common LISP Runtime Library
 *
 * I/O operations implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liblisp.h"

/*
 * Print object recursively
 */
static void
print_object(lisp_object_t *obj, int readable)
{
	if (obj == NULL) {
		printf("NULL");
		return;
	}

	switch (obj->type) {
	case LISP_TYPE_NIL:
		printf("NIL");
		break;

	case LISP_TYPE_T:
		printf("T");
		break;

	case LISP_TYPE_INTEGER:
		printf("%ld", obj->value.integer);
		break;

	case LISP_TYPE_FLOAT:
		printf("%g", obj->value.floating);
		break;

	case LISP_TYPE_STRING:
		if (readable)
			printf("\"%s\"", obj->value.string);
		else
			printf("%s", obj->value.string);
		break;

	case LISP_TYPE_SYMBOL:
		printf("%s", obj->value.symbol);
		break;

	case LISP_TYPE_CONS:
		printf("(");
		print_object(obj->value.cons.car, readable);
		obj = obj->value.cons.cdr;
		while (obj->type == LISP_TYPE_CONS) {
			printf(" ");
			print_object(obj->value.cons.car, readable);
			obj = obj->value.cons.cdr;
		}
		if (obj->type != LISP_TYPE_NIL) {
			printf(" . ");
			print_object(obj, readable);
		}
		printf(")");
		break;

	case LISP_TYPE_FUNCTION:
		printf("#<FUNCTION>");
		break;

	default:
		printf("#<UNKNOWN>");
		break;
	}
}

/*
 * PRINT: print object with newline
 */
void
lisp_print(lisp_object_t *obj)
{
	print_object(obj, 1);
	printf("\n");
}

/*
 * PRINC: print object without quotes, no newline
 */
void
lisp_princ(lisp_object_t *obj)
{
	print_object(obj, 0);
}

/*
 * PRIN1: print object with quotes, no newline
 */
void
lisp_prin1(lisp_object_t *obj)
{
	print_object(obj, 1);
}

/*
 * WRITE: print object with full readability
 */
void
lisp_write(lisp_object_t *obj)
{
	print_object(obj, 1);
	printf("\n");
}

/*
 * READ: read a LISP object from stdin (simplified)
 */
lisp_object_t *
lisp_read(void)
{
	char buffer[1024];

	if (fgets(buffer, sizeof(buffer), stdin) == NULL)
		return LISP_NIL;

	/* Very simplified reader - just handles integers and symbols */
	long value;
	if (sscanf(buffer, "%ld", &value) == 1)
		return lisp_make_integer(value);

	/* Remove newline */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len-1] == '\n')
		buffer[len-1] = '\0';

	if (strcmp(buffer, "nil") == 0 || strcmp(buffer, "NIL") == 0)
		return LISP_NIL;

	if (strcmp(buffer, "t") == 0 || strcmp(buffer, "T") == 0)
		return LISP_T;

	return lisp_make_symbol(buffer);
}

/*
 * READ-LINE: read a line as a string
 */
lisp_object_t *
lisp_read_line(void)
{
	char buffer[1024];

	if (fgets(buffer, sizeof(buffer), stdin) == NULL)
		return LISP_NIL;

	/* Remove newline */
	size_t len = strlen(buffer);
	if (len > 0 && buffer[len-1] == '\n')
		buffer[len-1] = '\0';

	return lisp_make_string(buffer);
}
