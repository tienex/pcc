/*
 * COBOL Runtime Library - OO Support
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cobolrt.h"

cobol_object_t *
__cobol_object_new(const char *class_name, size_t size)
{
	cobol_object_t *obj;

	obj = (cobol_object_t *)__cobol_malloc(sizeof(cobol_object_t));
	memset(obj, 0, sizeof(cobol_object_t));

	obj->class_name = strdup(class_name);
	obj->data = __cobol_malloc(size);
	memset(obj->data, 0, size);
	obj->ref_count = 1;

	return obj;
}

void
__cobol_object_free(cobol_object_t *obj)
{
	if (!obj)
		return;

	if (obj->class_name)
		free(obj->class_name);

	if (obj->data)
		free(obj->data);

	free(obj);
}

void
__cobol_object_retain(cobol_object_t *obj)
{
	if (obj)
		obj->ref_count++;
}

void
__cobol_object_release(cobol_object_t *obj)
{
	if (!obj)
		return;

	obj->ref_count--;
	if (obj->ref_count <= 0)
		__cobol_object_free(obj);
}

void *
__cobol_invoke(cobol_object_t *obj, const char *method_name, void **args)
{
	/* Simple method dispatch - in a real implementation, this would
	 * look up the method in the vtable and call it */

	if (!obj || !method_name)
		return NULL;

	/* TODO: Implement vtable lookup and method invocation */
	__cobol_set_exception(1, "Method invocation not yet implemented");

	return NULL;
}
