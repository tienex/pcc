/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Object-Oriented Programming runtime support
 */

#include "xbrt.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Class registry */
typedef struct class_def {
	char *name;
	struct class_def *parent;
	size_t field_count;
	char **field_names;
	size_t method_count;
	char **method_names;
	void (**method_ptrs)(void);
	struct class_def *next;
} CLASS_DEF;

static CLASS_DEF *class_registry = NULL;

/*
 * Register a class definition
 */
CLASS_DEF *
xb_register_class(const char *name, const char *parent_name)
{
	CLASS_DEF *cls = malloc(sizeof(CLASS_DEF));
	if (!cls) return NULL;

	cls->name = strdup(name);
	cls->parent = NULL;
	cls->field_count = 0;
	cls->field_names = NULL;
	cls->method_count = 0;
	cls->method_names = NULL;
	cls->method_ptrs = NULL;

	/* Find parent class */
	if (parent_name) {
		CLASS_DEF *p;
		for (p = class_registry; p; p = p->next) {
			if (strcmp(p->name, parent_name) == 0) {
				cls->parent = p;
				break;
			}
		}
	}

	/* Add to registry */
	cls->next = class_registry;
	class_registry = cls;

	return cls;
}

/*
 * Add field to class
 */
void
xb_class_add_field(CLASS_DEF *cls, const char *name)
{
	if (!cls) return;

	cls->field_names = realloc(cls->field_names,
	                           (cls->field_count + 1) * sizeof(char*));
	cls->field_names[cls->field_count++] = strdup(name);
}

/*
 * Add method to class
 */
void
xb_class_add_method(CLASS_DEF *cls, const char *name, void (*func)(void))
{
	if (!cls) return;

	cls->method_names = realloc(cls->method_names,
	                            (cls->method_count + 1) * sizeof(char*));
	cls->method_ptrs = realloc(cls->method_ptrs,
	                           (cls->method_count + 1) * sizeof(void(*)(void)));

	cls->method_names[cls->method_count] = strdup(name);
	cls->method_ptrs[cls->method_count] = func;
	cls->method_count++;
}

/*
 * Create object instance
 */
xb_object_t *
xb_object_new(const char *class_name)
{
	CLASS_DEF *cls;
	xb_object_t *obj;
	size_t total_fields = 0;

	/* Find class */
	for (cls = class_registry; cls; cls = cls->next) {
		if (strcmp(cls->name, class_name) == 0)
			break;
	}

	if (!cls) return NULL;

	/* Count total fields (including inherited) */
	CLASS_DEF *c = cls;
	while (c) {
		total_fields += c->field_count;
		c = c->parent;
	}

	/* Allocate object */
	obj = malloc(sizeof(xb_object_t));
	if (!obj) return NULL;

	obj->class_name = strdup(class_name);
	obj->field_count = total_fields;
	obj->field_names = malloc(total_fields * sizeof(char*));
	obj->fields = calloc(total_fields, sizeof(xb_value_t));

	/* Copy field names from class hierarchy */
	size_t idx = 0;
	c = cls;
	while (c) {
		size_t i;
		for (i = 0; i < c->field_count; i++) {
			obj->field_names[idx++] = strdup(c->field_names[i]);
		}
		c = c->parent;
	}

	return obj;
}

/*
 * Get field index by name
 */
static int
find_field(xb_object_t *obj, const char *name)
{
	size_t i;
	for (i = 0; i < obj->field_count; i++) {
		if (strcmp(obj->field_names[i], name) == 0)
			return i;
	}
	return -1;
}

/*
 * Get object field
 */
xb_value_t *
xb_object_get_field(xb_object_t *obj, const char *name)
{
	int idx = find_field(obj, name);
	if (idx < 0) return NULL;
	return &obj->fields[idx];
}

/*
 * Set object field
 */
void
xb_object_set_field(xb_object_t *obj, const char *name, const xb_value_t *val)
{
	int idx = find_field(obj, name);
	if (idx < 0) return;

	/* Free old value */
	if (obj->fields[idx].type == XB_CHARACTER && obj->fields[idx].data.string) {
		free(obj->fields[idx].data.string);
	}

	/* Copy new value */
	obj->fields[idx] = *val;
	if (val->type == XB_CHARACTER && val->data.string) {
		obj->fields[idx].data.string = strdup(val->data.string);
	}
}

/*
 * Call object method
 */
xb_value_t *
xb_object_call_method(xb_object_t *obj, const char *method_name, xb_value_t **args, int arg_count)
{
	CLASS_DEF *cls;
	size_t i;

	/* Find class */
	for (cls = class_registry; cls; cls = cls->next) {
		if (strcmp(cls->name, obj->class_name) == 0)
			break;
	}

	if (!cls) return xb_value_new_nil();

	/* Search for method in class hierarchy */
	while (cls) {
		for (i = 0; i < cls->method_count; i++) {
			if (strcmp(cls->method_names[i], method_name) == 0) {
				/* Call method */
				/* TODO: Implement proper method calling convention */
				if (cls->method_ptrs[i]) {
					cls->method_ptrs[i]();
				}
				return xb_value_new_nil();
			}
		}
		cls = cls->parent;
	}

	return xb_value_new_nil();
}

/*
 * Free object
 */
void
xb_object_free(xb_object_t *obj)
{
	if (!obj) return;

	size_t i;
	for (i = 0; i < obj->field_count; i++) {
		free(obj->field_names[i]);
		if (obj->fields[i].type == XB_CHARACTER && obj->fields[i].data.string) {
			free(obj->fields[i].data.string);
		}
	}

	free(obj->field_names);
	free(obj->fields);
	free(obj->class_name);
	free(obj);
}
