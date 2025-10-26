/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Value creation and manipulation
 */

#include "xbrt.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*
 * Create a new value of specified type
 */
xb_value_t *
xb_value_new(xb_type_t type)
{
	xb_value_t *val = (xb_value_t *)malloc(sizeof(xb_value_t));
	if (!val)
		return NULL;

	memset(val, 0, sizeof(xb_value_t));
	val->type = type;

	return val;
}

/*
 * Create a new numeric value
 */
xb_value_t *
xb_value_new_numeric(double value)
{
	xb_value_t *val = xb_value_new(XB_NUMERIC);
	if (val)
		val->data.numeric = value;
	return val;
}

/*
 * Create a new string value
 */
xb_value_t *
xb_value_new_string(const char *str)
{
	xb_value_t *val = xb_value_new(XB_CHARACTER);
	if (val && str) {
		val->data.string = strdup(str);
	}
	return val;
}

/*
 * Create a new date value
 */
xb_value_t *
xb_value_new_date(int32_t julian)
{
	xb_value_t *val = xb_value_new(XB_DATE);
	if (val)
		val->data.date = julian;
	return val;
}

/*
 * Create a new logical value
 */
xb_value_t *
xb_value_new_logical(int value)
{
	xb_value_t *val = xb_value_new(XB_LOGICAL);
	if (val)
		val->data.logical = value ? 1 : 0;
	return val;
}

/*
 * Create a new array value
 */
xb_value_t *
xb_value_new_array(size_t size)
{
	xb_value_t *val = xb_value_new(XB_ARRAY);
	if (!val)
		return NULL;

	xb_array_t *arr = (xb_array_t *)malloc(sizeof(xb_array_t));
	if (!arr) {
		free(val);
		return NULL;
	}

	arr->length = size;
	arr->capacity = size;
	arr->elements = NULL;

	if (size > 0) {
		arr->elements = (xb_value_t *)calloc(size, sizeof(xb_value_t));
		if (!arr->elements) {
			free(arr);
			free(val);
			return NULL;
		}
	}

	val->data.array = arr;
	return val;
}

/*
 * Create a new NIL value
 */
xb_value_t *
xb_value_new_nil(void)
{
	return xb_value_new(XB_NIL);
}

/*
 * Free a value and its contents
 */
void
xb_value_free(xb_value_t *val)
{
	if (!val)
		return;

	switch (val->type) {
	case XB_CHARACTER:
	case XB_MEMO:
		free(val->data.string);
		break;

	case XB_ARRAY:
		if (val->data.array) {
			if (val->data.array->elements) {
				size_t i;
				for (i = 0; i < val->data.array->length; i++) {
					xb_value_free(&val->data.array->elements[i]);
				}
				free(val->data.array->elements);
			}
			free(val->data.array);
		}
		break;

	case XB_OBJECT:
		if (val->data.object) {
			if (val->data.object->field_names) {
				size_t i;
				for (i = 0; i < val->data.object->field_count; i++) {
					free(val->data.object->field_names[i]);
				}
				free(val->data.object->field_names);
			}
			if (val->data.object->fields) {
				size_t i;
				for (i = 0; i < val->data.object->field_count; i++) {
					xb_value_free(&val->data.object->fields[i]);
				}
				free(val->data.object->fields);
			}
			free(val->data.object->class_name);
			free(val->data.object);
		}
		break;

	case XB_CODEBLOCK:
		if (val->data.block) {
			free(val->data.block->locals);
			free(val->data.block);
		}
		break;

	default:
		/* Nothing to free for simple types */
		break;
	}

	free(val);
}

/*
 * Clone a value (deep copy)
 */
xb_value_t *
xb_value_clone(const xb_value_t *val)
{
	if (!val)
		return NULL;

	xb_value_t *clone = xb_value_new(val->type);
	if (!clone)
		return NULL;

	switch (val->type) {
	case XB_NUMERIC:
		clone->data.numeric = val->data.numeric;
		break;

	case XB_CHARACTER:
	case XB_MEMO:
		if (val->data.string)
			clone->data.string = strdup(val->data.string);
		break;

	case XB_DATE:
		clone->data.date = val->data.date;
		break;

	case XB_LOGICAL:
		clone->data.logical = val->data.logical;
		break;

	case XB_ARRAY:
		/* TODO: Implement deep array clone */
		break;

	case XB_OBJECT:
		/* TODO: Implement object clone */
		break;

	default:
		break;
	}

	return clone;
}

/*
 * Convert value to numeric
 */
double
xb_value_to_numeric(const xb_value_t *val)
{
	if (!val)
		return 0.0;

	switch (val->type) {
	case XB_NUMERIC:
		return val->data.numeric;

	case XB_CHARACTER:
	case XB_MEMO:
		if (val->data.string)
			return atof(val->data.string);
		return 0.0;

	case XB_LOGICAL:
		return val->data.logical ? 1.0 : 0.0;

	case XB_DATE:
		return (double)val->data.date;

	default:
		return 0.0;
	}
}

/*
 * Convert value to string
 */
char *
xb_value_to_string(const xb_value_t *val)
{
	char buf[256];

	if (!val)
		return strdup("");

	switch (val->type) {
	case XB_CHARACTER:
	case XB_MEMO:
		return val->data.string ? strdup(val->data.string) : strdup("");

	case XB_NUMERIC:
		snprintf(buf, sizeof(buf), "%.2f", val->data.numeric);
		return strdup(buf);

	case XB_LOGICAL:
		return strdup(val->data.logical ? ".T." : ".F.");

	case XB_DATE:
		/* TODO: Format date properly */
		snprintf(buf, sizeof(buf), "%d", val->data.date);
		return strdup(buf);

	case XB_NIL:
		return strdup("NIL");

	default:
		return strdup("");
	}
}

/*
 * Convert value to logical
 */
int
xb_value_to_logical(const xb_value_t *val)
{
	if (!val)
		return 0;

	switch (val->type) {
	case XB_LOGICAL:
		return val->data.logical;

	case XB_NUMERIC:
		return val->data.numeric != 0.0;

	case XB_CHARACTER:
	case XB_MEMO:
		return val->data.string && val->data.string[0] != '\0';

	case XB_NIL:
	case XB_NULL:
		return 0;

	default:
		return 1;  /* Non-empty values are true */
	}
}

/*
 * Get type name of value
 */
const char *
xb_value_type_name(const xb_value_t *val)
{
	if (!val)
		return "NULL";

	switch (val->type) {
	case XB_NULL:      return "NULL";
	case XB_NUMERIC:   return "N";
	case XB_CHARACTER: return "C";
	case XB_DATE:      return "D";
	case XB_LOGICAL:   return "L";
	case XB_ARRAY:     return "A";
	case XB_OBJECT:    return "O";
	case XB_CODEBLOCK: return "B";
	case XB_MEMO:      return "M";
	case XB_NIL:       return "U";
	default:           return "?";
	}
}
