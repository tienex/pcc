/*
 * Copyright (c) 2025 PCC Project
 *
 * CoreFoundation ABI implementation
 *
 * CoreFoundation uses toll-free bridging with Objective-C.
 * All CF types start with CFRuntimeBase structure.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
cf_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (!func) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "CF%s%s",
		         func->parent_class->name, func->name);
	} else {
		snprintf(buf, sizeof(buf), "CF%s", func->name);
	}

	return strdup(buf);
}

static char *
cf_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	char buf[256];
	snprintf(buf, sizeof(buf), "kCF%s", name);
	return strdup(buf);
}

static char *
cf_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("void");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("void");
	case ABI_TYPE_BOOL: return strdup("Boolean");
	case ABI_TYPE_INT: return strdup("CFIndex");
	case ABI_TYPE_FLOAT: return strdup("CGFloat");
	case ABI_TYPE_DOUBLE: return strdup("CGFloat");
	default: return strdup("CFTypeRef");
	}
}

static char *
cf_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "__CF%sClass", cls->name);
	return strdup(buf);
}

static char *
cf_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "__kCF%sTypeID", cls->name);
	return strdup(buf);
}

/* CoreFoundation objects have CFRuntimeBase at offset 0 */
static void
cf_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	abi_field_t *field;
	size_t offset = 0;
	size_t max_align = sizeof(void *);

	if (cls == NULL)
		return;

	/* CFRuntimeBase: pointer + pointer */
	offset = sizeof(void *) * 2;

	/* Layout fields after CFRuntimeBase */
	for (field = cls->fields; field != NULL; field = field->next) {
		if (field->is_static)
			continue;

		size_t field_size = field->type->size;
		size_t field_align = field->type->alignment;

		if (field_align == 0)
			field_align = 1;
		if (field_align > max_align)
			max_align = field_align;

		offset = (offset + field_align - 1) & ~(field_align - 1);
		field->offset = offset;
		offset += field_size;
	}

	offset = (offset + max_align - 1) & ~(max_align - 1);
	cls->size = offset;
	cls->alignment = max_align;
}

const abi_ops_t corefoundation_abi_ops = {
	.mangle_function = cf_mangle_function,
	.mangle_variable = cf_mangle_variable,
	.mangle_type = cf_mangle_type_str,
	.mangle_vtable = cf_mangle_vtable,
	.mangle_rtti = cf_mangle_rtti,
	.layout_class = cf_layout_class,
};
