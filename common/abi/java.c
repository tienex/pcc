/*
 * Copyright (c) 2025 PCC Project
 *
 * Java ABI implementation (JNI/JVM)
 *
 * Java name mangling for JNI (Java Native Interface):
 *   Java_<class>_<method>
 *   Java_<package>_<class>_<method>
 *
 * Example:
 *   package.ClassName.methodName -> Java_package_ClassName_methodName
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "abi.h"

static char *
java_mangle_name_part(const char *name)
{
	static char buf[256];
	int i, j = 0;

	for (i = 0; name[i] && j < 255; i++) {
		if (name[i] == '.') buf[j++] = '_';
		else if (name[i] == '_') {
			buf[j++] = '_';
			buf[j++] = '1';
		} else buf[j++] = name[i];
	}
	buf[j] = '\0';
	return buf;
}

static char *
java_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	char buf[512];

	if (func == NULL) return NULL;

	if (func->parent_class && func->parent_class->name) {
		snprintf(buf, sizeof(buf), "Java_%s_%s",
		         java_mangle_name_part(func->parent_class->name),
		         java_mangle_name_part(func->name));
	} else {
		snprintf(buf, sizeof(buf), "Java_%s", java_mangle_name_part(func->name));
	}

	return strdup(buf);
}

static char *
java_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	return strdup(java_mangle_name_part(name));
}

static char *
java_mangle_type_str(abi_context_t *ctx, const abi_type_t *type)
{
	if (!type) return strdup("V");
	switch (type->kind) {
	case ABI_TYPE_VOID: return strdup("V");
	case ABI_TYPE_BOOL: return strdup("Z");
	case ABI_TYPE_CHAR: return strdup("C");
	case ABI_TYPE_SHORT: return strdup("S");
	case ABI_TYPE_INT: return strdup("I");
	case ABI_TYPE_LONG: return strdup("J");
	case ABI_TYPE_FLOAT: return strdup("F");
	case ABI_TYPE_DOUBLE: return strdup("D");
	case ABI_TYPE_CLASS: return strdup("Ljava/lang/Object;");
	default: return strdup("V");
	}
}

static char *
java_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_vtable_%s", java_mangle_name_part(cls->name));
	return strdup(buf);
}

static char *
java_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (!cls) return NULL;
	char buf[256];
	snprintf(buf, sizeof(buf), "_class_%s", java_mangle_name_part(cls->name));
	return strdup(buf);
}

const abi_ops_t java_abi_ops = {
	.mangle_function = java_mangle_function,
	.mangle_variable = java_mangle_variable,
	.mangle_type = java_mangle_type_str,
	.mangle_vtable = java_mangle_vtable,
	.mangle_rtti = java_mangle_rtti,
};
