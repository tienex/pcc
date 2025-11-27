/*
 * C# Runtime - Type System Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/csruntime.h"

/* Type registry */
#define MAX_TYPES 1024
static CSTypeInfo *type_registry[MAX_TYPES];
static int type_count = 0;

/* Built-in types */
static CSTypeInfo builtin_void = {
	.type_id = 0,
	.kind = CS_TYPEKIND_VOID,
	.name = "Void",
	.namespace = "System",
	.size = 0,
	.alignment = 0,
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_bool = {
	.type_id = 1,
	.kind = CS_TYPEKIND_BOOL,
	.name = "Boolean",
	.namespace = "System",
	.size = sizeof(CSBool),
	.alignment = _Alignof(CSBool),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_char = {
	.type_id = 2,
	.kind = CS_TYPEKIND_CHAR,
	.name = "Char",
	.namespace = "System",
	.size = sizeof(CSChar),
	.alignment = _Alignof(CSChar),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_byte = {
	.type_id = 3,
	.kind = CS_TYPEKIND_BYTE,
	.name = "Byte",
	.namespace = "System",
	.size = sizeof(uint8_t),
	.alignment = _Alignof(uint8_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_sbyte = {
	.type_id = 4,
	.kind = CS_TYPEKIND_SBYTE,
	.name = "SByte",
	.namespace = "System",
	.size = sizeof(int8_t),
	.alignment = _Alignof(int8_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_short = {
	.type_id = 5,
	.kind = CS_TYPEKIND_SHORT,
	.name = "Int16",
	.namespace = "System",
	.size = sizeof(int16_t),
	.alignment = _Alignof(int16_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_ushort = {
	.type_id = 6,
	.kind = CS_TYPEKIND_USHORT,
	.name = "UInt16",
	.namespace = "System",
	.size = sizeof(uint16_t),
	.alignment = _Alignof(uint16_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_int = {
	.type_id = 7,
	.kind = CS_TYPEKIND_INT,
	.name = "Int32",
	.namespace = "System",
	.size = sizeof(int32_t),
	.alignment = _Alignof(int32_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_uint = {
	.type_id = 8,
	.kind = CS_TYPEKIND_UINT,
	.name = "UInt32",
	.namespace = "System",
	.size = sizeof(uint32_t),
	.alignment = _Alignof(uint32_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_long = {
	.type_id = 9,
	.kind = CS_TYPEKIND_LONG,
	.name = "Int64",
	.namespace = "System",
	.size = sizeof(int64_t),
	.alignment = _Alignof(int64_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_ulong = {
	.type_id = 10,
	.kind = CS_TYPEKIND_ULONG,
	.name = "UInt64",
	.namespace = "System",
	.size = sizeof(uint64_t),
	.alignment = _Alignof(uint64_t),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_float = {
	.type_id = 11,
	.kind = CS_TYPEKIND_FLOAT,
	.name = "Single",
	.namespace = "System",
	.size = sizeof(float),
	.alignment = _Alignof(float),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_double = {
	.type_id = 12,
	.kind = CS_TYPEKIND_DOUBLE,
	.name = "Double",
	.namespace = "System",
	.size = sizeof(double),
	.alignment = _Alignof(double),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 1,
	.is_reference_type = 0,
};

static CSTypeInfo builtin_string = {
	.type_id = 13,
	.kind = CS_TYPEKIND_STRING,
	.name = "String",
	.namespace = "System",
	.size = sizeof(CSString),
	.alignment = _Alignof(CSString),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 0,
	.is_reference_type = 1,
};

static CSTypeInfo builtin_object = {
	.type_id = 14,
	.kind = CS_TYPEKIND_OBJECT,
	.name = "Object",
	.namespace = "System",
	.size = sizeof(CSObject),
	.alignment = _Alignof(CSObject),
	.base_type = NULL,
	.interfaces = NULL,
	.interface_count = 0,
	.is_value_type = 0,
	.is_reference_type = 1,
};

/* Initialize type system */
static void init_type_system(void) {
	static int initialized = 0;
	if (initialized) return;

	/* Register built-in types */
	CS_RegisterType(&builtin_void);
	CS_RegisterType(&builtin_bool);
	CS_RegisterType(&builtin_char);
	CS_RegisterType(&builtin_byte);
	CS_RegisterType(&builtin_sbyte);
	CS_RegisterType(&builtin_short);
	CS_RegisterType(&builtin_ushort);
	CS_RegisterType(&builtin_int);
	CS_RegisterType(&builtin_uint);
	CS_RegisterType(&builtin_long);
	CS_RegisterType(&builtin_ulong);
	CS_RegisterType(&builtin_float);
	CS_RegisterType(&builtin_double);
	CS_RegisterType(&builtin_string);
	CS_RegisterType(&builtin_object);

	initialized = 1;
}

/* ========== Type Registration ========== */

void CS_RegisterType(CSTypeInfo *type) {
	init_type_system();

	if (!type) return;
	if (type_count >= MAX_TYPES) {
		fprintf(stderr, "CS_RegisterType: Type registry full\n");
		return;
	}

	type_registry[type_count++] = type;
}

CSTypeInfo *CS_GetTypeInfo(uint32_t type_id) {
	init_type_system();

	for (int i = 0; i < type_count; i++) {
		if (type_registry[i]->type_id == type_id) {
			return type_registry[i];
		}
	}

	return NULL;
}

CSTypeInfo *CS_GetTypeByName(const char *name) {
	init_type_system();

	if (!name) return NULL;

	for (int i = 0; i < type_count; i++) {
		if (strcmp(type_registry[i]->name, name) == 0) {
			return type_registry[i];
		}
	}

	return NULL;
}

/* ========== Type Checking ========== */

int CS_IsInstanceOf(CSObject *obj, CSTypeInfo *type) {
	if (!obj || !type) return 0;

	/* Get object's type */
	CSTypeInfo *obj_type = (CSTypeInfo *)obj->header.type_info;
	if (!obj_type) return 0;

	/* Check if types match */
	if (obj_type == type) return 1;

	/* Check base types */
	CSTypeInfo *base = obj_type->base_type;
	while (base) {
		if (base == type) return 1;
		base = base->base_type;
	}

	/* Check interfaces */
	for (int i = 0; i < obj_type->interface_count; i++) {
		if (obj_type->interfaces[i] == type) return 1;
	}

	return 0;
}

int CS_IsAssignableFrom(CSTypeInfo *from, CSTypeInfo *to) {
	if (!from || !to) return 0;

	/* Same type */
	if (from == to) return 1;

	/* Check inheritance chain */
	CSTypeInfo *base = from->base_type;
	while (base) {
		if (base == to) return 1;
		base = base->base_type;
	}

	/* Check interfaces */
	for (int i = 0; i < from->interface_count; i++) {
		if (from->interfaces[i] == to) return 1;
	}

	return 0;
}

/* ========== Boxing and Unboxing ========== */

CSObject *CS_Box(void *value, CSTypeInfo *value_type) {
	if (!value || !value_type) return NULL;

	/* Allocate boxed object */
	size_t obj_size = sizeof(CSObject) + value_type->size;
	CSObject *obj = CS_AllocObject(obj_size, value_type->type_id);
	if (!obj) return NULL;

	/* Copy value */
	void *data = (char *)obj + sizeof(CSObject);
	memcpy(data, value, value_type->size);

	return obj;
}

void *CS_Unbox(CSObject *obj) {
	if (!obj) return NULL;

	/* Return pointer to value data (after CSObject header) */
	return (char *)obj + sizeof(CSObject);
}

/* ========== Default Values ========== */

void CS_GetDefaultValue(CSTypeInfo *type, void *dest) {
	if (!type || !dest) return;

	/* Zero-initialize */
	memset(dest, 0, type->size);
}

/* ========== Type Name Helpers ========== */

const char *CS_GetTypeName(CSObject *obj) {
	if (!obj) return NULL;

	CSTypeInfo *type = (CSTypeInfo *)obj->header.type_info;
	return type ? type->name : NULL;
}

const char *CS_GetFullTypeName(CSObject *obj) {
	if (!obj) return NULL;

	CSTypeInfo *type = (CSTypeInfo *)obj->header.type_info;
	if (!type) return NULL;

	/* Simplified - would normally format as Namespace.Name */
	static char buffer[256];
	if (type->namespace) {
		snprintf(buffer, sizeof(buffer), "%s.%s", type->namespace, type->name);
	} else {
		snprintf(buffer, sizeof(buffer), "%s", type->name);
	}

	return buffer;
}
