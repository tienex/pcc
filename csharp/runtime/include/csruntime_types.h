/*
 * C# Runtime - Type System Support
 */

#ifndef _CSRUNTIME_TYPES_H_
#define _CSRUNTIME_TYPES_H_

#include <stdint.h>
#include <stddef.h>

/* Type kinds */
typedef enum {
	CS_TYPEKIND_VOID,
	CS_TYPEKIND_BOOL,
	CS_TYPEKIND_CHAR,
	CS_TYPEKIND_BYTE,
	CS_TYPEKIND_SBYTE,
	CS_TYPEKIND_SHORT,
	CS_TYPEKIND_USHORT,
	CS_TYPEKIND_INT,
	CS_TYPEKIND_UINT,
	CS_TYPEKIND_LONG,
	CS_TYPEKIND_ULONG,
	CS_TYPEKIND_FLOAT,
	CS_TYPEKIND_DOUBLE,
	CS_TYPEKIND_DECIMAL,
	CS_TYPEKIND_STRING,
	CS_TYPEKIND_OBJECT,
	CS_TYPEKIND_CLASS,
	CS_TYPEKIND_STRUCT,
	CS_TYPEKIND_INTERFACE,
	CS_TYPEKIND_ENUM,
	CS_TYPEKIND_DELEGATE,
	CS_TYPEKIND_ARRAY,
} CSTypeKind;

/* Compatibility aliases */
#define CS_TYPE_VOID   CS_TYPEKIND_VOID
#define CS_TYPE_BOOL   CS_TYPEKIND_BOOL
#define CS_TYPE_CHAR   CS_TYPEKIND_CHAR
#define CS_TYPE_BYTE   CS_TYPEKIND_BYTE
#define CS_TYPE_SBYTE  CS_TYPEKIND_SBYTE
#define CS_TYPE_INT16  CS_TYPEKIND_SHORT
#define CS_TYPE_UINT16 CS_TYPEKIND_USHORT
#define CS_TYPE_INT32  CS_TYPEKIND_INT
#define CS_TYPE_UINT32 CS_TYPEKIND_UINT
#define CS_TYPE_INT64  CS_TYPEKIND_LONG
#define CS_TYPE_UINT64 CS_TYPEKIND_ULONG
#define CS_TYPE_FLOAT  CS_TYPEKIND_FLOAT
#define CS_TYPE_DOUBLE CS_TYPEKIND_DOUBLE
#define CS_TYPE_STRING CS_TYPEKIND_STRING
#define CS_TYPE_OBJECT CS_TYPEKIND_OBJECT

/* Type information */
typedef struct CSTypeInfo {
	uint32_t type_id;
	CSTypeKind kind;
	const char *name;
	const char *namespace;
	size_t size;
	size_t alignment;
	struct CSTypeInfo *base_type;
	struct CSTypeInfo **interfaces;
	int interface_count;
	int is_value_type;
	int is_reference_type;
} CSTypeInfo;

/* System.Object - base type for all reference types */
typedef struct {
	CSObject base;
} CSSystemObject;

/* System.String */
typedef struct {
	CSObject base;
	int32_t length;
	uint16_t *chars;  /* UTF-16 characters */
} CSString;

/* System.Boolean */
typedef uint8_t CSBool;
#define CS_TRUE  1
#define CS_FALSE 0

/* System.Char (UTF-16) */
typedef uint16_t CSChar;

/* System.Decimal (128-bit) */
typedef struct {
	uint64_t lo64;
	uint32_t hi32;
	uint32_t flags;
} CSDecimal;

/* Type registration */
void CS_RegisterType(CSTypeInfo *type);
CSTypeInfo *CS_GetTypeInfo(uint32_t type_id);
CSTypeInfo *CS_GetTypeByName(const char *name);

/* Type checking */
int CS_IsInstanceOf(CSObject *obj, CSTypeInfo *type);
int CS_IsAssignableFrom(CSTypeInfo *from, CSTypeInfo *to);

/* Boxing and unboxing */
CSObject *CS_Box(void *value, CSTypeInfo *value_type);
void *CS_Unbox(CSObject *obj);

/* Default values */
void CS_GetDefaultValue(CSTypeInfo *type, void *dest);

/* Type name helpers */
const char *CS_GetTypeName(CSObject *obj);
const char *CS_GetFullTypeName(CSObject *obj);

#endif /* _CSRUNTIME_TYPES_H_ */
