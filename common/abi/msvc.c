/*
 * Copyright (c) 2025 PCC Project
 *
 * Microsoft Visual C++ ABI implementation
 *
 * Used by:
 * - Microsoft Visual C++ on Windows
 * - MinGW-w64 (with -fms-extensions)
 * - Clang with -fms-compatibility
 *
 * Key differences from Itanium ABI:
 * - Different name mangling scheme
 * - Different vtable layout
 * - Different virtual inheritance model
 * - __thiscall calling convention for methods
 * - Multiple inheritance uses virtual function table offsets (vftable)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "abi.h"

/* Mangling buffer helpers */
#define MANGLE_APPEND(ctx, str) do { \
	size_t len = strlen(str); \
	if ((ctx)->pos + len >= (ctx)->bufsize) { \
		(ctx)->bufsize *= 2; \
		(ctx)->buffer = realloc((ctx)->buffer, (ctx)->bufsize); \
	} \
	memcpy((ctx)->buffer + (ctx)->pos, str, len); \
	(ctx)->pos += len; \
	(ctx)->buffer[(ctx)->pos] = '\0'; \
} while (0)

#define MANGLE_APPENDC(ctx, c) do { \
	if ((ctx)->pos + 1 >= (ctx)->bufsize) { \
		(ctx)->bufsize *= 2; \
		(ctx)->buffer = realloc((ctx)->buffer, (ctx)->bufsize); \
	} \
	(ctx)->buffer[(ctx)->pos++] = (c); \
	(ctx)->buffer[(ctx)->pos] = '\0'; \
} while (0)

/*
 * MSVC name mangling
 *
 * Format: ? <name> <qualifiers> <return-type> <parameters>
 *
 * Access qualifiers:
 *   private:   0, 1 (static)
 *   protected: 2, 3 (static)
 *   public:    4, 5 (static)
 *
 * Type encodings:
 *   X = void, D = char, C = signed char, E = unsigned char
 *   F = short, G = unsigned short, H = int, I = unsigned int
 *   J = long, K = unsigned long, _J = __int64, _K = unsigned __int64
 *   M = float, N = double, O = long double
 *   P = pointer, A = reference, $ = rvalue reference
 *
 * Calling conventions:
 *   A = __cdecl, C = __pascal, E = __thiscall
 *   G = __stdcall, I = __fastcall
 */

/* Forward declarations */
static void msvc_mangle_name(abi_mangle_ctx_t *ctx, const char *name);
static void msvc_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type);
static void msvc_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind);
static void msvc_mangle_function_class(abi_mangle_ctx_t *ctx, const abi_function_t *func);
static void msvc_mangle_calling_conv(abi_mangle_ctx_t *ctx, abi_calling_conv_t cc);
static void msvc_mangle_access(abi_mangle_ctx_t *ctx, int access, int is_static);
static void msvc_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params);

/*
 * Mangle a function name
 *
 * Format:
 * ? <name> @ <class> @ <access> <calling-conv> <return-type> <params> @Z
 */
static char *
msvc_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (func == NULL)
		return NULL;

	/* C functions are not mangled (prefixed with _ on x86) */
	if (func->calling_conv == ABI_CC_C && !func->parent_class) {
		result = malloc(strlen(func->name) + 2);
		sprintf(result, "_%s", func->name);
		return result;
	}

	/* Initialize mangling context */
	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_MSVC;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* All C++ symbols start with ? */
	MANGLE_APPENDC(&mctx, '?');

	/* Function name */
	if (func->is_constructor) {
		MANGLE_APPENDC(&mctx, '?');
		MANGLE_APPENDC(&mctx, '0');
	} else if (func->is_destructor) {
		MANGLE_APPENDC(&mctx, '?');
		MANGLE_APPENDC(&mctx, '1');
	} else {
		msvc_mangle_name(&mctx, func->name);
	}

	MANGLE_APPENDC(&mctx, '@');

	/* Class name (if method) */
	if (func->parent_class) {
		msvc_mangle_name(&mctx, func->parent_class->name);
		MANGLE_APPENDC(&mctx, '@');
	}

	/* Terminator for name */
	MANGLE_APPENDC(&mctx, '@');

	/* Access level and storage class */
	if (func->parent_class) {
		msvc_mangle_access(&mctx, 2, func->is_static);  /* assume public */

		/* Virtual qualifier */
		if (func->is_virtual)
			MANGLE_APPENDC(&mctx, 'E');
	} else {
		/* Global function */
		MANGLE_APPENDC(&mctx, 'Y');
	}

	/* Calling convention */
	msvc_mangle_calling_conv(&mctx, func->calling_conv);

	/* Return type (unless constructor/destructor) */
	if (!func->is_constructor && !func->is_destructor) {
		if (func->return_type)
			msvc_mangle_type_internal(&mctx, func->return_type);
		else
			MANGLE_APPENDC(&mctx, 'X');  /* void */
	}

	/* Parameters */
	msvc_mangle_params(&mctx, func->params);

	/* Terminator */
	MANGLE_APPENDC(&mctx, '@');
	MANGLE_APPENDC(&mctx, 'Z');

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a name (without length prefix, unlike Itanium)
 */
static void
msvc_mangle_name(abi_mangle_ctx_t *ctx, const char *name)
{
	if (name == NULL)
		return;

	MANGLE_APPEND(ctx, name);
}

/*
 * Mangle access specifier
 */
static void
msvc_mangle_access(abi_mangle_ctx_t *ctx, int access, int is_static)
{
	/* access: 0=private, 1=protected, 2=public */
	/* Encoding: 0-1 (private), 2-3 (protected), 4-5 (public) */
	/* Even = non-static, odd = static */

	int code = access * 2 + (is_static ? 1 : 0);
	MANGLE_APPENDC(ctx, '0' + code);
}

/*
 * Mangle calling convention
 */
static void
msvc_mangle_calling_conv(abi_mangle_ctx_t *ctx, abi_calling_conv_t cc)
{
	switch (cc) {
	case ABI_CC_C:
	case ABI_CC_CDECL:
		MANGLE_APPENDC(ctx, 'A');
		break;
	case ABI_CC_PASCAL:
		MANGLE_APPENDC(ctx, 'C');
		break;
	case ABI_CC_THISCALL:
		MANGLE_APPENDC(ctx, 'E');
		break;
	case ABI_CC_STDCALL:
		MANGLE_APPENDC(ctx, 'G');
		break;
	case ABI_CC_FASTCALL:
		MANGLE_APPENDC(ctx, 'I');
		break;
	case ABI_CC_VECTORCALL:
		MANGLE_APPENDC(ctx, 'Q');
		break;
	default:
		MANGLE_APPENDC(ctx, 'A');  /* Default to cdecl */
		break;
	}
}

/*
 * Mangle parameters
 */
static void
msvc_mangle_params(abi_mangle_ctx_t *ctx, const abi_param_t *params)
{
	const abi_param_t *p;

	if (params == NULL) {
		MANGLE_APPENDC(ctx, 'X');  /* void */
		return;
	}

	for (p = params; p != NULL; p = p->next) {
		msvc_mangle_type_internal(ctx, p->type);
	}
}

/*
 * Mangle a type
 */
static void
msvc_mangle_type_internal(abi_mangle_ctx_t *ctx, const abi_type_t *type)
{
	if (type == NULL) {
		MANGLE_APPENDC(ctx, 'X');  /* void */
		return;
	}

	/* Const/volatile qualifiers come before the type */
	if (type->is_const && type->is_volatile)
		MANGLE_APPENDC(ctx, 'D');
	else if (type->is_const)
		MANGLE_APPENDC(ctx, 'B');
	else if (type->is_volatile)
		MANGLE_APPENDC(ctx, 'C');

	/* Base type */
	switch (type->kind) {
	case ABI_TYPE_VOID:
	case ABI_TYPE_BOOL:
	case ABI_TYPE_CHAR:
	case ABI_TYPE_SCHAR:
	case ABI_TYPE_UCHAR:
	case ABI_TYPE_SHORT:
	case ABI_TYPE_USHORT:
	case ABI_TYPE_INT:
	case ABI_TYPE_UINT:
	case ABI_TYPE_LONG:
	case ABI_TYPE_ULONG:
	case ABI_TYPE_LONGLONG:
	case ABI_TYPE_ULONGLONG:
	case ABI_TYPE_FLOAT:
	case ABI_TYPE_DOUBLE:
	case ABI_TYPE_LONGDOUBLE:
		msvc_mangle_builtin_type(ctx, type->kind);
		break;

	case ABI_TYPE_POINTER:
		if (type->is_const)
			MANGLE_APPENDC(ctx, 'Q');
		else
			MANGLE_APPENDC(ctx, 'P');
		msvc_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_REFERENCE:
		MANGLE_APPENDC(ctx, 'A');
		msvc_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_RVALUE_REFERENCE:
		MANGLE_APPENDC(ctx, '$');
		MANGLE_APPENDC(ctx, '$');
		msvc_mangle_type_internal(ctx, type->u.pointee);
		break;

	case ABI_TYPE_ARRAY:
		MANGLE_APPENDC(ctx, 'Y');
		/* Array size would go here */
		msvc_mangle_type_internal(ctx, type->u.element);
		break;

	case ABI_TYPE_CLASS:
	case ABI_TYPE_STRUCT:
		/* Class: V<name>@@ */
		MANGLE_APPENDC(ctx, 'V');
		if (type->u.class_type && type->u.class_type->name)
			msvc_mangle_name(ctx, type->u.class_type->name);
		MANGLE_APPENDC(ctx, '@');
		MANGLE_APPENDC(ctx, '@');
		break;

	case ABI_TYPE_UNION:
		/* Union: T<name>@@ */
		MANGLE_APPENDC(ctx, 'T');
		if (type->u.class_type && type->u.class_type->name)
			msvc_mangle_name(ctx, type->u.class_type->name);
		MANGLE_APPENDC(ctx, '@');
		MANGLE_APPENDC(ctx, '@');
		break;

	default:
		MANGLE_APPENDC(ctx, 'X');  /* Unknown = void */
		break;
	}
}

/*
 * Mangle built-in type
 */
static void
msvc_mangle_builtin_type(abi_mangle_ctx_t *ctx, abi_type_kind_t kind)
{
	switch (kind) {
	case ABI_TYPE_VOID:           MANGLE_APPENDC(ctx, 'X'); break;
	case ABI_TYPE_BOOL:           MANGLE_APPEND(ctx, "_N"); break;
	case ABI_TYPE_CHAR:           MANGLE_APPENDC(ctx, 'D'); break;
	case ABI_TYPE_SCHAR:          MANGLE_APPENDC(ctx, 'C'); break;
	case ABI_TYPE_UCHAR:          MANGLE_APPENDC(ctx, 'E'); break;
	case ABI_TYPE_SHORT:          MANGLE_APPENDC(ctx, 'F'); break;
	case ABI_TYPE_USHORT:         MANGLE_APPENDC(ctx, 'G'); break;
	case ABI_TYPE_INT:            MANGLE_APPENDC(ctx, 'H'); break;
	case ABI_TYPE_UINT:           MANGLE_APPENDC(ctx, 'I'); break;
	case ABI_TYPE_LONG:           MANGLE_APPENDC(ctx, 'J'); break;
	case ABI_TYPE_ULONG:          MANGLE_APPENDC(ctx, 'K'); break;
	case ABI_TYPE_LONGLONG:       MANGLE_APPEND(ctx, "_J"); break;
	case ABI_TYPE_ULONGLONG:      MANGLE_APPEND(ctx, "_K"); break;
	case ABI_TYPE_FLOAT:          MANGLE_APPENDC(ctx, 'M'); break;
	case ABI_TYPE_DOUBLE:         MANGLE_APPENDC(ctx, 'N'); break;
	case ABI_TYPE_LONGDOUBLE:     MANGLE_APPENDC(ctx, 'O'); break;
	default:                      MANGLE_APPENDC(ctx, 'X'); break;
	}
}

/*
 * Mangle a variable name
 */
static char *
msvc_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_MSVC;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	/* Global variable: ?<name>@@3<type>A */
	MANGLE_APPENDC(&mctx, '?');
	msvc_mangle_name(&mctx, name);
	MANGLE_APPENDC(&mctx, '@');
	MANGLE_APPENDC(&mctx, '@');
	MANGLE_APPENDC(&mctx, '3');
	msvc_mangle_type_internal(&mctx, type);
	MANGLE_APPENDC(&mctx, 'A');

	result = mctx.buffer;
	return result;
}

/*
 * Mangle a type (standalone)
 */
static char *
msvc_mangle_type(abi_context_t *ctx, const abi_type_t *type)
{
	abi_mangle_ctx_t mctx;
	char *result;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_MSVC;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	msvc_mangle_type_internal(&mctx, type);

	result = mctx.buffer;
	return result;
}

/*
 * Mangle vtable name
 * Format: ??_7<class>@@6B@
 */
static char *
msvc_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_MSVC;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "??_7");
	msvc_mangle_name(&mctx, cls->name);
	MANGLE_APPEND(&mctx, "@@6B@");

	result = mctx.buffer;
	return result;
}

/*
 * Mangle RTTI Complete Object Locator
 * Format: ??_R4<class>@@6B@
 */
static char *
msvc_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_mangle_ctx_t mctx;
	char *result;

	if (cls == NULL)
		return NULL;

	memset(&mctx, 0, sizeof(mctx));
	mctx.abi = ABI_MSVC;
	mctx.bufsize = 256;
	mctx.buffer = malloc(mctx.bufsize);
	mctx.buffer[0] = '\0';

	MANGLE_APPEND(&mctx, "??_R4");
	msvc_mangle_name(&mctx, cls->name);
	MANGLE_APPEND(&mctx, "@@6B@");

	result = mctx.buffer;
	return result;
}

/*
 * MSVC class layout
 *
 * Key differences from Itanium:
 * - Vtable pointer is always at offset 0
 * - Virtual base classes use virtual base table (vbtable)
 * - Multiple inheritance creates multiple vftables
 * - #pragma pack affects layout
 */

static void
msvc_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	size_t offset = 0;
	abi_field_t *field;

	if (cls == NULL)
		return;

	/* Check if already laid out */
	if (cls->size > 0)
		return;

	cls->alignment = 1;

	/* Vtable pointer at offset 0 */
	if (cls->has_vtable) {
		cls->vtable_offset = 0;
		offset = ctx->pointer_size;
		cls->alignment = ctx->pointer_align;
	}

	/* Layout bases */
	if (ctx->ops && ctx->ops->layout_bases)
		ctx->ops->layout_bases(ctx, cls);

	/* Layout non-static data members */
	for (field = cls->fields; field != NULL; field = field->next) {
		size_t field_size, field_align;

		if (field->is_static)
			continue;

		field_size = abi_sizeof_type(ctx, field->type);
		field_align = abi_alignof_type(ctx, field->type);

		/* Align field */
		offset = (offset + field_align - 1) & ~(field_align - 1);
		field->offset = offset;
		offset += field_size;

		/* Update class alignment */
		if (field_align > cls->alignment)
			cls->alignment = field_align;
	}

	/* Align final size */
	cls->size = (offset + cls->alignment - 1) & ~(cls->alignment - 1);

	/* Empty class has size 1 */
	if (cls->size == 0) {
		cls->size = 1;
		cls->is_empty = 1;
	}

	/* Check if POD */
	cls->is_pod = (!cls->has_vtable && !cls->has_vbases);
}

static void
msvc_layout_bases(abi_context_t *ctx, abi_class_t *cls)
{
	abi_base_t *base;

	/* Layout non-virtual bases */
	for (base = cls->bases; base != NULL; base = base->next) {
		if (!base->is_virtual) {
			/* Recursively layout base class */
			if (base->base_class->size == 0)
				abi_layout_class(ctx, base->base_class);

			base->offset = 0;  /* MSVC lays out bases at offset 0 typically */
		}
	}
}

static void
msvc_layout_fields(abi_context_t *ctx, abi_class_t *cls)
{
	/* Already done in layout_class */
}

static void
msvc_layout_vtable(abi_context_t *ctx, abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t index = 0;

	if (!cls->has_vtable)
		return;

	/* Assign vtable indices */
	for (v = cls->virtuals; v != NULL; v = v->next) {
		v->vtable_index = index++;
	}
}

/*
 * Compute vtable size
 */
static size_t
msvc_compute_vtable_size(abi_context_t *ctx, const abi_class_t *cls)
{
	abi_virtual_func_t *v;
	size_t count = 0;

	/* Count virtual functions */
	for (v = cls->virtuals; v != NULL; v = v->next)
		count++;

	return count;
}

/*
 * Build vtable
 */
static void
msvc_build_vtable(abi_context_t *ctx, abi_class_t *cls, void **vtable)
{
	abi_virtual_func_t *v;
	size_t i = 0;

	if (!cls->has_vtable || vtable == NULL)
		return;

	/* Virtual function pointers (no RTTI slots at beginning) */
	for (v = cls->virtuals; v != NULL; v = v->next) {
		vtable[i++] = NULL;  /* Would point to function */
	}
}

/*
 * Type size/alignment
 */
static size_t
msvc_sizeof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->size;
}

static size_t
msvc_alignof_type(abi_context_t *ctx, const abi_type_t *type)
{
	return type->alignment;
}

/*
 * MSVC ABI operations table
 */
const abi_ops_t msvc_abi_ops = {
	.mangle_function = msvc_mangle_function,
	.mangle_variable = msvc_mangle_variable,
	.mangle_type = msvc_mangle_type,
	.mangle_vtable = msvc_mangle_vtable,
	.mangle_rtti = msvc_mangle_rtti,
	.layout_class = msvc_layout_class,
	.layout_bases = msvc_layout_bases,
	.layout_fields = msvc_layout_fields,
	.layout_vtable = msvc_layout_vtable,
	.compute_vtable_size = msvc_compute_vtable_size,
	.build_vtable = msvc_build_vtable,
	.sizeof_type = msvc_sizeof_type,
	.alignof_type = msvc_alignof_type,
};
