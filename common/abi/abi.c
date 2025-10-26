/*
 * Copyright (c) 2025 PCC Project
 *
 * Generic ABI library - Core implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "abi.h"

/* Forward declarations for ABI-specific operations */
extern const abi_ops_t itanium_abi_ops;
extern const abi_ops_t msvc_abi_ops;
extern const abi_ops_t watcom_abi_ops;
extern const abi_ops_t borland_abi_ops;
extern const abi_ops_t gnu_old_abi_ops;
extern const abi_ops_t dmc_abi_ops;
extern const abi_ops_t cfront_abi_ops;
extern const abi_ops_t edg_abi_ops;
extern const abi_ops_t apple_objc1_abi_ops;
extern const abi_ops_t apple_objc2_abi_ops;
extern const abi_ops_t gnu_objc_abi_ops;
extern const abi_ops_t corefoundation_abi_ops;
extern const abi_ops_t dlang_abi_ops;
extern const abi_ops_t swift_abi_ops;
extern const abi_ops_t rust_abi_ops;
extern const abi_ops_t go_abi_ops;
extern const abi_ops_t zig_abi_ops;
extern const abi_ops_t crystal_abi_ops;
extern const abi_ops_t nim_abi_ops;
extern const abi_ops_t vlang_abi_ops;
extern const abi_ops_t julia_abi_ops;
extern const abi_ops_t java_abi_ops;
extern const abi_ops_t clr_abi_ops;
extern const abi_ops_t dart_abi_ops;
extern const abi_ops_t kotlin_abi_ops;
extern const abi_ops_t ghc_abi_ops;
extern const abi_ops_t ocaml_abi_ops;
extern const abi_ops_t fsharp_abi_ops;
extern const abi_ops_t freepascal_abi_ops;
extern const abi_ops_t gnu_pascal_abi_ops;
extern const abi_ops_t gfortran_abi_ops;
extern const abi_ops_t ifort_abi_ops;
extern const abi_ops_t nag_fortran_abi_ops;
extern const abi_ops_t gnat_abi_ops;
extern const abi_ops_t ecere_abi_ops;
extern const abi_ops_t llvm_ir_abi_ops;
extern const abi_ops_t wasm_abi_ops;

/*
 * Initialize ABI context
 */
abi_context_t *
abi_init(abi_kind_t kind)
{
	abi_context_t *ctx;

	ctx = calloc(1, sizeof(abi_context_t));
	if (ctx == NULL)
		return NULL;

	ctx->kind = kind;
	ctx->pointer_size = sizeof(void *);
	ctx->pointer_align = sizeof(void *);

	/* Set up operations based on ABI */
	switch (kind) {
	/* C++ ABIs */
	case ABI_ITANIUM:
	case ABI_ARM:
		ctx->ops = &itanium_abi_ops;
		break;
	case ABI_MSVC:
		ctx->ops = &msvc_abi_ops;
		break;
	case ABI_WATCOM:
		ctx->ops = &watcom_abi_ops;
		break;
	case ABI_BORLAND:
		ctx->ops = &borland_abi_ops;
		break;
	case ABI_GNU_OLD:
		ctx->ops = &gnu_old_abi_ops;
		break;
	case ABI_DMC:
		ctx->ops = &dmc_abi_ops;
		break;
	case ABI_CFRONT:
		ctx->ops = &cfront_abi_ops;
		break;
	case ABI_EDG:
		ctx->ops = &edg_abi_ops;
		break;
	case ABI_SUN:
	case ABI_INTEL:
	case ABI_IBM:
	case ABI_HP:
		/* These use Itanium ABI with minor variations */
		ctx->ops = &itanium_abi_ops;
		break;

	/* Objective-C ABIs */
	case ABI_APPLE_OBJC1:
		ctx->ops = &apple_objc1_abi_ops;
		break;
	case ABI_APPLE_OBJC2:
		ctx->ops = &apple_objc2_abi_ops;
		break;
	case ABI_GNU_OBJC:
		ctx->ops = &gnu_objc_abi_ops;
		break;
	case ABI_COREFOUNDATION:
		ctx->ops = &corefoundation_abi_ops;
		break;

	/* Modern System Languages */
	case ABI_DLANG:
		ctx->ops = &dlang_abi_ops;
		break;
	case ABI_SWIFT:
		ctx->ops = &swift_abi_ops;
		break;
	case ABI_RUST:
		ctx->ops = &rust_abi_ops;
		break;
	case ABI_GO:
		ctx->ops = &go_abi_ops;
		break;
	case ABI_ZIG:
		ctx->ops = &zig_abi_ops;
		break;
	case ABI_CRYSTAL:
		ctx->ops = &crystal_abi_ops;
		break;
	case ABI_NIM:
		ctx->ops = &nim_abi_ops;
		break;
	case ABI_VLANG:
		ctx->ops = &vlang_abi_ops;
		break;
	case ABI_JULIA:
		ctx->ops = &julia_abi_ops;
		break;

	/* VM/Managed Languages */
	case ABI_JAVA:
		ctx->ops = &java_abi_ops;
		break;
	case ABI_CLR:
		ctx->ops = &clr_abi_ops;
		break;
	case ABI_DART:
		ctx->ops = &dart_abi_ops;
		break;
	case ABI_KOTLIN:
		ctx->ops = &kotlin_abi_ops;
		break;

	/* Functional Languages */
	case ABI_GHC:
		ctx->ops = &ghc_abi_ops;
		break;
	case ABI_OCAML:
		ctx->ops = &ocaml_abi_ops;
		break;
	case ABI_FSHARP:
		ctx->ops = &fsharp_abi_ops;
		break;

	/* Pascal ABIs */
	case ABI_FREEPASCAL:
		ctx->ops = &freepascal_abi_ops;
		break;
	case ABI_GNU_PASCAL:
		ctx->ops = &gnu_pascal_abi_ops;
		break;

	/* Fortran ABIs */
	case ABI_GFORTRAN:
		ctx->ops = &gfortran_abi_ops;
		break;
	case ABI_IFORT:
		ctx->ops = &ifort_abi_ops;
		break;
	case ABI_NAG_FORTRAN:
		ctx->ops = &nag_fortran_abi_ops;
		break;

	/* Ada ABI */
	case ABI_GNAT:
		ctx->ops = &gnat_abi_ops;
		break;

	/* Framework ABIs */
	case ABI_ECERE:
		ctx->ops = &ecere_abi_ops;
		break;

	/* IR/Bytecode ABIs */
	case ABI_LLVM_IR:
		ctx->ops = &llvm_ir_abi_ops;
		break;
	case ABI_WASM:
		ctx->ops = &wasm_abi_ops;
		break;

	default:
		free(ctx);
		return NULL;
	}

	return ctx;
}

/*
 * Destroy ABI context
 */
void
abi_destroy(abi_context_t *ctx)
{
	if (ctx == NULL)
		return;

	/* Free caches */
	/* TODO: implement cache cleanup */

	free(ctx);
}

/*
 * Auto-detect ABI based on target platform
 */
abi_kind_t
abi_detect(const char *target_triple)
{
	if (target_triple == NULL)
		return ABI_ITANIUM;  /* Default */

	/* Windows targets use MSVC ABI */
	if (strstr(target_triple, "windows") ||
	    strstr(target_triple, "mingw") ||
	    strstr(target_triple, "msvc") ||
	    strstr(target_triple, "win32"))
		return ABI_MSVC;

	/* Watcom is explicitly specified */
	if (strstr(target_triple, "watcom"))
		return ABI_WATCOM;

	/* ARM targets may use ARM variant of Itanium */
	if (strstr(target_triple, "arm"))
		return ABI_ARM;

	/* Default to Itanium for Unix/Linux/macOS */
	return ABI_ITANIUM;
}

/*
 * Name mangling wrapper functions
 */
char *
abi_mangle_function(abi_context_t *ctx, const abi_function_t *func)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->mangle_function == NULL)
		return NULL;

	return ctx->ops->mangle_function(ctx, func);
}

char *
abi_mangle_variable(abi_context_t *ctx, const char *name, const abi_type_t *type)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->mangle_variable == NULL)
		return NULL;

	return ctx->ops->mangle_variable(ctx, name, type);
}

char *
abi_mangle_type(abi_context_t *ctx, const abi_type_t *type)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->mangle_type == NULL)
		return NULL;

	return ctx->ops->mangle_type(ctx, type);
}

char *
abi_mangle_vtable(abi_context_t *ctx, const abi_class_t *cls)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->mangle_vtable == NULL)
		return NULL;

	return ctx->ops->mangle_vtable(ctx, cls);
}

char *
abi_mangle_rtti(abi_context_t *ctx, const abi_class_t *cls)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->mangle_rtti == NULL)
		return NULL;

	return ctx->ops->mangle_rtti(ctx, cls);
}

/*
 * Class layout wrapper functions
 */
void
abi_layout_class(abi_context_t *ctx, abi_class_t *cls)
{
	if (ctx == NULL || ctx->ops == NULL || ctx->ops->layout_class == NULL)
		return;

	ctx->ops->layout_class(ctx, cls);
}

size_t
abi_sizeof_class(abi_context_t *ctx, const abi_class_t *cls)
{
	if (cls == NULL)
		return 0;

	return cls->size;
}

size_t
abi_alignof_class(abi_context_t *ctx, const abi_class_t *cls)
{
	if (cls == NULL)
		return 1;

	return cls->alignment;
}

size_t
abi_offsetof_field(abi_context_t *ctx, const abi_class_t *cls, const char *field)
{
	abi_field_t *f;

	if (cls == NULL || field == NULL)
		return (size_t)-1;

	for (f = cls->fields; f != NULL; f = f->next) {
		if (strcmp(f->name, field) == 0)
			return f->offset;
	}

	return (size_t)-1;
}

/*
 * Type operations
 */
abi_type_t *
abi_create_type(abi_type_kind_t kind)
{
	abi_type_t *type;

	type = calloc(1, sizeof(abi_type_t));
	if (type == NULL)
		return NULL;

	type->kind = kind;

	/* Set default sizes */
	switch (kind) {
	case ABI_TYPE_VOID:
		type->size = 0;
		type->alignment = 1;
		break;
	case ABI_TYPE_BOOL:
	case ABI_TYPE_CHAR:
	case ABI_TYPE_SCHAR:
	case ABI_TYPE_UCHAR:
		type->size = 1;
		type->alignment = 1;
		break;
	case ABI_TYPE_SHORT:
	case ABI_TYPE_USHORT:
		type->size = 2;
		type->alignment = 2;
		break;
	case ABI_TYPE_INT:
	case ABI_TYPE_UINT:
	case ABI_TYPE_FLOAT:
		type->size = 4;
		type->alignment = 4;
		break;
	case ABI_TYPE_LONG:
	case ABI_TYPE_ULONG:
	case ABI_TYPE_DOUBLE:
		type->size = 8;
		type->alignment = 8;
		break;
	case ABI_TYPE_LONGLONG:
	case ABI_TYPE_ULONGLONG:
		type->size = 8;
		type->alignment = 8;
		break;
	case ABI_TYPE_LONGDOUBLE:
		type->size = 16;
		type->alignment = 16;
		break;
	case ABI_TYPE_POINTER:
	case ABI_TYPE_REFERENCE:
	case ABI_TYPE_RVALUE_REFERENCE:
		type->size = sizeof(void *);
		type->alignment = sizeof(void *);
		break;
	default:
		type->size = 0;
		type->alignment = 1;
		break;
	}

	return type;
}

void
abi_destroy_type(abi_type_t *type)
{
	if (type == NULL)
		return;

	/* Free type-specific data */
	/* TODO: implement recursive cleanup */

	free(type);
}

size_t
abi_sizeof_type(abi_context_t *ctx, const abi_type_t *type)
{
	if (ctx == NULL || type == NULL)
		return 0;

	if (ctx->ops && ctx->ops->sizeof_type)
		return ctx->ops->sizeof_type(ctx, type);

	return type->size;
}

size_t
abi_alignof_type(abi_context_t *ctx, const abi_type_t *type)
{
	if (ctx == NULL || type == NULL)
		return 1;

	if (ctx->ops && ctx->ops->alignof_type)
		return ctx->ops->alignof_type(ctx, type);

	return type->alignment;
}

/*
 * Class operations
 */
abi_class_t *
abi_create_class(const char *name)
{
	abi_class_t *cls;

	cls = calloc(1, sizeof(abi_class_t));
	if (cls == NULL)
		return NULL;

	cls->name = strdup(name);
	cls->size = 0;
	cls->alignment = 1;

	return cls;
}

void
abi_destroy_class(abi_class_t *cls)
{
	abi_field_t *f, *fnext;
	abi_base_t *b, *bnext;
	abi_virtual_func_t *v, *vnext;

	if (cls == NULL)
		return;

	free((void *)cls->name);
	free((void *)cls->mangled_name);

	/* Free fields */
	for (f = cls->fields; f != NULL; f = fnext) {
		fnext = f->next;
		free((void *)f->name);
		free(f);
	}

	/* Free bases */
	for (b = cls->bases; b != NULL; b = bnext) {
		bnext = b->next;
		free(b);
	}

	/* Free virtuals */
	for (v = cls->virtuals; v != NULL; v = vnext) {
		vnext = v->next;
		free((void *)v->name);
		free(v);
	}

	free(cls->abi_data);
	free(cls);
}

void
abi_add_field(abi_class_t *cls, abi_field_t *field)
{
	abi_field_t **p;

	if (cls == NULL || field == NULL)
		return;

	/* Add to end of list */
	for (p = &cls->fields; *p != NULL; p = &(*p)->next)
		;
	*p = field;
	field->next = NULL;
}

void
abi_add_base(abi_class_t *cls, abi_base_t *base)
{
	abi_base_t **p;

	if (cls == NULL || base == NULL)
		return;

	/* Add to end of list */
	for (p = &cls->bases; *p != NULL; p = &(*p)->next)
		;
	*p = base;
	base->next = NULL;
}

void
abi_add_virtual(abi_class_t *cls, abi_virtual_func_t *vfunc)
{
	abi_virtual_func_t **p;

	if (cls == NULL || vfunc == NULL)
		return;

	/* Add to end of list */
	for (p = &cls->virtuals; *p != NULL; p = &(*p)->next)
		;
	*p = vfunc;
	vfunc->next = NULL;

	cls->has_vtable = 1;
	cls->is_polymorphic = 1;

	if (vfunc->is_pure)
		cls->is_abstract = 1;
}

/*
 * Function operations
 */
abi_function_t *
abi_create_function(const char *name)
{
	abi_function_t *func;

	func = calloc(1, sizeof(abi_function_t));
	if (func == NULL)
		return NULL;

	func->name = strdup(name);
	func->calling_conv = ABI_CC_C;

	return func;
}

void
abi_destroy_function(abi_function_t *func)
{
	abi_param_t *p, *pnext;

	if (func == NULL)
		return;

	free((void *)func->name);
	free((void *)func->mangled_name);

	/* Free parameters */
	for (p = func->params; p != NULL; p = pnext) {
		pnext = p->next;
		free((void *)p->name);
		free(p);
	}

	free(func);
}

void
abi_add_param(abi_function_t *func, abi_param_t *param)
{
	abi_param_t **p;

	if (func == NULL || param == NULL)
		return;

	/* Add to end of list */
	for (p = &func->params; *p != NULL; p = &(*p)->next)
		;
	*p = param;
	param->next = NULL;
}

/*
 * Vtable operations
 */
void **
abi_build_vtable(abi_context_t *ctx, abi_class_t *cls)
{
	size_t size;
	void **vtable;

	if (ctx == NULL || cls == NULL || !cls->has_vtable)
		return NULL;

	size = abi_vtable_size(ctx, cls);
	vtable = calloc(size, sizeof(void *));

	if (ctx->ops && ctx->ops->build_vtable)
		ctx->ops->build_vtable(ctx, cls, vtable);

	return vtable;
}

size_t
abi_vtable_size(abi_context_t *ctx, const abi_class_t *cls)
{
	if (ctx == NULL || cls == NULL)
		return 0;

	if (ctx->ops && ctx->ops->compute_vtable_size)
		return ctx->ops->compute_vtable_size(ctx, cls);

	/* Default: count virtual functions */
	abi_virtual_func_t *v;
	size_t count = 0;

	for (v = cls->virtuals; v != NULL; v = v->next)
		count++;

	return count;
}

void *
abi_vtable_lookup(void **vtable, size_t index)
{
	if (vtable == NULL)
		return NULL;

	return vtable[index];
}

/*
 * Compatibility checking
 */
int
abi_is_compatible(abi_kind_t abi1, abi_kind_t abi2)
{
	/* Same ABI is always compatible */
	if (abi1 == abi2)
		return 1;

	/* Itanium and ARM are mostly compatible */
	if ((abi1 == ABI_ITANIUM && abi2 == ABI_ARM) ||
	    (abi1 == ABI_ARM && abi2 == ABI_ITANIUM))
		return 1;

	/* Different ABIs are generally not compatible */
	return 0;
}

int
abi_can_interoperate(abi_kind_t abi1, abi_kind_t abi2, const abi_function_t *func)
{
	/* C functions can often interoperate */
	if (func && func->calling_conv == ABI_CC_C)
		return 1;

	/* Otherwise, ABIs must be compatible */
	return abi_is_compatible(abi1, abi2);
}

/*
 * Get ABI name as string
 */
const char *
abi_name(abi_kind_t abi)
{
	switch (abi) {
	case ABI_ITANIUM:
		return "Itanium C++ ABI";
	case ABI_MSVC:
		return "Microsoft Visual C++ ABI";
	case ABI_WATCOM:
		return "Watcom C++ ABI";
	case ABI_ARM:
		return "ARM C++ ABI";
	default:
		return "Unknown ABI";
	}
}

/*
 * Debug output
 */
void
abi_dump_class(abi_context_t *ctx, const abi_class_t *cls, FILE *out)
{
	abi_field_t *f;
	abi_base_t *b;
	abi_virtual_func_t *v;

	if (cls == NULL || out == NULL)
		return;

	fprintf(out, "class %s {\n", cls->name);
	fprintf(out, "  size: %zu bytes\n", cls->size);
	fprintf(out, "  alignment: %zu bytes\n", cls->alignment);
	fprintf(out, "  mangled: %s\n", cls->mangled_name ? cls->mangled_name : "<none>");

	if (cls->bases) {
		fprintf(out, "\n  Base classes:\n");
		for (b = cls->bases; b != NULL; b = b->next) {
			fprintf(out, "    %s at offset %zu (%s%s)\n",
			        b->base_class->name,
			        b->offset,
			        b->is_virtual ? "virtual" : "non-virtual",
			        b->is_primary ? ", primary" : "");
		}
	}

	if (cls->fields) {
		fprintf(out, "\n  Fields:\n");
		for (f = cls->fields; f != NULL; f = f->next) {
			fprintf(out, "    %s at offset %zu",
			        f->name, f->offset);
			if (f->bit_width)
				fprintf(out, " (bitfield %zu bits)", f->bit_width);
			fprintf(out, "\n");
		}
	}

	if (cls->virtuals) {
		fprintf(out, "\n  Virtual functions:\n");
		for (v = cls->virtuals; v != NULL; v = v->next) {
			fprintf(out, "    [%zu] %s%s\n",
			        v->vtable_index,
			        v->name,
			        v->is_pure ? " (pure)" : "");
		}
	}

	fprintf(out, "}\n");
}

void
abi_dump_function(abi_context_t *ctx, const abi_function_t *func, FILE *out)
{
	abi_param_t *p;

	if (func == NULL || out == NULL)
		return;

	fprintf(out, "function %s\n", func->name);
	fprintf(out, "  mangled: %s\n", func->mangled_name ? func->mangled_name : "<none>");
	fprintf(out, "  calling convention: %d\n", func->calling_conv);

	if (func->params) {
		fprintf(out, "  parameters:\n");
		for (p = func->params; p != NULL; p = p->next) {
			fprintf(out, "    %s\n", p->name);
		}
	}
}

void
abi_dump_vtable(abi_context_t *ctx, const abi_class_t *cls, FILE *out)
{
	abi_virtual_func_t *v;

	if (cls == NULL || out == NULL)
		return;

	fprintf(out, "vtable for %s {\n", cls->name);

	if (cls->virtuals) {
		for (v = cls->virtuals; v != NULL; v = v->next) {
			fprintf(out, "  [%zu] %s\n", v->vtable_index, v->name);
		}
	}

	fprintf(out, "}\n");
}
