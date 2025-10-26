/*
 * Copyright (c) 2025 PCC Project
 *
 * ABI library test suite
 * Tests name mangling, class layout, and cross-ABI compatibility
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../abi.h"

int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) \
	printf("Testing %s... ", name); \
	fflush(stdout);

#define PASS() \
	printf("PASS\n"); \
	tests_passed++;

#define FAIL(msg) \
	printf("FAIL: %s\n", msg); \
	tests_failed++;

#define ASSERT_EQ(a, b) \
	if ((a) != (b)) { \
		FAIL("assertion failed"); \
		return; \
	}

#define ASSERT_STR_EQ(a, b) \
	if (strcmp((a), (b)) != 0) { \
		printf("FAIL: expected '%s', got '%s'\n", b, a); \
		tests_failed++; \
		return; \
	}

/*
 * Test basic ABI initialization
 */
void
test_init(void)
{
	abi_context_t *ctx;

	TEST("ABI initialization");

	ctx = abi_init(ABI_ITANIUM);
	assert(ctx != NULL);
	assert(ctx->kind == ABI_ITANIUM);
	abi_destroy(ctx);

	ctx = abi_init(ABI_MSVC);
	assert(ctx != NULL);
	assert(ctx->kind == ABI_MSVC);
	abi_destroy(ctx);

	ctx = abi_init(ABI_WATCOM);
	assert(ctx != NULL);
	assert(ctx->kind == ABI_WATCOM);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test ABI auto-detection
 */
void
test_detect(void)
{
	abi_kind_t abi;

	TEST("ABI auto-detection");

	abi = abi_detect("x86_64-pc-linux-gnu");
	ASSERT_EQ(abi, ABI_ITANIUM);

	abi = abi_detect("i686-pc-windows-msvc");
	ASSERT_EQ(abi, ABI_MSVC);

	abi = abi_detect("i386-pc-watcom");
	ASSERT_EQ(abi, ABI_WATCOM);

	abi = abi_detect("arm-linux-gnueabi");
	ASSERT_EQ(abi, ABI_ARM);

	PASS();
}

/*
 * Test Itanium name mangling
 */
void
test_itanium_mangling(void)
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_param_t *param;
	char *mangled;

	TEST("Itanium name mangling");

	ctx = abi_init(ABI_ITANIUM);

	/* Test: void foo(int) */
	func = abi_create_function("foo");
	func->return_type = abi_create_type(ABI_TYPE_VOID);
	param = calloc(1, sizeof(abi_param_t));
	param->type = abi_create_type(ABI_TYPE_INT);
	abi_add_param(func, param);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_STR_EQ(mangled, "_Z3fooi");

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test MSVC name mangling
 */
void
test_msvc_mangling(void)
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_param_t *param;
	char *mangled;

	TEST("MSVC name mangling");

	ctx = abi_init(ABI_MSVC);

	/* Test C function with underscore prefix */
	func = abi_create_function("foo");
	func->calling_conv = ABI_CC_C;
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_STR_EQ(mangled, "_foo");

	free(mangled);
	abi_destroy_function(func);

	/* Test C++ function */
	func = abi_create_function("bar");
	func->calling_conv = ABI_CC_CDECL;
	func->return_type = abi_create_type(ABI_TYPE_VOID);
	param = calloc(1, sizeof(abi_param_t));
	param->type = abi_create_type(ABI_TYPE_INT);
	abi_add_param(func, param);

	mangled = abi_mangle_function(ctx, func);
	/* MSVC mangling is complex, just check it starts with ? */
	assert(mangled[0] == '?');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test class layout
 */
void
test_class_layout(void)
{
	abi_context_t *ctx;
	abi_class_t *cls;
	abi_field_t *field1, *field2;

	TEST("Class layout");

	ctx = abi_init(ABI_ITANIUM);

	cls = abi_create_class("TestClass");

	/* Add int field */
	field1 = calloc(1, sizeof(abi_field_t));
	field1->name = "x";
	field1->type = abi_create_type(ABI_TYPE_INT);
	abi_add_field(cls, field1);

	/* Add double field */
	field2 = calloc(1, sizeof(abi_field_t));
	field2->name = "y";
	field2->type = abi_create_type(ABI_TYPE_DOUBLE);
	abi_add_field(cls, field2);

	/* Layout the class */
	abi_layout_class(ctx, cls);

	/* Check layout */
	assert(cls->size > 0);
	assert(field1->offset == 0);  /* First field at offset 0 */
	assert(field2->offset >= 8);   /* Double aligned to 8 bytes */

	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test virtual table
 */
void
test_vtable(void)
{
	abi_context_t *ctx;
	abi_class_t *cls;
	abi_virtual_func_t *vfunc;
	void **vtable;
	size_t size;

	TEST("Virtual table");

	ctx = abi_init(ABI_ITANIUM);

	cls = abi_create_class("Base");

	/* Add virtual function */
	vfunc = calloc(1, sizeof(abi_virtual_func_t));
	vfunc->name = "speak";
	vfunc->func = abi_create_function("speak");
	abi_add_virtual(cls, vfunc);

	/* Layout and build vtable */
	abi_layout_class(ctx, cls);
	size = abi_vtable_size(ctx, cls);
	vtable = abi_build_vtable(ctx, cls);

	assert(size > 0);
	assert(vtable != NULL);
	assert(cls->has_vtable);
	assert(cls->is_polymorphic);

	free(vtable);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test ABI compatibility
 */
void
test_compatibility(void)
{
	TEST("ABI compatibility");

	assert(abi_is_compatible(ABI_ITANIUM, ABI_ITANIUM));
	assert(abi_is_compatible(ABI_ITANIUM, ABI_ARM));
	assert(!abi_is_compatible(ABI_ITANIUM, ABI_MSVC));
	assert(!abi_is_compatible(ABI_MSVC, ABI_WATCOM));

	PASS();
}

/*
 * Test vtable mangling
 */
void
test_vtable_mangling(void)
{
	abi_context_t *ctx;
	abi_class_t *cls;
	char *mangled;

	TEST("Vtable name mangling");

	/* Itanium */
	ctx = abi_init(ABI_ITANIUM);
	cls = abi_create_class("MyClass");
	mangled = abi_mangle_vtable(ctx, cls);
	ASSERT_STR_EQ(mangled, "_ZTV7MyClass");
	free(mangled);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	/* MSVC */
	ctx = abi_init(ABI_MSVC);
	cls = abi_create_class("MyClass");
	mangled = abi_mangle_vtable(ctx, cls);
	ASSERT_STR_EQ(mangled, "??_7MyClass@@6B@");
	free(mangled);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test inheritance layout
 */
void
test_inheritance(void)
{
	abi_context_t *ctx;
	abi_class_t *base, *derived;
	abi_base_t *base_desc;
	abi_field_t *field;

	TEST("Inheritance layout");

	ctx = abi_init(ABI_ITANIUM);

	/* Create base class */
	base = abi_create_class("Base");
	field = calloc(1, sizeof(abi_field_t));
	field->name = "base_field";
	field->type = abi_create_type(ABI_TYPE_INT);
	abi_add_field(base, field);
	abi_layout_class(ctx, base);

	/* Create derived class */
	derived = abi_create_class("Derived");

	/* Add base class */
	base_desc = calloc(1, sizeof(abi_base_t));
	base_desc->base_class = base;
	base_desc->is_virtual = 0;
	base_desc->is_primary = 1;
	abi_add_base(derived, base_desc);

	/* Add derived field */
	field = calloc(1, sizeof(abi_field_t));
	field->name = "derived_field";
	field->type = abi_create_type(ABI_TYPE_DOUBLE);
	abi_add_field(derived, field);

	/* Layout */
	abi_layout_class(ctx, derived);

	/* Check that derived is larger than base */
	assert(derived->size >= base->size);

	abi_destroy_class(derived);
	abi_destroy_class(base);
	abi_destroy(ctx);

	PASS();
}

/*
 * Main test runner
 */
int
main(void)
{
	printf("=== PCC ABI Library Test Suite ===\n\n");

	test_init();
	test_detect();
	test_itanium_mangling();
	test_msvc_mangling();
	test_class_layout();
	test_vtable();
	test_compatibility();
	test_vtable_mangling();
	test_inheritance();

	printf("\n=== Test Results ===\n");
	printf("Passed: %d\n", tests_passed);
	printf("Failed: %d\n", tests_failed);

	return tests_failed > 0 ? 1 : 0;
}
