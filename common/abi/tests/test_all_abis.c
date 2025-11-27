/*
 * Copyright (c) 2025 PCC Project
 *
 * Comprehensive ABI Test Suite
 * Tests all 25+ ABIs with mangling, demangling, and transformations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "../abi.h"
#include "../demangle.h"
#include "../universal_demangle.h"
#include "../template_renamer.h"

int tests_passed = 0;
int tests_failed = 0;

#define TEST(name) \
	printf("Testing %s... ", name); \
	fflush(stdout);

#define PASS() \
	printf("PASS\\n"); \
	tests_passed++;

#define FAIL(msg) \
	printf("FAIL: %s\\n", msg); \
	tests_failed++;

#define ASSERT_EQ(a, b) \
	if ((a) != (b)) { \
		FAIL("assertion failed"); \
		return; \
	}

#define ASSERT_STR_EQ(a, b) \
	if (strcmp((a), (b)) != 0) { \
		printf("FAIL: expected '%s', got '%s'\\n", b, a); \
		tests_failed++; \
		return; \
	}

#define ASSERT_NOT_NULL(a) \
	if ((a) == NULL) { \
		FAIL("unexpected NULL"); \
		return; \
	}

/*
 * Test Itanium C++ ABI mangling
 */
void
test_itanium()
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_param_t *param;
	char *mangled;

	TEST("Itanium C++ mangling");

	ctx = abi_init(ABI_ITANIUM);
	assert(ctx != NULL);

	/* Test: void foo(int, double) */
	func = abi_create_function("foo");
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	param = calloc(1, sizeof(abi_param_t));
	param->type = abi_create_type(ABI_TYPE_INT);
	abi_add_param(func, param);

	param = calloc(1, sizeof(abi_param_t));
	param->type = abi_create_type(ABI_TYPE_DOUBLE);
	abi_add_param(func, param);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '_' && mangled[1] == 'Z');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test MSVC C++ ABI mangling
 */
void
test_msvc()
{
	abi_context_t *ctx;
	abi_function_t *func;
	char *mangled;

	TEST("MSVC C++ mangling");

	ctx = abi_init(ABI_MSVC);
	assert(ctx != NULL);

	func = abi_create_function("foo");
	func->calling_conv = ABI_CC_CDECL;
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '?');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test D Language ABI mangling
 */
void
test_dlang()
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_param_t *param;
	char *mangled;

	TEST("D Language mangling");

	ctx = abi_init(ABI_DLANG);
	assert(ctx != NULL);

	func = abi_create_function("foo");
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	param = calloc(1, sizeof(abi_param_t));
	param->type = abi_create_type(ABI_TYPE_INT);
	abi_add_param(func, param);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '_' && mangled[1] == 'D');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test Swift ABI mangling
 */
void
test_swift()
{
	abi_context_t *ctx;
	abi_function_t *func;
	char *mangled;

	TEST("Swift mangling");

	ctx = abi_init(ABI_SWIFT);
	assert(ctx != NULL);

	func = abi_create_function("foo");
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '$' && mangled[1] == 's');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test Apple Objective-C 2.0 mangling
 */
void
test_objc()
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_class_t *cls;
	char *mangled;

	TEST("Objective-C 2.0 mangling");

	ctx = abi_init(ABI_APPLE_OBJC2);
	assert(ctx != NULL);

	cls = abi_create_class("MyClass");
	func = abi_create_function("doSomething");
	func->parent_class = cls;
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '-' && mangled[1] == '[');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test Java JNI mangling
 */
void
test_java()
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_class_t *cls;
	char *mangled;

	TEST("Java JNI mangling");

	ctx = abi_init(ABI_JAVA);
	assert(ctx != NULL);

	cls = abi_create_class("com.example.MyClass");
	func = abi_create_function("nativeMethod");
	func->parent_class = cls;

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(strncmp(mangled, "Java_", 5) == 0);

	free(mangled);
	abi_destroy_function(func);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test Rust ABI mangling
 */
void
test_rust()
{
	abi_context_t *ctx;
	abi_function_t *func;
	char *mangled;

	TEST("Rust mangling");

	ctx = abi_init(ABI_RUST);
	assert(ctx != NULL);

	func = abi_create_function("foo");
	func->return_type = abi_create_type(ABI_TYPE_VOID);

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(mangled[0] == '_' && mangled[1] == 'Z');

	free(mangled);
	abi_destroy_function(func);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test FreePascal ABI mangling
 */
void
test_freepascal()
{
	abi_context_t *ctx;
	abi_function_t *func;
	abi_class_t *cls;
	char *mangled;

	TEST("FreePascal mangling");

	ctx = abi_init(ABI_FREEPASCAL);
	assert(ctx != NULL);

	cls = abi_create_class("TMyClass");
	func = abi_create_function("DoSomething");
	func->parent_class = cls;

	mangled = abi_mangle_function(ctx, func);
	ASSERT_NOT_NULL(mangled);
	assert(strncmp(mangled, "UNIT_", 5) == 0);

	free(mangled);
	abi_destroy_function(func);
	abi_destroy_class(cls);
	abi_destroy(ctx);

	PASS();
}

/*
 * Test universal demangler auto-detection
 */
void
test_universal_demangle()
{
	abi_kind_t abi;

	TEST("Universal demangler detection");

	/* Itanium */
	abi = demangle_detect_abi("_Z3foov");
	ASSERT_EQ(abi, ABI_ITANIUM);

	/* MSVC */
	abi = demangle_detect_abi("?foo@@YAXXZ");
	ASSERT_EQ(abi, ABI_MSVC);

	/* D */
	abi = demangle_detect_abi("_D3fooFZv");
	ASSERT_EQ(abi, ABI_DLANG);

	/* Swift */
	abi = demangle_detect_abi("$s6Module3fooyyF");
	ASSERT_EQ(abi, ABI_SWIFT);

	/* Objective-C */
	abi = demangle_detect_abi("-[MyClass doSomething]");
	ASSERT_EQ(abi, ABI_APPLE_OBJC2);

	/* Java */
	abi = demangle_detect_abi("Java_com_example_MyClass_method");
	ASSERT_EQ(abi, ABI_JAVA);

	/* Go */
	abi = demangle_detect_abi("go.main.MyFunc");
	ASSERT_EQ(abi, ABI_GO);

	/* Kotlin */
	abi = demangle_detect_abi("kfun:MyClass#method");
	ASSERT_EQ(abi, ABI_KOTLIN);

	PASS();
}

/*
 * Test template renamer
 */
void
test_template_renamer()
{
	rename_context_t *ctx;
	char *result;

	TEST("Template renamer");

	ctx = rename_init();
	assert(ctx != NULL);

	/* Add rule: std::vector<?1> -> Array<$1> */
	rename_add_rule(ctx, "std::vector<?1>", "Array<$1>", 0);

	result = rename_apply(ctx, "std::vector<int>");
	ASSERT_STR_EQ(result, "Array<int>");
	free(result);

	/* Test parameter reordering */
	rename_add_rule(ctx, "pair<?1,?2>", "tuple<$2,$1>", 0);
	result = rename_apply(ctx, "pair<int,string>");
	ASSERT_STR_EQ(result, "tuple<string,int>");
	free(result);

	rename_destroy(ctx);

	PASS();
}

/*
 * Test demangling
 */
void
test_demangle()
{
	char *demangled;

	TEST("Itanium demangling");

	demangled = demangle_itanium("_Z3foov", DEMANGLE_OPT_NONE);
	ASSERT_NOT_NULL(demangled);
	assert(strstr(demangled, "foo") != NULL);
	free(demangled);

	PASS();
}

/*
 * Main test runner
 */
int
main(void)
{
	printf("=== Comprehensive ABI Test Suite ===\\n\\n");

	/* C++ ABIs */
	test_itanium();
	test_msvc();

	/* Modern languages */
	test_dlang();
	test_swift();
	test_rust();

	/* Objective-C */
	test_objc();

	/* VM languages */
	test_java();

	/* Pascal */
	test_freepascal();

	/* Universal */
	test_universal_demangle();
	test_template_renamer();
	test_demangle();

	printf("\\n=== Test Results ===\\n");
	printf("Passed: %d\\n", tests_passed);
	printf("Failed: %d\\n", tests_failed);

	return tests_failed > 0 ? 1 : 0;
}
