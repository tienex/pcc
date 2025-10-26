/*
 * Test program for Universal Debug Symbol System
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Test counters */
static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) \
	do { \
		printf("TEST: %s\n", name); \
		tests_run++; \
	} while (0)

#define ASSERT(cond) \
	do { \
		if (!(cond)) { \
			printf("  FAIL: %s:%d: assertion failed: %s\n", \
			    __FILE__, __LINE__, #cond); \
			tests_failed++; \
			return; \
		} \
	} while (0)

#define PASS() \
	do { \
		printf("  PASS\n"); \
		tests_passed++; \
	} while (0)

/*
 * Test basic initialization and cleanup
 */
static void
test_init_finish(void)
{
	TEST("init_finish");

	debugsym_init(DBGFMT_DWARF3);
	ASSERT(dbg_ctx != NULL);
	ASSERT(dbg_ctx->format == DBGFMT_DWARF3);

	debugsym_finish();
	ASSERT(dbg_ctx == NULL);

	PASS();
}

/*
 * Test format name retrieval
 */
static void
test_format_names(void)
{
	const char *name;

	TEST("format_names");

	name = debugsym_format_name(DBGFMT_DWARF3);
	ASSERT(name != NULL);
	ASSERT(strcmp(name, "DWARF3") == 0);

	name = debugsym_format_name(DBGFMT_CV8);
	ASSERT(name != NULL);
	ASSERT(strcmp(name, "CodeView8") == 0);

	name = debugsym_format_name(DBGFMT_STABS);
	ASSERT(name != NULL);
	ASSERT(strcmp(name, "STABS") == 0);

	PASS();
}

/*
 * Test primitive type creation
 */
static void
test_primitive_types(void)
{
	debug_type_t *type;

	TEST("primitive_types");

	debugsym_type_init();

	type = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	ASSERT(type != NULL);
	ASSERT(type->encoding == DBGTYPE_INT32);
	ASSERT(type->size == 4);

	type = debugsym_get_primitive_type(DBGTYPE_CHAR, 1);
	ASSERT(type != NULL);
	ASSERT(type->encoding == DBGTYPE_CHAR);
	ASSERT(type->size == 1);

	PASS();
}

/*
 * Test pointer type creation
 */
static void
test_pointer_types(void)
{
	debug_type_t *base, *ptr;

	TEST("pointer_types");

	debugsym_type_init();

	base = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	ASSERT(base != NULL);

	ptr = debugsym_pointer_type(base);
	ASSERT(ptr != NULL);
	ASSERT(ptr->encoding == DBGTYPE_POINTER);
	ASSERT(ptr->base_type == base);
	ASSERT(ptr->size == sizeof(void *));

	PASS();
}

/*
 * Test array type creation
 */
static void
test_array_types(void)
{
	debug_type_t *base, *array;
	int dims[3] = {10, 20, 30};

	TEST("array_types");

	debugsym_type_init();

	base = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	ASSERT(base != NULL);

	array = debugsym_array_type(base, dims, 3);
	ASSERT(array != NULL);
	ASSERT(array->encoding == DBGTYPE_ARRAY);
	ASSERT(array->base_type == base);
	ASSERT(array->array_dimensions == 3);
	ASSERT(array->array_bounds != NULL);
	ASSERT(array->array_bounds[0] == 10);
	ASSERT(array->array_bounds[1] == 20);
	ASSERT(array->array_bounds[2] == 30);

	PASS();
}

/*
 * Test type qualifiers
 */
static void
test_type_qualifiers(void)
{
	debug_type_t *base, *const_type, *volatile_type;

	TEST("type_qualifiers");

	debugsym_type_init();

	base = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	ASSERT(base != NULL);
	ASSERT(!base->is_const);
	ASSERT(!base->is_volatile);

	const_type = debugsym_const_type(base);
	ASSERT(const_type != NULL);
	ASSERT(const_type->is_const);
	ASSERT(!const_type->is_volatile);

	volatile_type = debugsym_volatile_type(base);
	ASSERT(volatile_type != NULL);
	ASSERT(!volatile_type->is_const);
	ASSERT(volatile_type->is_volatile);

	PASS();
}

/*
 * Test symbol creation
 */
static void
test_symbol_creation(void)
{
	debug_symbol_t *sym;
	debug_type_t *type;

	TEST("symbol_creation");

	debugsym_init(DBGFMT_DWARF3);
	debugsym_type_init();

	sym = debugsym_new_symbol();
	ASSERT(sym != NULL);

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup("test_var");
	ASSERT(sym->name != NULL);
	ASSERT(strcmp(sym->name, "test_var") == 0);

	type = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	sym->type = type;
	ASSERT(sym->type != NULL);

	sym->location.filename = debugsym_strdup("test.c");
	sym->location.line = 42;

	debugsym_record_symbol(sym);
	ASSERT(dbg_ctx->symbols != NULL);

	debugsym_finish();

	PASS();
}

/*
 * Test enum type creation
 */
static void
test_enum_types(void)
{
	debug_type_t *enum_type;
	debug_enum_value_t values[3];

	TEST("enum_types");

	debugsym_type_init();

	values[0].name = "RED";
	values[0].value = 0;
	values[1].name = "GREEN";
	values[1].value = 1;
	values[2].name = "BLUE";
	values[2].value = 2;

	enum_type = debugsym_enum_type("Color", values, 3);
	ASSERT(enum_type != NULL);
	ASSERT(enum_type->encoding == DBGTYPE_ENUM);
	ASSERT(enum_type->enum_value_count == 3);
	ASSERT(enum_type->enum_values != NULL);
	ASSERT(strcmp(enum_type->enum_values[0].name, "RED") == 0);
	ASSERT(enum_type->enum_values[0].value == 0);

	PASS();
}

/*
 * Test struct type creation
 */
static void
test_struct_types(void)
{
	debug_type_t *struct_type;
	debug_member_t members[2];
	debug_type_t *int_type, *float_type;

	TEST("struct_types");

	debugsym_type_init();

	int_type = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	float_type = debugsym_get_primitive_type(DBGTYPE_FLOAT32, 4);

	members[0].name = "x";
	members[0].type = int_type;
	members[0].offset = 0;
	members[0].bit_size = 0;
	members[0].bit_offset = 0;

	members[1].name = "y";
	members[1].type = float_type;
	members[1].offset = 4;
	members[1].bit_size = 0;
	members[1].bit_offset = 0;

	struct_type = debugsym_composite_type(DBGTYPE_STRUCT, "Point",
	    members, 2, 8);
	ASSERT(struct_type != NULL);
	ASSERT(struct_type->encoding == DBGTYPE_STRUCT);
	ASSERT(struct_type->member_count == 2);
	ASSERT(struct_type->members != NULL);
	ASSERT(strcmp(struct_type->members[0].name, "x") == 0);
	ASSERT(struct_type->members[0].type == int_type);

	PASS();
}

/*
 * Test function type creation
 */
static void
test_function_types(void)
{
	debug_type_t *func_type, *return_type;
	debug_type_t *param_types[2];

	TEST("function_types");

	debugsym_type_init();

	return_type = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	param_types[0] = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	param_types[1] = debugsym_get_primitive_type(DBGTYPE_FLOAT32, 4);

	func_type = debugsym_function_type(return_type, param_types, 2);
	ASSERT(func_type != NULL);
	ASSERT(func_type->encoding == DBGTYPE_FUNCTION);
	ASSERT(func_type->base_type == return_type);
	ASSERT(func_type->param_count == 2);
	ASSERT(func_type->param_types != NULL);
	ASSERT(func_type->param_types[0] == param_types[0]);

	PASS();
}

/*
 * Test format detection
 */
static void
test_format_detection(void)
{
	unsigned char elf_header[4] = {0x7f, 'E', 'L', 'F'};
	unsigned char pe_header[2] = {'M', 'Z'};
	debug_format_t format;

	TEST("format_detection");

	format = debugsym_detect_format(elf_header, 4);
	ASSERT(format == DBGFMT_DWARF3);

	format = debugsym_detect_format(pe_header, 2);
	ASSERT(format == DBGFMT_PECOFF);

	PASS();
}

/*
 * Test format conversion capabilities
 */
static void
test_format_conversion(void)
{
	int can_convert;

	TEST("format_conversion");

	/* Same format should always work */
	can_convert = debugsym_can_convert(DBGFMT_DWARF3, DBGFMT_DWARF3);
	ASSERT(can_convert);

	/* DWARF versions should convert between each other */
	can_convert = debugsym_can_convert(DBGFMT_DWARF3, DBGFMT_DWARF4);
	ASSERT(can_convert);

	/* CodeView versions should convert between each other */
	can_convert = debugsym_can_convert(DBGFMT_CV5, DBGFMT_CV8);
	ASSERT(can_convert);

	/* STABS and DBX should convert */
	can_convert = debugsym_can_convert(DBGFMT_STABS, DBGFMT_DBX);
	ASSERT(can_convert);

	PASS();
}

/*
 * Test type size calculations
 */
static void
test_type_sizes(void)
{
	debug_type_t *type, *array_type;
	unsigned int size;
	int dims[2] = {10, 20};

	TEST("type_sizes");

	debugsym_type_init();

	type = debugsym_get_primitive_type(DBGTYPE_INT32, 4);
	size = debugsym_type_size(type);
	ASSERT(size == 4);

	array_type = debugsym_array_type(type, dims, 2);
	size = debugsym_type_size(array_type);
	ASSERT(size == 4 * 10 * 20);

	PASS();
}

/*
 * Main test runner
 */
int
main(int argc, char *argv[])
{
	printf("Universal Debug Symbol Test Suite\n");
	printf("==================================\n\n");

	/* Run all tests */
	test_init_finish();
	test_format_names();
	test_primitive_types();
	test_pointer_types();
	test_array_types();
	test_type_qualifiers();
	test_symbol_creation();
	test_enum_types();
	test_struct_types();
	test_function_types();
	test_format_detection();
	test_format_conversion();
	test_type_sizes();

	/* Print summary */
	printf("\n==================================\n");
	printf("Tests run:    %d\n", tests_run);
	printf("Tests passed: %d\n", tests_passed);
	printf("Tests failed: %d\n", tests_failed);

	if (tests_failed == 0) {
		printf("\nALL TESTS PASSED!\n");
		return 0;
	} else {
		printf("\nSOME TESTS FAILED!\n");
		return 1;
	}
}
