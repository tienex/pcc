/*
 * Example: Pascal-like language integration with generic GC
 *
 * This demonstrates how the generic GC can be used by different language
 * runtimes (Pascal, Fortran, etc.) by implementing language-specific
 * mark callbacks.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "libgc/gc.h"

/* ============================================================
 * Pascal Value Representation
 * ============================================================ */

typedef enum {
	PASCAL_INTEGER,
	PASCAL_REAL,
	PASCAL_BOOLEAN,
	PASCAL_CHAR,
	PASCAL_STRING,
	PASCAL_ARRAY,
	PASCAL_RECORD,
	PASCAL_POINTER
} pascal_type_t;

/* Pascal value header (stored before actual data) */
typedef struct pascal_value {
	pascal_type_t type;
	size_t size;  /* Size of data following this header */
	/* Actual data follows */
} pascal_value_t;

/* Pascal array structure */
typedef struct pascal_array {
	size_t length;
	pascal_value_t **elements;  /* Array of pointers to values */
} pascal_array_t;

/* Pascal record structure */
typedef struct pascal_record {
	size_t num_fields;
	pascal_value_t **fields;  /* Array of pointers to field values */
} pascal_record_t;

/* ============================================================
 * Pascal Runtime State
 * ============================================================ */

static gc_context_t *pascal_gc = NULL;

/* ============================================================
 * Pascal Mark Callback
 * ============================================================ */

static void
pascal_mark_value(gc_context_t *gc, void *obj, void *userdata)
{
	pascal_value_t *val = (pascal_value_t *)obj;
	size_t i;

	/* Mark based on Pascal type */
	switch (val->type) {
	case PASCAL_ARRAY: {
		pascal_array_t *arr = (pascal_array_t *)((char *)val + sizeof(pascal_value_t));
		/* Mark all array elements */
		for (i = 0; i < arr->length; i++) {
			if (arr->elements[i]) {
				gc_mark(gc, arr->elements[i]);
			}
		}
		break;
	}

	case PASCAL_RECORD: {
		pascal_record_t *rec = (pascal_record_t *)((char *)val + sizeof(pascal_value_t));
		/* Mark all record fields */
		for (i = 0; i < rec->num_fields; i++) {
			if (rec->fields[i]) {
				gc_mark(gc, rec->fields[i]);
			}
		}
		break;
	}

	case PASCAL_POINTER: {
		/* Dereference and mark pointed-to value */
		pascal_value_t **ptr = (pascal_value_t **)((char *)val + sizeof(pascal_value_t));
		if (*ptr) {
			gc_mark(gc, *ptr);
		}
		break;
	}

	/* Primitive types have no references to mark */
	case PASCAL_INTEGER:
	case PASCAL_REAL:
	case PASCAL_BOOLEAN:
	case PASCAL_CHAR:
	case PASCAL_STRING:
	default:
		break;
	}
}

/* ============================================================
 * Pascal Runtime Functions
 * ============================================================ */

void
pascal_runtime_init(void)
{
	gc_config_t config = GC_DEFAULT_CONFIG;

	config.heap_size = 16 * 1024 * 1024;  /* 16MB */
	config.gc_threshold = 8 * 1024 * 1024;
	config.enable_pools = 1;
	config.verbose = 0;

	pascal_gc = gc_init(&config);
	if (!pascal_gc) {
		fprintf(stderr, "Failed to initialize Pascal GC\n");
		exit(1);
	}

	/* Set Pascal-specific mark callback */
	gc_set_mark_callback(pascal_gc, pascal_mark_value, NULL);
}

void
pascal_runtime_destroy(void)
{
	gc_destroy(pascal_gc);
}

/* Allocate Pascal integer */
pascal_value_t *
pascal_new_integer(int value)
{
	pascal_value_t *val;
	int *data;

	val = gc_alloc(pascal_gc, sizeof(pascal_value_t) + sizeof(int));
	val->type = PASCAL_INTEGER;
	val->size = sizeof(int);

	data = (int *)((char *)val + sizeof(pascal_value_t));
	*data = value;

	return val;
}

/* Allocate Pascal string */
pascal_value_t *
pascal_new_string(const char *str)
{
	pascal_value_t *val;
	char *data;
	size_t len = strlen(str) + 1;

	val = gc_alloc(pascal_gc, sizeof(pascal_value_t) + len);
	val->type = PASCAL_STRING;
	val->size = len;

	data = (char *)((char *)val + sizeof(pascal_value_t));
	strcpy(data, str);

	return val;
}

/* Allocate Pascal array */
pascal_value_t *
pascal_new_array(size_t length)
{
	pascal_value_t *val;
	pascal_array_t *arr;

	val = gc_alloc(pascal_gc, sizeof(pascal_value_t) + sizeof(pascal_array_t) +
	               length * sizeof(pascal_value_t *));
	val->type = PASCAL_ARRAY;
	val->size = sizeof(pascal_array_t) + length * sizeof(pascal_value_t *);

	arr = (pascal_array_t *)((char *)val + sizeof(pascal_value_t));
	arr->length = length;
	arr->elements = (pascal_value_t **)((char *)arr + sizeof(pascal_array_t));

	/* Initialize to NULL */
	size_t i;
	for (i = 0; i < length; i++) {
		arr->elements[i] = NULL;
	}

	return val;
}

/* Allocate Pascal record */
pascal_value_t *
pascal_new_record(size_t num_fields)
{
	pascal_value_t *val;
	pascal_record_t *rec;

	val = gc_alloc(pascal_gc, sizeof(pascal_value_t) + sizeof(pascal_record_t) +
	               num_fields * sizeof(pascal_value_t *));
	val->type = PASCAL_RECORD;
	val->size = sizeof(pascal_record_t) + num_fields * sizeof(pascal_value_t *);

	rec = (pascal_record_t *)((char *)val + sizeof(pascal_value_t));
	rec->num_fields = num_fields;
	rec->fields = (pascal_value_t **)((char *)rec + sizeof(pascal_record_t));

	/* Initialize to NULL */
	size_t i;
	for (i = 0; i < num_fields; i++) {
		rec->fields[i] = NULL;
	}

	return val;
}

/* Get integer value */
int
pascal_get_integer(pascal_value_t *val)
{
	assert(val->type == PASCAL_INTEGER);
	return *(int *)((char *)val + sizeof(pascal_value_t));
}

/* Get string value */
const char *
pascal_get_string(pascal_value_t *val)
{
	assert(val->type == PASCAL_STRING);
	return (const char *)((char *)val + sizeof(pascal_value_t));
}

/* Set array element */
void
pascal_array_set(pascal_value_t *arr_val, size_t index, pascal_value_t *elem)
{
	pascal_array_t *arr;

	assert(arr_val->type == PASCAL_ARRAY);
	arr = (pascal_array_t *)((char *)arr_val + sizeof(pascal_value_t));
	assert(index < arr->length);

	arr->elements[index] = elem;
}

/* Get array element */
pascal_value_t *
pascal_array_get(pascal_value_t *arr_val, size_t index)
{
	pascal_array_t *arr;

	assert(arr_val->type == PASCAL_ARRAY);
	arr = (pascal_array_t *)((char *)arr_val + sizeof(pascal_value_t));
	assert(index < arr->length);

	return arr->elements[index];
}

/* Set record field */
void
pascal_record_set(pascal_value_t *rec_val, size_t field, pascal_value_t *val)
{
	pascal_record_t *rec;

	assert(rec_val->type == PASCAL_RECORD);
	rec = (pascal_record_t *)((char *)rec_val + sizeof(pascal_value_t));
	assert(field < rec->num_fields);

	rec->fields[field] = val;
}

/* Get record field */
pascal_value_t *
pascal_record_get(pascal_value_t *rec_val, size_t field)
{
	pascal_record_t *rec;

	assert(rec_val->type == PASCAL_RECORD);
	rec = (pascal_record_t *)((char *)rec_val + sizeof(pascal_value_t));
	assert(field < rec->num_fields);

	return rec->fields[field];
}

/* Register root */
void
pascal_register_root(pascal_value_t **root)
{
	gc_register_root(pascal_gc, (void **)root);
}

/* Unregister root */
void
pascal_unregister_root(pascal_value_t **root)
{
	gc_unregister_root(pascal_gc, (void **)root);
}

/* Run garbage collection */
void
pascal_collect(void)
{
	gc_collect(pascal_gc);
}

/* ============================================================
 * Test Program
 * ============================================================ */

int
main(void)
{
	pascal_value_t *int_val, *str_val, *arr_val, *rec_val;
	pascal_value_t *temp;

	printf("========================================\n");
	printf("Pascal-GC Integration Example\n");
	printf("========================================\n");

	/* Initialize Pascal runtime */
	printf("\n1. Initializing Pascal runtime...\n");
	pascal_runtime_init();
	printf("✓ Runtime initialized with generic GC\n");

	/* Test 1: Create Pascal integer */
	printf("\n2. Creating Pascal integer...\n");
	int_val = pascal_new_integer(42);
	pascal_register_root(&int_val);
	printf("✓ Created integer: %d\n", pascal_get_integer(int_val));

	/* Test 2: Create Pascal string */
	printf("\n3. Creating Pascal string...\n");
	str_val = pascal_new_string("Hello, Pascal!");
	pascal_register_root(&str_val);
	printf("✓ Created string: '%s'\n", pascal_get_string(str_val));

	/* Test 3: Create Pascal array */
	printf("\n4. Creating Pascal array...\n");
	arr_val = pascal_new_array(5);
	pascal_register_root(&arr_val);

	/* Fill array with integers */
	int i;
	for (i = 0; i < 5; i++) {
		temp = pascal_new_integer(i * 10);
		pascal_array_set(arr_val, i, temp);
	}

	printf("✓ Created array of 5 integers: ");
	for (i = 0; i < 5; i++) {
		temp = pascal_array_get(arr_val, i);
		printf("%d ", pascal_get_integer(temp));
	}
	printf("\n");

	/* Test 4: Create Pascal record */
	printf("\n5. Creating Pascal record...\n");
	rec_val = pascal_new_record(3);
	pascal_register_root(&rec_val);

	/* Field 0: name (string) */
	temp = pascal_new_string("John Doe");
	pascal_record_set(rec_val, 0, temp);

	/* Field 1: age (integer) */
	temp = pascal_new_integer(30);
	pascal_record_set(rec_val, 1, temp);

	/* Field 2: score (integer) */
	temp = pascal_new_integer(95);
	pascal_record_set(rec_val, 2, temp);

	printf("✓ Created record with 3 fields:\n");
	printf("    name: '%s'\n", pascal_get_string(pascal_record_get(rec_val, 0)));
	printf("    age:  %d\n", pascal_get_integer(pascal_record_get(rec_val, 1)));
	printf("    score: %d\n", pascal_get_integer(pascal_record_get(rec_val, 2)));

	/* Test 5: Garbage collection */
	printf("\n6. Testing garbage collection...\n");

	/* Create some garbage */
	for (i = 0; i < 100; i++) {
		pascal_new_integer(i);
		pascal_new_string("temp");
	}

	printf("✓ Created 200 temporary objects\n");

	size_t freed = gc_collect(pascal_gc);
	printf("✓ Garbage collection freed %zu objects\n", freed);

	/* Test 6: Nested structures */
	printf("\n7. Testing nested structures...\n");

	/* Create array of records */
	pascal_value_t *arr_of_records = pascal_new_array(3);
	pascal_register_root(&arr_of_records);

	for (i = 0; i < 3; i++) {
		pascal_value_t *rec = pascal_new_record(2);
		pascal_record_set(rec, 0, pascal_new_string("Item"));
		pascal_record_set(rec, 1, pascal_new_integer(i));
		pascal_array_set(arr_of_records, i, rec);
	}

	printf("✓ Created array of 3 records\n");
	printf("  Array contents:\n");
	for (i = 0; i < 3; i++) {
		pascal_value_t *rec = pascal_array_get(arr_of_records, i);
		const char *name = pascal_get_string(pascal_record_get(rec, 0));
		int value = pascal_get_integer(pascal_record_get(rec, 1));
		printf("    [%d]: {name='%s', value=%d}\n", i, name, value);
	}

	/* Test 7: Verify all roots survived */
	printf("\n8. Collecting with all roots...\n");
	pascal_collect();
	printf("✓ All rooted objects survived\n");

	/* Verify data integrity */
	assert(pascal_get_integer(int_val) == 42);
	assert(strcmp(pascal_get_string(str_val), "Hello, Pascal!") == 0);
	printf("✓ Data integrity verified\n");

	/* Cleanup */
	printf("\n9. Cleaning up...\n");
	pascal_unregister_root(&int_val);
	pascal_unregister_root(&str_val);
	pascal_unregister_root(&arr_val);
	pascal_unregister_root(&rec_val);
	pascal_unregister_root(&arr_of_records);

	/* Print statistics */
	printf("\n========================================\n");
	printf("Statistics:\n");
	printf("========================================\n");
	gc_print_stats(pascal_gc);

	/* Destroy runtime */
	pascal_runtime_destroy();

	printf("\n========================================\n");
	printf("✅ INTEGRATION TEST PASSED!\n");
	printf("========================================\n");
	printf("\nThis demonstrates how ANY language can use\n");
	printf("the generic GC by implementing a mark callback.\n");
	printf("========================================\n");

	return 0;
}
