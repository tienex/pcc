/*
 * Copyright (c) 2025 PCC Go Runtime Library
 *
 * Panic/recover mechanism and defer support
 */

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "runtime.h"

/* Defer entry */
typedef struct defer_entry {
	go_defer_func fn;
	void *arg;
	struct defer_entry *next;
} defer_entry;

/* Panic context (per goroutine) */
typedef struct panic_context {
	jmp_buf env;
	go_panic_info info;
	bool active;
	bool recovered;
	defer_entry *defers;
} panic_context;

/* Thread-local panic context */
static __thread panic_context panic_ctx = {
	.active = false,
	.recovered = false,
	.defers = NULL
};

/*
 * Add deferred function
 */
void
go_defer(go_defer_func fn, void *arg)
{
	defer_entry *entry;

	entry = (defer_entry *)go_malloc(sizeof(defer_entry));
	entry->fn = fn;
	entry->arg = arg;
	entry->next = panic_ctx.defers;

	panic_ctx.defers = entry;
}

/*
 * Run deferred functions (LIFO order)
 */
void
go_defer_run(void)
{
	defer_entry *entry, *next;

	for (entry = panic_ctx.defers; entry != NULL; entry = next) {
		next = entry->next;

		/* Call deferred function */
		if (entry->fn != NULL)
			entry->fn(entry->arg);

		go_free(entry);
	}

	panic_ctx.defers = NULL;
}

/*
 * Panic - unwind stack and run deferred functions
 */
void
go_panic(go_interface value)
{
	/* Store panic information */
	panic_ctx.info.value = value;
	panic_ctx.info.pc = NULL;
	panic_ctx.info.file = "<unknown>";
	panic_ctx.info.line = 0;
	panic_ctx.active = true;
	panic_ctx.recovered = false;

	/* Run deferred functions */
	go_defer_run();

	/* If not recovered, print panic message and exit */
	if (!panic_ctx.recovered) {
		fprintf(stderr, "panic: ");

		/* Try to print the panic value */
		if (value.type == &go_type_string) {
			go_string *s = (go_string *)value.data;
			fprintf(stderr, "%.*s\n", (int)s->len, s->data);
		} else if (value.type == &go_type_int) {
			fprintf(stderr, "%lld\n", (long long)*(go_int *)value.data);
		} else if (value.type != NULL) {
			fprintf(stderr, "<%s value>\n", ((go_type *)value.type)->name);
		} else {
			fprintf(stderr, "<nil>\n");
		}

		go_runtime_exit(2);
	}

	panic_ctx.active = false;
}

/*
 * Recover from panic
 */
go_interface
go_recover(void)
{
	go_interface result;

	if (panic_ctx.active && !panic_ctx.recovered) {
		/* Mark as recovered */
		panic_ctx.recovered = true;
		result = panic_ctx.info.value;
	} else {
		/* No active panic */
		result.type = NULL;
		result.data = NULL;
	}

	return result;
}

/*
 * Check if there's an active panic
 */
bool
go_has_panic(void)
{
	return panic_ctx.active;
}
