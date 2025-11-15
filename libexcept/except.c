/*
 * Copyright (c) 2025 PCC Project
 * Exception Handling Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "except.h"

#define MAX_BACKTRACE 64

/* Exception state (thread-local) */
static __thread except_context_t *current_context = NULL;
static __thread except_info_t current_exception;
static __thread int exception_active = 0;

/* Global handlers */
static except_handler_t global_handler = NULL;
static void (*terminate_handler)(void) = NULL;
static void (*unexpected_handler)(void) = NULL;

/* Cleanup handlers */
typedef struct cleanup_entry {
	void (*cleanup)(void *);
	void *arg;
	struct cleanup_entry *next;
} cleanup_entry_t;

static __thread cleanup_entry_t *cleanup_stack = NULL;

/*
 * Initialization
 */
int except_init(void) {
	current_context = NULL;
	exception_active = 0;
	memset(&current_exception, 0, sizeof(current_exception));
	return 0;
}

void except_shutdown(void) {
	/* Free any remaining cleanup handlers */
	while (cleanup_stack) {
		cleanup_entry_t *next = cleanup_stack->next;
		free(cleanup_stack);
		cleanup_stack = next;
	}

	/* Free backtrace if allocated */
	if (current_exception.backtrace) {
		free(current_exception.backtrace);
		current_exception.backtrace = NULL;
	}
}

/*
 * Exception Raising
 */
void except_raise(const char *name, const char *message) {
	except_info_t info = {0};
	info.name = name;
	info.message = message;
	info.type = EXCEPT_TYPE_C_LONGJMP;
	info.severity = EXCEPT_ERROR;

	except_raise_full(&info);
}

void except_raise_typed(except_type_t type, const char *name, const char *message) {
	except_info_t info = {0};
	info.name = name;
	info.message = message;
	info.type = type;
	info.severity = EXCEPT_ERROR;

	except_raise_full(&info);
}

void except_raise_full(const except_info_t *info) {
	if (!info) {
		return;
	}

	/* Copy exception info */
	current_exception = *info;
	exception_active = 1;

	/* Capture backtrace (simplified - would use backtrace() on Unix) */
	current_exception.backtrace = NULL;
	current_exception.backtrace_size = 0;

	/* Run cleanup handlers */
	while (cleanup_stack) {
		cleanup_entry_t *entry = cleanup_stack;
		cleanup_stack = entry->next;

		if (entry->cleanup) {
			entry->cleanup(entry->arg);
		}
		free(entry);
	}

	/* If we have a context, longjmp to it */
	if (current_context) {
		current_context->info = current_exception;
		current_context->caught = 1;
		longjmp(current_context->env, 1);
	}

	/* No handler - call global handler or terminate */
	if (global_handler) {
		global_handler(&current_exception);
	} else {
		fprintf(stderr, "Unhandled exception: %s\n", info->name ? info->name : "unknown");
		if (info->message) {
			fprintf(stderr, "Message: %s\n", info->message);
		}
		if (terminate_handler) {
			terminate_handler();
		}
		abort();
	}
}

void except_reraise(void) {
	if (exception_active) {
		except_raise_full(&current_exception);
	}
}

/*
 * Exception Context Management
 */
except_context_t *except_push(void) {
	except_context_t *ctx = (except_context_t *)malloc(sizeof(except_context_t));
	if (!ctx) {
		return NULL;
	}

	memset(ctx, 0, sizeof(except_context_t));
	ctx->prev = current_context;
	ctx->caught = 0;
	current_context = ctx;

	return ctx;
}

void except_pop(void) {
	if (!current_context) {
		return;
	}

	except_context_t *ctx = current_context;
	current_context = ctx->prev;

	/* Don't free - caller might still need it */
	/* User should manually free after EXCEPT_END */
}

const except_info_t *except_current(void) {
	return exception_active ? &current_exception : NULL;
}

int except_is_caught(const char *name) {
	if (!exception_active || !current_exception.name || !name) {
		return 0;
	}

	/* Simple string comparison - could be more sophisticated */
	return strcmp(current_exception.name, name) == 0;
}

/*
 * C++ Exception Support
 */
void *except_cxx_allocate(size_t size) {
	return malloc(size);
}

void except_cxx_free(void *obj) {
	free(obj);
}

void except_cxx_throw(void *obj, void *type_info, void (*destructor)(void *)) {
	except_info_t info = {0};
	info.name = "C++ exception";
	info.type = EXCEPT_TYPE_CXX;
	info.exception_object = obj;
	info.type_info = type_info;

	/* Store destructor for cleanup */
	if (destructor && obj) {
		except_register_cleanup(destructor, obj);
	}

	except_raise_full(&info);
}

void *except_cxx_begin_catch(void *exception_ptr) {
	return current_exception.exception_object;
}

void except_cxx_end_catch(void) {
	exception_active = 0;
}

void *except_cxx_current_exception_type(void) {
	return exception_active ? current_exception.type_info : NULL;
}

/*
 * Ada Exception Support
 */
void except_ada_raise(const char *exception_name, const char *message) {
	except_raise_typed(EXCEPT_TYPE_ADA, exception_name, message);
}

void *except_ada_current_occurrence(void) {
	return exception_active ? (void *)&current_exception : NULL;
}

/*
 * Stack Unwinding
 */
void except_force_unwind(void *frame_pointer) {
	/* Platform-specific unwinding would go here */
	(void)frame_pointer;

	/* Run cleanup handlers */
	while (cleanup_stack) {
		cleanup_entry_t *entry = cleanup_stack;
		cleanup_stack = entry->next;

		if (entry->cleanup) {
			entry->cleanup(entry->arg);
		}
		free(entry);
	}
}

int except_get_unwind_reason(void) {
	return exception_active ? 1 : 0;
}

void except_register_cleanup(void (*cleanup)(void *), void *arg) {
	cleanup_entry_t *entry = (cleanup_entry_t *)malloc(sizeof(cleanup_entry_t));
	if (!entry) {
		return;
	}

	entry->cleanup = cleanup;
	entry->arg = arg;
	entry->next = cleanup_stack;
	cleanup_stack = entry;
}

/*
 * Exception Handlers
 */
void except_set_handler(except_handler_t handler) {
	global_handler = handler;
}

void except_set_terminate(void (*handler)(void)) {
	terminate_handler = handler;
}

void except_set_unexpected(void (*handler)(void)) {
	unexpected_handler = handler;
}

/*
 * Exception Information
 */
void **except_get_backtrace(int *size) {
	if (size) {
		*size = current_exception.backtrace_size;
	}
	return current_exception.backtrace;
}

void except_print_backtrace(void) {
	if (!exception_active || !current_exception.backtrace) {
		printf("No backtrace available\n");
		return;
	}

	printf("Backtrace (%d frames):\n", current_exception.backtrace_size);
	for (int i = 0; i < current_exception.backtrace_size; i++) {
		printf("  #%d: %p\n", i, current_exception.backtrace[i]);
	}
}

const char *except_get_message(void) {
	return exception_active ? current_exception.message : NULL;
}

const char *except_get_name(void) {
	return exception_active ? current_exception.name : NULL;
}

/*
 * Exception Translation
 */
void except_translate(except_type_t from_type, except_type_t to_type) {
	if (exception_active && current_exception.type == from_type) {
		current_exception.type = to_type;
	}
}

void except_map(const char *from_name, const char *to_name) {
	/* Exception name mapping - simplified implementation */
	(void)from_name;
	(void)to_name;
}
