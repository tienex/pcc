/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Core Runtime Support
 * - Initialization and finalization
 * - Error handling and exceptions
 * - Module/package management
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <setjmp.h>
#include "wirthrt.h"

/* Runtime state */
static int wirth_initialized = 0;
static const char *current_language = "Pascal";
static int argc_saved = 0;
static char **argv_saved = NULL;

/* Error handling */
static void (*user_error_handler)(WirthError, const char *) = NULL;
static WirthExceptionContext *exception_stack = NULL;

/* Module registry */
#define MAX_MODULES 256
static WirthModule *module_registry[MAX_MODULES];
static int module_count = 0;

/* Error messages */
static const char *error_messages[] = {
	[WIRTH_ERR_NONE] = "No error",
	[WIRTH_ERR_RANGE_CHECK] = "Range check error",
	[WIRTH_ERR_INDEX_CHECK] = "Index out of bounds",
	[WIRTH_ERR_CASE_SELECT] = "Case selector error",
	[WIRTH_ERR_TYPE_GUARD] = "Type guard error",
	[WIRTH_ERR_INTEGER_OVERFLOW] = "Integer overflow",
	[WIRTH_ERR_INTEGER_DIVIDE] = "Integer division by zero",
	[WIRTH_ERR_REAL_OVERFLOW] = "Real overflow",
	[WIRTH_ERR_REAL_UNDERFLOW] = "Real underflow",
	[WIRTH_ERR_REAL_DIVIDE] = "Real division by zero",
	[WIRTH_ERR_STACK_OVERFLOW] = "Stack overflow",
	[WIRTH_ERR_HEAP_OVERFLOW] = "Heap overflow",
	[WIRTH_ERR_NIL_POINTER] = "Nil pointer dereference",
	[WIRTH_ERR_INVALID_POINTER] = "Invalid pointer",
	[WIRTH_ERR_FILE_NOT_FOUND] = "File not found",
	[WIRTH_ERR_FILE_NOT_OPEN] = "File not open",
	[WIRTH_ERR_FILE_READ] = "File read error",
	[WIRTH_ERR_FILE_WRITE] = "File write error",
	[WIRTH_ERR_END_OF_FILE] = "End of file",
	[WIRTH_ERR_MODULE_NOT_FOUND] = "Module not found",
	[WIRTH_ERR_CIRCULAR_IMPORT] = "Circular import",
	[WIRTH_ERR_CONSTRAINT_ERROR] = "Constraint error (Ada)",
	[WIRTH_ERR_PROGRAM_ERROR] = "Program error (Ada)",
	[WIRTH_ERR_STORAGE_ERROR] = "Storage error (Ada)",
	[WIRTH_ERR_TASKING_ERROR] = "Tasking error (Ada)",
	[WIRTH_ERR_THREAD_ALERTED] = "Thread alerted (Modula-3)",
	[WIRTH_ERR_ASSERT_FAILED] = "Assertion failed",
	[WIRTH_ERR_INVALID_OPERATION] = "Invalid operation"
};

/* Signal handler for runtime errors */
static void signal_handler(int sig) {
	WirthError error = WIRTH_ERR_NONE;
	const char *msg = "Signal received";

	switch (sig) {
	case SIGFPE:
		error = WIRTH_ERR_REAL_DIVIDE;
		msg = "Floating point exception";
		break;
	case SIGSEGV:
		error = WIRTH_ERR_NIL_POINTER;
		msg = "Segmentation fault (possible nil dereference)";
		break;
	case SIGINT:
		error = WIRTH_ERR_PROGRAM_ERROR;
		msg = "Interrupt signal";
		break;
	default:
		error = WIRTH_ERR_INVALID_OPERATION;
		msg = "Unknown signal";
		break;
	}

	wirth_runtime_error(error, msg);
}

/*
 * Runtime Initialization
 */

void wirth_init_runtime(int argc, char **argv) {
	if (wirth_initialized) {
		return;
	}

	argc_saved = argc;
	argv_saved = argv;

	/* Install signal handlers */
	signal(SIGFPE, signal_handler);
	signal(SIGSEGV, signal_handler);
	signal(SIGINT, signal_handler);

	/* Initialize subsystems */
	exception_stack = NULL;
	module_count = 0;
	memset(module_registry, 0, sizeof(module_registry));

	wirth_initialized = 1;
}

void wirth_fini_runtime(void) {
	if (!wirth_initialized) {
		return;
	}

	/* Finalize all modules in reverse order */
	for (int i = module_count - 1; i >= 0; i--) {
		if (module_registry[i]) {
			wirth_module_fini(module_registry[i]);
		}
	}

	wirth_initialized = 0;
}

void wirth_set_language(const char *lang) {
	current_language = lang;
}

/*
 * Module/Package Management
 */

WirthModule *wirth_module_register(const char *name,
                                     void (*init)(void),
                                     void (*fini)(void)) {
	if (module_count >= MAX_MODULES) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Too many modules");
		return NULL;
	}

	WirthModule *module = (WirthModule *)malloc(sizeof(WirthModule));
	if (!module) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate module");
		return NULL;
	}

	module->name = strdup(name);
	module->init_proc = init;
	module->fini_proc = fini;
	module->initialized = 0;
	module->import_count = 0;
	module->imports = NULL;

	module_registry[module_count++] = module;

	return module;
}

void wirth_module_import(WirthModule *module, WirthModule *import) {
	if (!module || !import) {
		return;
	}

	/* Ensure import is initialized first */
	if (!import->initialized) {
		wirth_module_init(import);
	}

	/* Add to imports list */
	WirthModule **new_imports = (WirthModule **)realloc(
		module->imports,
		(module->import_count + 1) * sizeof(WirthModule *)
	);

	if (new_imports) {
		module->imports = new_imports;
		module->imports[module->import_count++] = import;
	}
}

void wirth_module_init(WirthModule *module) {
	if (!module || module->initialized) {
		return;
	}

	/* Initialize imports first */
	for (int i = 0; i < module->import_count; i++) {
		if (module->imports[i] && !module->imports[i]->initialized) {
			wirth_module_init(module->imports[i]);
		}
	}

	/* Call module initializer */
	if (module->init_proc) {
		module->init_proc();
	}

	module->initialized = 1;
}

void wirth_module_fini(WirthModule *module) {
	if (!module || !module->initialized) {
		return;
	}

	/* Call module finalizer */
	if (module->fini_proc) {
		module->fini_proc();
	}

	module->initialized = 0;

	/* Free resources */
	if (module->name) {
		free((void *)module->name);
	}
	if (module->imports) {
		free(module->imports);
	}
}

/*
 * Exception Handling
 */

void wirth_runtime_error(WirthError error, const char *message) {
	/* If there's a user handler, call it */
	if (user_error_handler) {
		user_error_handler(error, message);
		return;
	}

	/* If there's an exception context, raise exception */
	if (exception_stack) {
		exception_stack->error = error;
		if (message) {
			strncpy(exception_stack->message, message, 255);
			exception_stack->message[255] = '\0';
		} else if (error < sizeof(error_messages)/sizeof(error_messages[0])) {
			strncpy(exception_stack->message, error_messages[error], 255);
			exception_stack->message[255] = '\0';
		}
		longjmp(exception_stack->env, 1);
	}

	/* Default: print error and exit */
	fprintf(stderr, "\n*** Runtime Error in %s Program ***\n", current_language);
	if (message) {
		fprintf(stderr, "Error: %s\n", message);
	} else if (error < sizeof(error_messages)/sizeof(error_messages[0])) {
		fprintf(stderr, "Error: %s\n", error_messages[error]);
	} else {
		fprintf(stderr, "Error code: %d\n", error);
	}
	exit(1);
}

void wirth_set_error_handler(void (*handler)(WirthError, const char *)) {
	user_error_handler = handler;
}

WirthExceptionContext *wirth_exception_push(void) {
	WirthExceptionContext *ctx = (WirthExceptionContext *)malloc(sizeof(WirthExceptionContext));
	if (!ctx) {
		wirth_runtime_error(WIRTH_ERR_HEAP_OVERFLOW, "Cannot allocate exception context");
		return NULL;
	}

	ctx->error = WIRTH_ERR_NONE;
	ctx->message[0] = '\0';
	ctx->prev = exception_stack;
	exception_stack = ctx;

	return ctx;
}

void wirth_exception_pop(void) {
	if (!exception_stack) {
		return;
	}

	WirthExceptionContext *ctx = exception_stack;
	exception_stack = ctx->prev;
	free(ctx);
}

void wirth_exception_raise(WirthError error, const char *message) {
	wirth_runtime_error(error, message);
}

/*
 * Range Checking
 */

int wirth_range_check(int value, int min, int max, const char *name) {
	if (value < min || value > max) {
		char msg[256];
		if (name) {
			snprintf(msg, sizeof(msg), "Value %d out of range [%d..%d] for %s",
			         value, min, max, name);
		} else {
			snprintf(msg, sizeof(msg), "Value %d out of range [%d..%d]",
			         value, min, max);
		}
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, msg);
	}
	return value;
}

void wirth_index_check(int index, int length, const char *name) {
	if (index < 0 || index >= length) {
		char msg[256];
		if (name) {
			snprintf(msg, sizeof(msg), "Index %d out of bounds [0..%d) for %s",
			         index, length, name);
		} else {
			snprintf(msg, sizeof(msg), "Index %d out of bounds [0..%d)",
			         index, length);
		}
		wirth_runtime_error(WIRTH_ERR_INDEX_CHECK, msg);
	}
}

void wirth_nil_check(void *ptr, const char *name) {
	if (ptr == NULL) {
		char msg[256];
		if (name) {
			snprintf(msg, sizeof(msg), "Nil pointer dereference: %s", name);
		} else {
			snprintf(msg, sizeof(msg), "Nil pointer dereference");
		}
		wirth_runtime_error(WIRTH_ERR_NIL_POINTER, msg);
	}
}

/*
 * Ordinal Operations
 */

int wirth_ord(char c) {
	return (int)(unsigned char)c;
}

char wirth_chr(int x) {
	if (x < 0 || x > 255) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "CHR argument out of range");
	}
	return (char)x;
}

int wirth_succ(int x) {
	return x + 1;
}

int wirth_pred(int x) {
	return x - 1;
}

int wirth_max_int(int a, int b) {
	return (a > b) ? a : b;
}

int wirth_min_int(int a, int b) {
	return (a < b) ? a : b;
}

double wirth_max_real(double a, double b) {
	return (a > b) ? a : b;
}

double wirth_min_real(double a, double b) {
	return (a < b) ? a : b;
}
