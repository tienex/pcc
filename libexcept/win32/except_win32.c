/*
 * Copyright (c) 2025 PCC Project
 * Exception Handling Implementation - Windows
 * Uses Structured Exception Handling (SEH) and Windows threading
 */

#ifdef _WIN32

#include "../except.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <dbghelp.h>

#pragma comment(lib, "dbghelp.lib")

#define MAX_BACKTRACE 64

/* Exception state (thread-local via TLS) */
static DWORD tls_context = TLS_OUT_OF_INDEXES;
static DWORD tls_exception = TLS_OUT_OF_INDEXES;
static DWORD tls_active = TLS_OUT_OF_INDEXES;
static DWORD tls_cleanup = TLS_OUT_OF_INDEXES;

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

/* TLS accessors */
static inline except_context_t *get_current_context(void) {
	return (except_context_t *)TlsGetValue(tls_context);
}

static inline void set_current_context(except_context_t *ctx) {
	TlsSetValue(tls_context, ctx);
}

static inline except_info_t *get_current_exception(void) {
	return (except_info_t *)TlsGetValue(tls_exception);
}

static inline void set_current_exception(except_info_t *info) {
	TlsSetValue(tls_exception, info);
}

static inline int get_exception_active(void) {
	return (int)(intptr_t)TlsGetValue(tls_active);
}

static inline void set_exception_active(int active) {
	TlsSetValue(tls_active, (void *)(intptr_t)active);
}

static inline cleanup_entry_t *get_cleanup_stack(void) {
	return (cleanup_entry_t *)TlsGetValue(tls_cleanup);
}

static inline void set_cleanup_stack(cleanup_entry_t *stack) {
	TlsSetValue(tls_cleanup, stack);
}

/*
 * SEH Exception filter
 */
static LONG WINAPI seh_exception_filter(EXCEPTION_POINTERS *exception_info) {
	const char *exception_name = "SEH_EXCEPTION";
	const char *message = "";

	switch (exception_info->ExceptionRecord->ExceptionCode) {
	case EXCEPTION_ACCESS_VIOLATION:
		exception_name = "ACCESS_VIOLATION";
		message = "The thread tried to read from or write to a virtual address for which it does not have the appropriate access";
		break;
	case EXCEPTION_ARRAY_BOUNDS_EXCEEDED:
		exception_name = "ARRAY_BOUNDS_EXCEEDED";
		message = "Array index out of bounds";
		break;
	case EXCEPTION_BREAKPOINT:
		exception_name = "BREAKPOINT";
		message = "Breakpoint was encountered";
		break;
	case EXCEPTION_DATATYPE_MISALIGNMENT:
		exception_name = "DATATYPE_MISALIGNMENT";
		message = "Misaligned data access";
		break;
	case EXCEPTION_FLT_DENORMAL_OPERAND:
	case EXCEPTION_FLT_DIVIDE_BY_ZERO:
	case EXCEPTION_FLT_INEXACT_RESULT:
	case EXCEPTION_FLT_INVALID_OPERATION:
	case EXCEPTION_FLT_OVERFLOW:
	case EXCEPTION_FLT_STACK_CHECK:
	case EXCEPTION_FLT_UNDERFLOW:
		exception_name = "FLT_EXCEPTION";
		message = "Floating-point exception";
		break;
	case EXCEPTION_ILLEGAL_INSTRUCTION:
		exception_name = "ILLEGAL_INSTRUCTION";
		message = "Illegal instruction";
		break;
	case EXCEPTION_INT_DIVIDE_BY_ZERO:
		exception_name = "INT_DIVIDE_BY_ZERO";
		message = "Integer division by zero";
		break;
	case EXCEPTION_INT_OVERFLOW:
		exception_name = "INT_OVERFLOW";
		message = "Integer overflow";
		break;
	case EXCEPTION_STACK_OVERFLOW:
		exception_name = "STACK_OVERFLOW";
		message = "Stack overflow";
		break;
	default:
		break;
	}

	except_raise(exception_name, message);
	return EXCEPTION_EXECUTE_HANDLER;
}

/*
 * Backtrace capture using CaptureStackBackTrace
 */
static void capture_backtrace(except_info_t *info) {
	void *buffer[MAX_BACKTRACE];
	WORD nptrs = CaptureStackBackTrace(0, MAX_BACKTRACE, buffer, NULL);

	if (nptrs > 0) {
		info->backtrace = malloc(nptrs * sizeof(void *));
		if (info->backtrace) {
			memcpy(info->backtrace, buffer, nptrs * sizeof(void *));
			info->backtrace_size = nptrs;
		}
	}
}

/*
 * Initialization
 */
int except_init(void) {
	/* Allocate TLS slots */
	if (tls_context == TLS_OUT_OF_INDEXES) {
		tls_context = TlsAlloc();
		tls_exception = TlsAlloc();
		tls_active = TlsAlloc();
		tls_cleanup = TlsAlloc();
	}

	/* Initialize for this thread */
	set_current_context(NULL);
	set_exception_active(0);

	except_info_t *info = calloc(1, sizeof(except_info_t));
	set_current_exception(info);

	set_cleanup_stack(NULL);

	/* Install SEH handler */
	SetUnhandledExceptionFilter(seh_exception_filter);

	return 0;
}

void except_shutdown(void) {
	/* Free any remaining cleanup handlers */
	cleanup_entry_t *cleanup_stack = get_cleanup_stack();
	while (cleanup_stack) {
		cleanup_entry_t *next = cleanup_stack->next;
		free(cleanup_stack);
		cleanup_stack = next;
	}
	set_cleanup_stack(NULL);

	/* Free backtrace if allocated */
	except_info_t *info = get_current_exception();
	if (info) {
		if (info->backtrace) {
			free(info->backtrace);
			info->backtrace = NULL;
		}
		free(info);
		set_current_exception(NULL);
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

	/* Get current thread's exception info */
	except_info_t *current_exception = get_current_exception();
	if (!current_exception) {
		current_exception = calloc(1, sizeof(except_info_t));
		set_current_exception(current_exception);
	}

	/* Copy exception info */
	*current_exception = *info;
	set_exception_active(1);

	/* Capture backtrace using Windows API */
	capture_backtrace(current_exception);

	/* Run cleanup handlers */
	cleanup_entry_t *cleanup_stack = get_cleanup_stack();
	while (cleanup_stack) {
		cleanup_entry_t *entry = cleanup_stack;
		cleanup_stack = entry->next;

		if (entry->cleanup) {
			entry->cleanup(entry->arg);
		}
		free(entry);
	}
	set_cleanup_stack(NULL);

	/* If we have a context, longjmp to it */
	except_context_t *current_context = get_current_context();
	if (current_context) {
		current_context->info = *current_exception;
		current_context->caught = 1;
		longjmp(current_context->env, 1);
	}

	/* No handler - call global handler or terminate */
	if (global_handler) {
		global_handler(current_exception);
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
	if (get_exception_active()) {
		except_info_t *current_exception = get_current_exception();
		except_raise_full(current_exception);
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
	ctx->prev = get_current_context();
	ctx->caught = 0;
	set_current_context(ctx);

	return ctx;
}

void except_pop(void) {
	except_context_t *current_context = get_current_context();
	if (!current_context) {
		return;
	}

	set_current_context(current_context->prev);
}

const except_info_t *except_current(void) {
	return get_exception_active() ? get_current_exception() : NULL;
}

int except_is_caught(const char *name) {
	except_info_t *current_exception = get_current_exception();
	if (!get_exception_active() || !current_exception->name || !name) {
		return 0;
	}

	return strcmp(current_exception->name, name) == 0;
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

	if (destructor && obj) {
		except_register_cleanup(destructor, obj);
	}

	except_raise_full(&info);
}

void *except_cxx_begin_catch(void *exception_ptr) {
	except_info_t *current_exception = get_current_exception();
	return current_exception->exception_object;
}

void except_cxx_end_catch(void) {
	set_exception_active(0);
}

void *except_cxx_current_exception_type(void) {
	except_info_t *current_exception = get_current_exception();
	return get_exception_active() ? current_exception->type_info : NULL;
}

/*
 * Ada Exception Support
 */
void except_ada_raise(const char *exception_name, const char *message) {
	except_raise_typed(EXCEPT_TYPE_ADA, exception_name, message);
}

void *except_ada_current_occurrence(void) {
	if (get_exception_active()) {
		return (void *)get_current_exception();
	}
	return NULL;
}

/*
 * Stack Unwinding
 */
void except_force_unwind(void *frame_pointer) {
	(void)frame_pointer;

	/* Run cleanup handlers */
	cleanup_entry_t *cleanup_stack = get_cleanup_stack();
	while (cleanup_stack) {
		cleanup_entry_t *entry = cleanup_stack;
		cleanup_stack = entry->next;

		if (entry->cleanup) {
			entry->cleanup(entry->arg);
		}
		free(entry);
	}
	set_cleanup_stack(NULL);
}

int except_get_unwind_reason(void) {
	return get_exception_active() ? 1 : 0;
}

void except_register_cleanup(void (*cleanup)(void *), void *arg) {
	cleanup_entry_t *entry = (cleanup_entry_t *)malloc(sizeof(cleanup_entry_t));
	if (!entry) {
		return;
	}

	entry->cleanup = cleanup;
	entry->arg = arg;
	entry->next = get_cleanup_stack();
	set_cleanup_stack(entry);
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
	except_info_t *current_exception = get_current_exception();
	if (size) {
		*size = current_exception->backtrace_size;
	}
	return current_exception->backtrace;
}

void except_print_backtrace(void) {
	except_info_t *current_exception = get_current_exception();
	if (!get_exception_active() || !current_exception->backtrace) {
		printf("No backtrace available\n");
		return;
	}

	printf("Backtrace (%d frames):\n", current_exception->backtrace_size);

	/* Use SymFromAddr for symbolic names */
	HANDLE process = GetCurrentProcess();
	SymInitialize(process, NULL, TRUE);

	SYMBOL_INFO *symbol = (SYMBOL_INFO *)calloc(sizeof(SYMBOL_INFO) + 256 * sizeof(char), 1);
	symbol->MaxNameLen = 255;
	symbol->SizeOfStruct = sizeof(SYMBOL_INFO);

	for (int i = 0; i < current_exception->backtrace_size; i++) {
		DWORD64 address = (DWORD64)(current_exception->backtrace[i]);
		if (SymFromAddr(process, address, 0, symbol)) {
			printf("  #%d: %s (0x%p)\n", i, symbol->Name, current_exception->backtrace[i]);
		} else {
			printf("  #%d: %p\n", i, current_exception->backtrace[i]);
		}
	}

	free(symbol);
	SymCleanup(process);
}

const char *except_get_message(void) {
	except_info_t *current_exception = get_current_exception();
	return get_exception_active() ? current_exception->message : NULL;
}

const char *except_get_name(void) {
	except_info_t *current_exception = get_current_exception();
	return get_exception_active() ? current_exception->name : NULL;
}

/*
 * Exception Translation
 */
void except_translate(except_type_t from_type, except_type_t to_type) {
	except_info_t *current_exception = get_current_exception();
	if (get_exception_active() && current_exception->type == from_type) {
		current_exception->type = to_type;
	}
}

void except_map(const char *from_name, const char *to_name) {
	(void)from_name;
	(void)to_name;
}

#endif /* _WIN32 */
