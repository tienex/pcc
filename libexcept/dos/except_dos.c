/*
 * Copyright (c) 2025 PCC Project
 * Exception Handling Implementation - DOS (16-bit and 32-bit)
 * Uses simple longjmp-based exception handling (no threading)
 */

#if defined(__DOS__) || defined(__MSDOS__) || defined(_MSDOS)

#include "../except.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dos.h>
#include <signal.h>

#define MAX_BACKTRACE 16  /* Smaller for DOS */

/* Exception state (single-threaded, global) */
static except_context_t *current_context = NULL;
static except_info_t current_exception;
static int exception_active = 0;

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

static cleanup_entry_t *cleanup_stack = NULL;

/* Old signal handlers */
static void (__interrupt __far *old_int0)(void);  /* Divide by zero */
static void (__interrupt __far *old_int4)(void);  /* Overflow */
static void (__interrupt __far *old_int6)(void);  /* Invalid opcode */

/* Signal handler for hardware exceptions */
static void signal_handler(int sig) {
	const char *exception_name = "SIGNAL";
	const char *message = "";

	switch (sig) {
	case SIGFPE:  exception_name = "SIGFPE"; message = "Floating point exception"; break;
	case SIGILL:  exception_name = "SIGILL"; message = "Illegal instruction"; break;
	case SIGSEGV: exception_name = "SIGSEGV"; message = "Segmentation fault"; break;
	case SIGABRT: exception_name = "SIGABRT"; message = "Abort signal"; break;
	default: break;
	}

	except_raise(exception_name, message);
}

/* INT 0 handler (divide by zero) */
static void __interrupt __far int0_handler(void) {
	signal(SIGFPE, signal_handler);
	raise(SIGFPE);
}

/* INT 4 handler (overflow) */
static void __interrupt __far int4_handler(void) {
	signal(SIGFPE, signal_handler);
	raise(SIGFPE);
}

/* INT 6 handler (invalid opcode) */
static void __interrupt __far int6_handler(void) {
	signal(SIGILL, signal_handler);
	raise(SIGILL);
}

/*
 * Simple backtrace using frame pointer walking (limited on DOS)
 */
static void capture_backtrace(except_info_t *info) {
	void *buffer[MAX_BACKTRACE];
	int nptrs = 0;

#ifdef __WATCOMC__
	/* Watcom C - walk the stack frame */
	void **ebp;
	__asm {
		mov ebp, ebp
	}

	for (nptrs = 0; nptrs < MAX_BACKTRACE && ebp && ebp[1]; nptrs++) {
		buffer[nptrs] = ebp[1];  /* Return address */
		ebp = (void **)ebp[0];   /* Previous frame */
	}
#elif defined(__DJGPP__)
	/* DJGPP - use similar frame walking */
	void **ebp;
	asm("movl %%ebp, %0" : "=r"(ebp));

	for (nptrs = 0; nptrs < MAX_BACKTRACE && ebp && ebp[1]; nptrs++) {
		buffer[nptrs] = ebp[1];
		ebp = (void **)ebp[0];
	}
#else
	/* Other DOS compilers - minimal backtrace */
	nptrs = 0;
#endif

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
	current_context = NULL;
	exception_active = 0;
	memset(&current_exception, 0, sizeof(current_exception));

	/* Install signal handlers */
	signal(SIGFPE, signal_handler);
	signal(SIGILL, signal_handler);
	signal(SIGSEGV, signal_handler);
	signal(SIGABRT, signal_handler);

	/* Install DOS interrupt handlers for better exception handling */
#ifdef __WATCOMC__
	old_int0 = _dos_getvect(0);  /* Divide by zero */
	old_int4 = _dos_getvect(4);  /* Overflow */
	old_int6 = _dos_getvect(6);  /* Invalid opcode */

	_dos_setvect(0, int0_handler);
	_dos_setvect(4, int4_handler);
	_dos_setvect(6, int6_handler);
#endif

	return 0;
}

void except_shutdown(void) {
	/* Restore old interrupt handlers */
#ifdef __WATCOMC__
	if (old_int0) _dos_setvect(0, old_int0);
	if (old_int4) _dos_setvect(4, old_int4);
	if (old_int6) _dos_setvect(6, old_int6);
#endif

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

	/* Capture backtrace */
	capture_backtrace(&current_exception);

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
}

const except_info_t *except_current(void) {
	return exception_active ? &current_exception : NULL;
}

int except_is_caught(const char *name) {
	if (!exception_active || !current_exception.name || !name) {
		return 0;
	}

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
	(void)from_name;
	(void)to_name;
}

#endif /* __DOS__ */
