/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Exception Handling Library
 *
 * Cross-language exception handling supporting:
 * - C++ exceptions
 * - Ada exceptions
 * - Modula-3 exceptions
 * - Oberon-2 TRAP
 * - C setjmp/longjmp
 * - SEH (Windows)
 * - DWARF unwinding
 */

#ifndef _PCC_EXCEPT_H_
#define _PCC_EXCEPT_H_

#include <stddef.h>
#include <stdint.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Exception Types
 */
typedef enum {
	EXCEPT_TYPE_C_LONGJMP = 0,
	EXCEPT_TYPE_CXX,
	EXCEPT_TYPE_ADA,
	EXCEPT_TYPE_MODULA3,
	EXCEPT_TYPE_OBERON,
	EXCEPT_TYPE_SEH,
	EXCEPT_TYPE_DWARF
} except_type_t;

/*
 * Exception Severity
 */
typedef enum {
	EXCEPT_FATAL = 0,
	EXCEPT_ERROR,
	EXCEPT_WARNING,
	EXCEPT_INFO
} except_severity_t;

/*
 * Exception Information
 */
typedef struct {
	const char *name;
	const char *message;
	except_type_t type;
	except_severity_t severity;
	void *exception_object;
	const char *source_file;
	int source_line;
	void **backtrace;
	int backtrace_size;
	void *type_info;  /* C++ type_info or similar */
} except_info_t;

/*
 * Exception Handler
 */
typedef void (*except_handler_t)(const except_info_t *info);

/*
 * Exception Context (for C-style exceptions)
 */
typedef struct except_context {
	jmp_buf env;
	except_info_t info;
	struct except_context *prev;
	int caught;
} except_context_t;

/*
 * Initialization
 */

/* Initialize exception handling system */
int except_init(void);

/* Shutdown exception handling */
void except_shutdown(void);

/*
 * Exception Raising
 */

/* Raise exception */
void except_raise(const char *name, const char *message);

/* Raise exception with type */
void except_raise_typed(except_type_t type, const char *name, const char *message);

/* Raise with full info */
void except_raise_full(const except_info_t *info);

/* Reraise current exception */
void except_reraise(void);

/*
 * Exception Handling (C style)
 */

/* Push exception context */
except_context_t *except_push(void);

/* Pop exception context */
void except_pop(void);

/* Get current exception info */
const except_info_t *except_current(void);

/* Check if exception is caught */
int except_is_caught(const char *name);

/*
 * C++ Exception Support
 */

/* Allocate C++ exception object */
void *except_cxx_allocate(size_t size);

/* Free C++ exception object */
void except_cxx_free(void *obj);

/* Throw C++ exception */
void except_cxx_throw(void *obj, void *type_info, void (*destructor)(void *));

/* Begin catch */
void *except_cxx_begin_catch(void *exception_ptr);

/* End catch */
void except_cxx_end_catch(void);

/* Get C++ exception type */
void *except_cxx_current_exception_type(void);

/*
 * Ada Exception Support
 */

/* Raise Ada exception */
void except_ada_raise(const char *exception_name, const char *message);

/* Get Ada exception occurrence */
void *except_ada_current_occurrence(void);

/*
 * Stack Unwinding
 */

/* Force unwind to specific frame */
void except_force_unwind(void *frame_pointer);

/* Get unwind reason */
int except_get_unwind_reason(void);

/* Register cleanup handler */
void except_register_cleanup(void (*cleanup)(void *), void *arg);

/*
 * Exception Handlers
 */

/* Set global exception handler */
void except_set_handler(except_handler_t handler);

/* Set terminate handler (unhandled exceptions) */
void except_set_terminate(void (*handler)(void));

/* Set unexpected handler */
void except_set_unexpected(void (*handler)(void));

/*
 * Exception Macros (C-style)
 */

#define EXCEPT_TRY(ctx) \
	do { \
		except_context_t *ctx = except_push(); \
		if (setjmp(ctx->env) == 0) {

#define EXCEPT_CATCH(exception_name) \
		except_pop(); \
		} else if (except_is_caught(exception_name)) {

#define EXCEPT_CATCH_ALL \
		except_pop(); \
		} else {

#define EXCEPT_END \
		} \
	} while(0)

#define EXCEPT_FINALLY \
	/* Cleanup code */

/*
 * Exception Information
 */

/* Get exception backtrace */
void **except_get_backtrace(int *size);

/* Print exception backtrace */
void except_print_backtrace(void);

/* Get exception message */
const char *except_get_message(void);

/* Get exception name */
const char *except_get_name(void);

/*
 * Exception Translation
 */

/* Convert between exception types */
void except_translate(except_type_t from_type, except_type_t to_type);

/* Map exception names between languages */
void except_map(const char *from_name, const char *to_name);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_EXCEPT_H_ */
