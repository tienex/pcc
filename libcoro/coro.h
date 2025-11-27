/*
 * Copyright (c) 2025 PCC Project
 * Coroutine/Fiber Library
 * Lightweight cooperative multitasking primitives
 */

#ifndef CORO_H
#define CORO_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct coro coro_t;
typedef struct coro_context coro_context_t;

/* Coroutine function */
typedef void (*coro_func_t)(void *arg);

/* Coroutine state */
typedef enum {
	CORO_STATE_READY,
	CORO_STATE_RUNNING,
	CORO_STATE_SUSPENDED,
	CORO_STATE_TERMINATED
} coro_state_t;

/* Coroutine transfer result */
typedef struct {
	coro_t *from;
	coro_t *to;
	void *value;
} coro_transfer_t;

/* Coroutine configuration */
typedef struct {
	size_t stack_size;      /* Stack size (0 = default) */
	void *userdata;         /* User data */
	int protected;          /* Protected stack (guard pages) */
} coro_config_t;

/*
 * Core API
 */

/* Create/destroy coroutines */
coro_t *coro_create(coro_func_t func, void *arg, const coro_config_t *config);
void coro_destroy(coro_t *coro);

/* Transfer control */
coro_transfer_t coro_transfer(coro_t *to, void *value);
void coro_yield(void *value);
void coro_resume(coro_t *coro, void *value);

/* State queries */
coro_t *coro_current(void);
coro_state_t coro_get_state(coro_t *coro);
int coro_is_terminated(coro_t *coro);

/* Stack management */
void *coro_get_stack(coro_t *coro);
size_t coro_get_stack_size(coro_t *coro);
size_t coro_get_stack_usage(coro_t *coro);

/* User data */
void *coro_get_userdata(coro_t *coro);
void coro_set_userdata(coro_t *coro, void *userdata);

/*
 * Context API (low-level)
 */

/* Context structure (platform-specific) */
struct coro_context {
#if defined(__x86_64__) || defined(_M_X64)
	/* x86-64: rip, rsp, rbp, rbx, r12-r15 */
	uint64_t regs[9];
#elif defined(__i386__) || defined(_M_IX86)
	/* x86-32: eip, esp, ebp, ebx, esi, edi */
	uint32_t regs[6];
#elif defined(__aarch64__) || defined(_M_ARM64)
	/* ARM64: x19-x30, sp, lr */
	uint64_t regs[14];
#elif defined(__arm__) || defined(_M_ARM)
	/* ARM32: r4-r11, sp, lr */
	uint32_t regs[10];
#else
	/* Generic: Use setjmp/longjmp buffer */
	uint8_t jmpbuf[256];
#endif
};

/* Low-level context operations */
int coro_context_init(coro_context_t *ctx, coro_func_t func, void *arg,
                      void *stack, size_t stack_size);
void coro_context_switch(coro_context_t *from, coro_context_t *to);

/*
 * Utilities
 */

/* Get default stack size */
size_t coro_default_stack_size(void);

/* Platform initialization */
int coro_platform_init(void);
void coro_platform_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* CORO_H */
