/*
 * Copyright (c) 2025 PCC Project
 * Coroutine/Fiber Library - Generic Implementation
 * Uses setjmp/longjmp for broad platform compatibility
 */

#include "../coro.h"
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

#if defined(__unix__) || defined(__linux__) || defined(__APPLE__)
  #include <sys/mman.h>
  #include <unistd.h>
  #define HAS_MMAP 1
#else
  #define HAS_MMAP 0
#endif

#define DEFAULT_STACK_SIZE (64 * 1024)
#define PAGE_SIZE 4096

/* Coroutine structure */
struct coro {
	coro_state_t state;
	coro_func_t func;
	void *arg;
	void *userdata;

	/* Stack */
	void *stack;
	size_t stack_size;
	int stack_allocated;
	int stack_protected;

	/* Context */
	jmp_buf context;
	coro_t *caller;

	/* Transfer value */
	void *transfer_value;
};

/* Current coroutine */
static __thread coro_t *current_coro = NULL;
static __thread coro_t main_coro = {0};

/*
 * Stack allocation
 */

static void *allocate_stack(size_t size, int protected) {
	void *stack = NULL;

#if HAS_MMAP
	if (protected) {
		/* Allocate with guard pages */
		size_t total_size = size + 2 * PAGE_SIZE;
		void *mem = mmap(NULL, total_size, PROT_READ | PROT_WRITE,
		                 MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (mem == MAP_FAILED) return NULL;

		/* Protect guard pages */
		mprotect(mem, PAGE_SIZE, PROT_NONE);
		mprotect((char *)mem + PAGE_SIZE + size, PAGE_SIZE, PROT_NONE);

		stack = (char *)mem + PAGE_SIZE;
	} else {
		stack = mmap(NULL, size, PROT_READ | PROT_WRITE,
		             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
		if (stack == MAP_FAILED) return NULL;
	}
#else
	stack = malloc(size);
#endif

	return stack;
}

static void free_stack(void *stack, size_t size, int protected) {
#if HAS_MMAP
	if (protected) {
		munmap((char *)stack - PAGE_SIZE, size + 2 * PAGE_SIZE);
	} else {
		munmap(stack, size);
	}
#else
	free(stack);
#endif
}

/*
 * Coroutine wrapper
 */

static void coro_wrapper(void) {
	coro_t *coro = current_coro;
	if (!coro || !coro->func) return;

	/* Call user function */
	coro->func(coro->arg);

	/* Mark as terminated */
	coro->state = CORO_STATE_TERMINATED;

	/* Transfer back to caller */
	if (coro->caller) {
		current_coro = coro->caller;
		coro->caller->state = CORO_STATE_RUNNING;
		longjmp(coro->caller->context, 1);
	}
}

/*
 * Stack pointer manipulation
 */

#if defined(__GNUC__) || defined(__clang__)

static void setup_stack(coro_t *coro) {
	/* Get current stack pointer */
	void *sp = (char *)coro->stack + coro->stack_size;

	/* Align stack */
#if defined(__x86_64__) || defined(__aarch64__)
	sp = (void *)((uintptr_t)sp & ~15ULL);  /* 16-byte alignment */
#else
	sp = (void *)((uintptr_t)sp & ~7ULL);   /* 8-byte alignment */
#endif

	/* Save context */
	if (setjmp(coro->context) == 0) {
		/* Switch to new stack and call wrapper */
#if defined(__x86_64__)
		__asm__ volatile (
			"mov %0, %%rsp\n"
			:
			: "r"(sp)
		);
#elif defined(__i386__)
		__asm__ volatile (
			"mov %0, %%esp\n"
			:
			: "r"(sp)
		);
#elif defined(__aarch64__)
		__asm__ volatile (
			"mov sp, %0\n"
			:
			: "r"(sp)
		);
#elif defined(__arm__)
		__asm__ volatile (
			"mov sp, %0\n"
			:
			: "r"(sp)
		);
#endif
		coro_wrapper();
	}
}

#else

/* Fallback: No inline assembly support */
static void setup_stack(coro_t *coro) {
	if (setjmp(coro->context) == 0) {
		/* This is a simplified approach - full stack switching
		 * requires platform-specific code */
		coro_wrapper();
	}
}

#endif

/*
 * Public API
 */

coro_t *coro_create(coro_func_t func, void *arg, const coro_config_t *config) {
	if (!func) return NULL;

	coro_t *coro = calloc(1, sizeof(coro_t));
	if (!coro) return NULL;

	coro->func = func;
	coro->arg = arg;
	coro->state = CORO_STATE_READY;

	if (config) {
		coro->stack_size = config->stack_size > 0 ? config->stack_size : DEFAULT_STACK_SIZE;
		coro->userdata = config->userdata;
		coro->stack_protected = config->protected;
	} else {
		coro->stack_size = DEFAULT_STACK_SIZE;
	}

	/* Allocate stack */
	coro->stack = allocate_stack(coro->stack_size, coro->stack_protected);
	if (!coro->stack) {
		free(coro);
		return NULL;
	}
	coro->stack_allocated = 1;

	/* Setup initial context */
	setup_stack(coro);

	return coro;
}

void coro_destroy(coro_t *coro) {
	if (!coro) return;

	if (coro->stack && coro->stack_allocated) {
		free_stack(coro->stack, coro->stack_size, coro->stack_protected);
	}

	free(coro);
}

coro_transfer_t coro_transfer(coro_t *to, void *value) {
	coro_transfer_t result = {0};

	if (!to) {
		result.from = current_coro;
		return result;
	}

	coro_t *from = current_coro ? current_coro : &main_coro;

	from->state = CORO_STATE_SUSPENDED;
	to->state = CORO_STATE_RUNNING;
	to->caller = from;
	to->transfer_value = value;

	result.from = from;
	result.to = to;
	result.value = to->transfer_value;

	/* Save current context and switch to new coroutine */
	if (setjmp(from->context) == 0) {
		current_coro = to;
		longjmp(to->context, 1);
	}

	/* Resumed */
	from->state = CORO_STATE_RUNNING;
	return result;
}

void coro_yield(void *value) {
	coro_t *coro = current_coro;
	if (!coro || !coro->caller) return;

	coro_transfer(coro->caller, value);
}

void coro_resume(coro_t *coro, void *value) {
	if (!coro) return;
	coro_transfer(coro, value);
}

coro_t *coro_current(void) {
	return current_coro ? current_coro : &main_coro;
}

coro_state_t coro_get_state(coro_t *coro) {
	return coro ? coro->state : CORO_STATE_TERMINATED;
}

int coro_is_terminated(coro_t *coro) {
	return coro && coro->state == CORO_STATE_TERMINATED;
}

void *coro_get_stack(coro_t *coro) {
	return coro ? coro->stack : NULL;
}

size_t coro_get_stack_size(coro_t *coro) {
	return coro ? coro->stack_size : 0;
}

size_t coro_get_stack_usage(coro_t *coro) {
	/* Simplified: Would need platform-specific stack walking */
	return coro ? coro->stack_size / 2 : 0;
}

void *coro_get_userdata(coro_t *coro) {
	return coro ? coro->userdata : NULL;
}

void coro_set_userdata(coro_t *coro, void *userdata) {
	if (coro) coro->userdata = userdata;
}

size_t coro_default_stack_size(void) {
	return DEFAULT_STACK_SIZE;
}

int coro_platform_init(void) {
	/* Initialize main coroutine */
	if (!current_coro) {
		main_coro.state = CORO_STATE_RUNNING;
		current_coro = &main_coro;
	}
	return 1;
}

void coro_platform_shutdown(void) {
	current_coro = NULL;
}

/*
 * Context API
 */

int coro_context_init(coro_context_t *ctx, coro_func_t func, void *arg,
                      void *stack, size_t stack_size) {
	(void)ctx; (void)func; (void)arg; (void)stack; (void)stack_size;
	/* Platform-specific implementation */
	return 0;
}

void coro_context_switch(coro_context_t *from, coro_context_t *to) {
	(void)from; (void)to;
	/* Platform-specific implementation */
}
