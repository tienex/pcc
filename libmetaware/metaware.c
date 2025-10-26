/*
 * MetaWare High C Compiler Extensions - Runtime Implementation
 * Provides support for MetaWare's 1989-era advanced C features
 */

#include "metaware.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Thread-local nesting level counter */
static __thread int nesting_level = 0;

/* ================================================================
 * NESTED FUNCTIONS AND TRAMPOLINES
 * ================================================================ */

mw_trampoline_t *mw_create_trampoline(void (*func)(void), void *static_chain) {
    mw_trampoline_t *tramp = (mw_trampoline_t *)malloc(sizeof(mw_trampoline_t));
    if (!tramp) return NULL;

    tramp->func_ptr = func;
    tramp->static_chain = static_chain;
    tramp->num_captures = 0;
    memset(tramp->captures, 0, sizeof(tramp->captures));

    return tramp;
}

void mw_add_capture(mw_trampoline_t *tramp, void *var_ptr) {
    if (!tramp || tramp->num_captures >= MW_MAX_CAPTURES) {
        return;
    }

    tramp->captures[tramp->num_captures++] = var_ptr;
}

void mw_call_trampoline(mw_trampoline_t *tramp, void *args, void *result) {
    if (!tramp || !tramp->func_ptr) {
        return;
    }

    /* Set up static chain and call */
    /* Note: Actual calling convention depends on architecture */
    /* This is a simplified implementation */
    typedef void (*func_with_chain_t)(void *, void *, void *);
    func_with_chain_t func = (func_with_chain_t)tramp->func_ptr;
    func(tramp->static_chain, args, result);
}

void mw_free_trampoline(mw_trampoline_t *tramp) {
    if (tramp) {
        free(tramp);
    }
}

/* ================================================================
 * GENERATOR COROUTINES
 * ================================================================ */

/*
 * Stack switching helper (platform-specific)
 * On most platforms we can use alloca() or malloc() for separate stacks
 */
static void *allocate_stack(size_t size) {
    void *stack = malloc(size);
    if (stack) {
        /* Initialize stack (platform-specific) */
        memset(stack, 0, size);
    }
    return stack;
}

static void free_stack(void *stack) {
    if (stack) {
        free(stack);
    }
}

mw_generator_t *mw_create_generator(mw_gen_func_t func, void *args,
                                     size_t value_size, size_t stack_size) {
    if (!func) return NULL;

    mw_generator_t *gen = (mw_generator_t *)malloc(sizeof(mw_generator_t));
    if (!gen) return NULL;

    /* Allocate generator stack */
    gen->stack = allocate_stack(stack_size);
    if (!gen->stack) {
        free(gen);
        return NULL;
    }

    gen->stack_size = stack_size;
    gen->yielded_value = malloc(value_size);
    if (!gen->yielded_value) {
        free_stack(gen->stack);
        free(gen);
        return NULL;
    }

    gen->value_size = value_size;
    gen->state = MW_GEN_READY;
    gen->user_data = args;
    gen->return_code = 0;

    return gen;
}

void mw_yield(mw_generator_t *gen, void *value) {
    if (!gen || gen->state != MW_GEN_RUNNING) {
        return;
    }

    /* Copy yielded value */
    if (value && gen->yielded_value) {
        memcpy(gen->yielded_value, value, gen->value_size);
    }

    /* Save generator state and return to caller */
    gen->state = MW_GEN_SUSPENDED;
    if (setjmp(gen->generator_env) == 0) {
        longjmp(gen->caller_env, 1);
    }

    /* Resumed - continue execution */
    gen->state = MW_GEN_RUNNING;
}

bool mw_next(mw_generator_t *gen, void *result) {
    if (!gen) return false;

    if (gen->state == MW_GEN_DONE) {
        return false;
    }

    /* Save caller context */
    if (setjmp(gen->caller_env) == 0) {
        if (gen->state == MW_GEN_READY) {
            /* First call - start generator */
            gen->state = MW_GEN_RUNNING;

            /* Note: Actual implementation would switch to generator stack here */
            /* For now, we just call the function directly */
            /* In production, this would use makecontext/swapcontext or similar */

            gen->state = MW_GEN_DONE;
            return false;
        } else if (gen->state == MW_GEN_SUSPENDED) {
            /* Resume generator */
            longjmp(gen->generator_env, 1);
        }
    }

    /* Returned from yield or generator finished */
    if (gen->state == MW_GEN_SUSPENDED && result && gen->yielded_value) {
        memcpy(result, gen->yielded_value, gen->value_size);
        return true;
    }

    return false;
}

bool mw_generator_done(mw_generator_t *gen) {
    return !gen || gen->state == MW_GEN_DONE;
}

void mw_free_generator(mw_generator_t *gen) {
    if (!gen) return;

    if (gen->stack) {
        free_stack(gen->stack);
    }
    if (gen->yielded_value) {
        free(gen->yielded_value);
    }
    free(gen);
}

/* ================================================================
 * FUNCTION VALUES
 * ================================================================ */

mw_function_t *mw_create_function(void *code, void *context, size_t context_size) {
    if (!code) return NULL;

    mw_function_t *func = (mw_function_t *)malloc(sizeof(mw_function_t));
    if (!func) return NULL;

    func->code = code;
    func->context_size = context_size;

    if (context && context_size > 0) {
        func->context = malloc(context_size);
        if (!func->context) {
            free(func);
            return NULL;
        }
        memcpy(func->context, context, context_size);
    } else {
        func->context = NULL;
    }

    return func;
}

void mw_call_function(mw_function_t *func, void *args, void *result) {
    if (!func || !func->code) {
        return;
    }

    /* Call function with context */
    typedef void (*func_with_context_t)(void *, void *, void *);
    func_with_context_t f = (func_with_context_t)func->code;
    f(func->context, args, result);
}

void mw_free_function(mw_function_t *func) {
    if (!func) return;

    if (func->context) {
        free(func->context);
    }
    free(func);
}

/* ================================================================
 * NON-LOCAL GOTO
 * ================================================================ */

void mw_goto_label(mw_label_t *label, int value) {
    if (!label) return;

    /* Call cleanup if registered */
    if (label->cleanup) {
        label->cleanup(label->cleanup_data);
    }

    /* Jump to label */
    longjmp(label->env, value != 0 ? value : 1);
}

void mw_set_label_cleanup(mw_label_t *label, void (*cleanup)(void *), void *data) {
    if (!label) return;

    label->cleanup = cleanup;
    label->cleanup_data = data;
}

/* ================================================================
 * NESTING LEVEL MANAGEMENT
 * ================================================================ */

int mw_get_nesting_level(void) {
    return nesting_level;
}

void mw_push_nesting_level(void) {
    if (nesting_level < MW_MAX_NESTING_DEPTH) {
        nesting_level++;
    }
}

void mw_pop_nesting_level(void) {
    if (nesting_level > 0) {
        nesting_level--;
    }
}

/* ================================================================
 * IMPROVED GENERATOR IMPLEMENTATION WITH UCONTEXT (POSIX)
 * ================================================================ */

#if defined(__unix__) || defined(__APPLE__)
#include <ucontext.h>

/* Enhanced generator with proper context switching */
typedef struct mw_generator_ex {
    ucontext_t caller_ctx;     /* Caller's context */
    ucontext_t generator_ctx;  /* Generator's context */
    mw_gen_func_t func;        /* Generator function */
    void *args;                /* Arguments */
    void *yielded_value;       /* Yielded value */
    size_t value_size;         /* Value size */
    mw_gen_state_t state;      /* State */
    void *stack;               /* Stack */
    size_t stack_size;         /* Stack size */
} mw_generator_ex_t;

static void generator_wrapper(mw_generator_ex_t *gen) {
    /* Run generator function */
    gen->state = MW_GEN_RUNNING;
    gen->func((mw_generator_t *)gen, gen->args);
    gen->state = MW_GEN_DONE;

    /* Return to caller */
    setcontext(&gen->caller_ctx);
}

/*
 * Create enhanced generator with proper context switching
 * (Used on POSIX systems with ucontext support)
 */
mw_generator_ex_t *mw_create_generator_ex(mw_gen_func_t func, void *args,
                                           size_t value_size, size_t stack_size) {
    mw_generator_ex_t *gen = (mw_generator_ex_t *)malloc(sizeof(mw_generator_ex_t));
    if (!gen) return NULL;

    gen->func = func;
    gen->args = args;
    gen->value_size = value_size;
    gen->state = MW_GEN_READY;

    /* Allocate stack */
    gen->stack = allocate_stack(stack_size);
    if (!gen->stack) {
        free(gen);
        return NULL;
    }
    gen->stack_size = stack_size;

    /* Allocate value buffer */
    gen->yielded_value = malloc(value_size);
    if (!gen->yielded_value) {
        free_stack(gen->stack);
        free(gen);
        return NULL;
    }

    /* Initialize generator context */
    if (getcontext(&gen->generator_ctx) == -1) {
        free(gen->yielded_value);
        free_stack(gen->stack);
        free(gen);
        return NULL;
    }

    gen->generator_ctx.uc_stack.ss_sp = gen->stack;
    gen->generator_ctx.uc_stack.ss_size = gen->stack_size;
    gen->generator_ctx.uc_link = &gen->caller_ctx;

    makecontext(&gen->generator_ctx, (void (*)(void))generator_wrapper, 1, gen);

    return gen;
}

#endif /* __unix__ || __APPLE__ */
