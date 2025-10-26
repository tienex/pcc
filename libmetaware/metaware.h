/*
 * MetaWare High C Compiler Extensions for PCC
 * Runtime support for MetaWare's innovative language features (circa 1989)
 *
 * This library provides runtime support for MetaWare High C extensions:
 * - Nested functions with closures
 * - Generator coroutines with yield
 * - Function values (trampolines for nested functions)
 *
 * Note: Some features require compiler frontend support (documented separately)
 */

#ifndef _METAWARE_H
#define _METAWARE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <setjmp.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ================================================================
 * NESTED FUNCTIONS AND CLOSURES
 * ================================================================ */

/*
 * MetaWare High C allowed nested functions with full closure support.
 * Nested functions can access variables from enclosing scopes.
 *
 * Example syntax (requires compiler support):
 *   int outer(int x) {
 *       int nested(int y) {
 *           return x + y;  // Captures 'x' from outer
 *       }
 *       return nested(10);
 *   }
 */

/* Maximum nesting depth for functions */
#define MW_MAX_NESTING_DEPTH 16

/* Maximum captured variables per nested function */
#define MW_MAX_CAPTURES 32

/* Trampoline structure for nested functions */
typedef struct mw_trampoline {
    void (*func_ptr)(void);      /* Actual function pointer */
    void *static_chain;          /* Pointer to parent frame */
    void *captures[MW_MAX_CAPTURES]; /* Captured variables */
    size_t num_captures;         /* Number of captured variables */
} mw_trampoline_t;

/* Create a trampoline for a nested function */
mw_trampoline_t *mw_create_trampoline(void (*func)(void), void *static_chain);

/* Add a captured variable to the trampoline */
void mw_add_capture(mw_trampoline_t *tramp, void *var_ptr);

/* Call a trampolined function */
void mw_call_trampoline(mw_trampoline_t *tramp, void *args, void *result);

/* Free a trampoline */
void mw_free_trampoline(mw_trampoline_t *tramp);

/* ================================================================
 * GENERATOR COROUTINES
 * ================================================================ */

/*
 * MetaWare High C supported Python-style generators in 1989!
 *
 * Syntax (requires compiler support):
 *   void fibonacci(int count) -> (int number) {
 *       int a = 0, b = 1;
 *       for (int i = 0; i < count; i++) {
 *           yield(a);
 *           int temp = a;
 *           a = b;
 *           b = temp + b;
 *       }
 *   }
 *
 *   // Usage:
 *   for num <- fibonacci(10) do {
 *       printf("%d ", num);
 *   }
 */

/* Generator state */
typedef enum {
    MW_GEN_READY,      /* Generator created, not started */
    MW_GEN_RUNNING,    /* Generator currently executing */
    MW_GEN_SUSPENDED,  /* Generator suspended at yield */
    MW_GEN_DONE        /* Generator finished */
} mw_gen_state_t;

/* Generator control structure */
typedef struct mw_generator {
    jmp_buf caller_env;        /* Caller's execution context */
    jmp_buf generator_env;     /* Generator's execution context */
    void *stack;               /* Dedicated stack for generator */
    size_t stack_size;         /* Stack size */
    void *yielded_value;       /* Last yielded value */
    size_t value_size;         /* Size of yielded value type */
    mw_gen_state_t state;      /* Current state */
    void *user_data;           /* User-provided data */
    int return_code;           /* Return value from setjmp */
} mw_generator_t;

/* Generator function prototype */
typedef void (*mw_gen_func_t)(mw_generator_t *gen, void *args);

/* Create a new generator */
mw_generator_t *mw_create_generator(mw_gen_func_t func, void *args,
                                     size_t value_size, size_t stack_size);

/* Yield a value from within a generator */
void mw_yield(mw_generator_t *gen, void *value);

/* Get next value from generator (returns false when done) */
bool mw_next(mw_generator_t *gen, void *result);

/* Check if generator is done */
bool mw_generator_done(mw_generator_t *gen);

/* Free generator resources */
void mw_free_generator(mw_generator_t *gen);

/* ================================================================
 * GENERATOR MACROS (Convenience)
 * ================================================================ */

/*
 * These macros provide a more C-friendly interface to generators
 */

/* Declare a generator function */
#define MW_GENERATOR(name, arg_type, yield_type) \
    void name##_gen_impl(mw_generator_t *__gen, void *__args); \
    mw_generator_t *name(arg_type args) { \
        return mw_create_generator(name##_gen_impl, &args, \
                                    sizeof(yield_type), 65536); \
    } \
    void name##_gen_impl(mw_generator_t *__gen, void *__args)

/* Yield a value */
#define MW_YIELD(gen, value) mw_yield(gen, &(value))

/* Iterate over generator values */
#define MW_FOR_EACH(var, generator) \
    for (bool __cont = mw_next(generator, &var); __cont; \
         __cont = mw_next(generator, &var))

/* ================================================================
 * FUNCTION VALUES (Full Function Closures)
 * ================================================================ */

/*
 * MetaWare supported "full function value" types that work as
 * non-escaping closures with both code and context pointers.
 */

typedef struct mw_function {
    void *code;          /* Function code pointer */
    void *context;       /* Captured context */
    size_t context_size; /* Size of context */
} mw_function_t;

/* Create a function value with context */
mw_function_t *mw_create_function(void *code, void *context, size_t context_size);

/* Call a function value */
void mw_call_function(mw_function_t *func, void *args, void *result);

/* Free a function value */
void mw_free_function(mw_function_t *func);

/* ================================================================
 * NON-LOCAL GOTO SUPPORT
 * ================================================================ */

/*
 * MetaWare allowed goto from nested functions back to parent functions
 * with automatic stack unwinding.
 *
 * Syntax (requires compiler support):
 *   int outer() {
 *   cleanup:
 *       // cleanup code
 *       return 0;
 *
 *       void nested() {
 *           if (error)
 *               goto cleanup;  // Jump to parent's label!
 *       }
 *   }
 */

/* Non-local label structure */
typedef struct mw_label {
    jmp_buf env;             /* Saved environment */
    int level;               /* Nesting level */
    void (*cleanup)(void *); /* Cleanup function */
    void *cleanup_data;      /* Cleanup data */
} mw_label_t;

/* Define a non-local label */
#define MW_LABEL(name) \
    mw_label_t name; \
    if (setjmp(name.env) != 0) goto __mw_label_##name; \
    __mw_label_##name:

/* Goto a non-local label */
void mw_goto_label(mw_label_t *label, int value);

/* Set cleanup handler for label */
void mw_set_label_cleanup(mw_label_t *label, void (*cleanup)(void *), void *data);

/* ================================================================
 * UTILITY FUNCTIONS
 * ================================================================ */

/* Get current nesting level */
int mw_get_nesting_level(void);

/* Push/pop nesting level (for compiler use) */
void mw_push_nesting_level(void);
void mw_pop_nesting_level(void);

#ifdef __cplusplus
}
#endif

#endif /* _METAWARE_H */
