# Compiler Patch: Labeled/Named Arguments (Keyword Arguments)

## Feature Description

Add support for labeled arguments (keyword arguments) allowing arguments to be passed by name in any order:

```c
void drawRect(int x, int y, int width, int height, int color);

/* Call with labeled arguments */
drawRect(
    width => 200,
    height => 100,
    x => 50,
    y => 75,
    color => 0xFF0000
);
```

This feature was in MetaWare High C (1989), **12 years before Python popularized it**.

## Implementation Difficulty

**Hard** - Lexer + Parser + Semantic analysis + Symbol table, ~3-4 weeks of work

## Overview of Implementation

The implementation requires:
1. **Lexer**: Add `=>` token
2. **Parser**: Extend argument lists to support `identifier => expression`
3. **Symbol table**: Store parameter names from function declarations
4. **Semantic analysis**: Match labels to parameters and reorder arguments
5. **Code generation**: Transform labeled calls to positional calls

## Files to Modify

### 1. cc/ccom/scan.l (Lexer)

**Add LABEL_ARROW token:**

```lex
/* Operators */
"=>"                    { return LABEL_ARROW; }
```

### 2. cc/ccom/cgram.y (Parser)

**Add token declaration:**

```yacc
%token LABEL_ARROW
```

**Modify argument expression list:**

Current grammar (standard C):
```yacc
argument_expr_list:
      assignment_expr
    | argument_expr_list ',' assignment_expr
    ;
```

Extended grammar with labeled arguments:
```yacc
argument_expr_list:
      argument_expr
    | argument_expr_list ',' argument_expr
    ;

argument_expr:
      assignment_expr
            {
                /* Positional argument */
                $$ = make_arg(NULL, $1);  /* label=NULL, expr */
            }
    | IDENTIFIER LABEL_ARROW assignment_expr
            {
                /* Labeled argument */
                $$ = make_arg($1, $3);    /* label, expr */
            }
    ;
```

**Primary expression modification:**

```yacc
postfix_expr:
      /* ... existing rules ... */
    | postfix_expr '(' argument_expr_list ')'
            {
                /* Function call */
                $$ = build_function_call($1, $3);
            }
    | postfix_expr '(' ')'
            {
                /* Function call with no arguments */
                $$ = build_function_call($1, NULL);
            }
    ;
```

### 3. cc/ccom/trees.c (AST Structures)

**Add labeled argument structure:**

```c
/* Argument node for function call */
typedef struct arg_node {
    char *label;           /* Parameter name (NULL for positional) */
    NODE *expr;            /* Argument expression */
    int position;          /* Final position after reordering */
    struct arg_node *next;
} ARG_NODE;

/*
 * Create argument node
 */
ARG_NODE *
make_arg(char *label, NODE *expr)
{
    ARG_NODE *arg = malloc(sizeof(ARG_NODE));
    arg->label = label ? strdup(label) : NULL;
    arg->expr = expr;
    arg->position = -1;  /* Not yet determined */
    arg->next = NULL;
    return arg;
}
```

### 4. cc/ccom/pftn.c (Function Declarations)

**Store parameter names in function type:**

```c
/* Add to function type structure */
struct func_type_info {
    TWORD return_type;
    int param_count;
    char **param_names;     /* NEW: Array of parameter names */
    TWORD *param_types;     /* Existing: Array of parameter types */
    /* ... other fields ... */
};

/*
 * Store parameter names when processing function declaration
 */
static void
store_param_names(NODE *func_decl, char **param_names, int count)
{
    struct func_type_info *fti = get_func_info(func_decl);
    fti->param_count = count;
    fti->param_names = malloc(count * sizeof(char *));

    for (int i = 0; i < count; i++) {
        fti->param_names[i] = param_names[i] ? strdup(param_names[i]) : NULL;
    }
}
```

**Function declaration processing:**

```c
/* When processing parameter list in function declaration */
static void
process_param_list(NODE *params)
{
    int param_count = 0;
    char **param_names = NULL;

    /* Walk parameter list */
    for (NODE *p = params; p != NULL; p = p->next) {
        if (p->name != NULL) {
            /* Resize array */
            param_names = realloc(param_names,
                                  (param_count + 1) * sizeof(char *));
            param_names[param_count] = strdup(p->name);
            param_count++;
        } else {
            /* Anonymous parameter - store NULL */
            param_names = realloc(param_names,
                                  (param_count + 1) * sizeof(char *));
            param_names[param_count] = NULL;
            param_count++;
        }
    }

    /* Store in function type */
    store_param_names(current_function, param_names, param_count);
}
```

### 5. cc/ccom/tree.c (Semantic Analysis)

**Match labeled arguments to parameters:**

```c
/*
 * Match labeled arguments to function parameters
 * Returns reordered argument list or NULL on error
 */
static ARG_NODE *
match_labeled_args(NODE *func, ARG_NODE *args)
{
    struct func_type_info *fti = get_func_info(func);

    if (fti == NULL || fti->param_names == NULL) {
        uerror("cannot use labeled arguments with function that has no parameter names");
        return NULL;
    }

    int param_count = fti->param_count;
    ARG_NODE **reordered = calloc(param_count, sizeof(ARG_NODE *));
    int *assigned = calloc(param_count, sizeof(int));
    int next_positional = 0;

    /* Process arguments */
    for (ARG_NODE *arg = args; arg != NULL; arg = arg->next) {
        if (arg->label == NULL) {
            /* Positional argument */
            if (next_positional >= param_count) {
                uerror("too many arguments to function");
                goto error;
            }

            if (assigned[next_positional]) {
                uerror("positional argument conflicts with labeled argument");
                goto error;
            }

            reordered[next_positional] = arg;
            assigned[next_positional] = 1;
            arg->position = next_positional;
            next_positional++;

        } else {
            /* Labeled argument - find parameter by name */
            int param_idx = find_param_by_name(fti, arg->label);

            if (param_idx < 0) {
                uerror("unknown parameter name '%s'", arg->label);
                goto error;
            }

            if (assigned[param_idx]) {
                uerror("duplicate argument for parameter '%s'", arg->label);
                goto error;
            }

            reordered[param_idx] = arg;
            assigned[param_idx] = 1;
            arg->position = param_idx;
        }
    }

    /* Check all parameters were provided */
    for (int i = 0; i < param_count; i++) {
        if (!assigned[i]) {
            if (fti->param_names[i]) {
                uerror("missing argument for parameter '%s'", fti->param_names[i]);
            } else {
                uerror("missing argument for parameter %d", i);
            }
            goto error;
        }
    }

    /* Build reordered list */
    ARG_NODE *result = reordered[0];
    for (int i = 1; i < param_count; i++) {
        reordered[i-1]->next = reordered[i];
    }
    reordered[param_count-1]->next = NULL;

    free(reordered);
    free(assigned);
    return result;

error:
    free(reordered);
    free(assigned);
    return NULL;
}

/*
 * Find parameter index by name
 * Returns index or -1 if not found
 */
static int
find_param_by_name(struct func_type_info *fti, const char *name)
{
    for (int i = 0; i < fti->param_count; i++) {
        if (fti->param_names[i] != NULL &&
            strcmp(fti->param_names[i], name) == 0) {
            return i;
        }
    }
    return -1;
}
```

**Build function call with labeled arguments:**

```c
/*
 * Build function call node
 * Handles both positional and labeled arguments
 */
NODE *
build_function_call(NODE *func, ARG_NODE *args)
{
    /* Check if any arguments are labeled */
    int has_labels = 0;
    for (ARG_NODE *arg = args; arg != NULL; arg = arg->next) {
        if (arg->label != NULL) {
            has_labels = 1;
            break;
        }
    }

    if (has_labels) {
        /* Reorder arguments to match function signature */
        args = match_labeled_args(func, args);
        if (args == NULL) {
            return error_node();  /* Error already reported */
        }
    }

    /* Type check arguments against parameters */
    check_argument_types(func, args);

    /* Build call node with reordered arguments */
    return make_call_node(func, args);
}
```

### 6. cc/ccom/inline.c (Inlining Support)

**Handle labeled arguments in inlined functions:**

```c
/*
 * When inlining a function called with labeled arguments,
 * the arguments are already reordered by match_labeled_args(),
 * so no special handling is needed during inlining.
 */
```

## Advanced Features

### Mix Positional and Labeled

**Rule:** All positional arguments must come before all labeled arguments.

```c
/* Valid */
foo(10, 20, z => 30);           /* x=10, y=20, z=30 */
foo(10, y => 20, z => 30);      /* x=10, y=20, z=30 */

/* Invalid */
foo(x => 10, 20);               /* Error: positional after labeled */
```

Implementation:

```c
static ARG_NODE *
validate_arg_order(ARG_NODE *args)
{
    int seen_labeled = 0;

    for (ARG_NODE *arg = args; arg != NULL; arg = arg->next) {
        if (arg->label != NULL) {
            seen_labeled = 1;
        } else if (seen_labeled) {
            uerror("positional argument cannot follow labeled argument");
            return NULL;
        }
    }

    return args;
}
```

### Default Arguments (Optional Extension)

Extend to support default values:

```c
void foo(int x = 10, int y = 20, int z = 30);

/* Call with some arguments omitted */
foo(x => 5);              /* x=5, y=20, z=30 (defaults) */
foo(z => 100);            /* x=10, y=20, z=100 */
```

This requires storing default values in the function type.

## Testing

### Test Cases

Create `test_labeled_args.c`:

```c
#include <stdio.h>

/* Simple function */
void print_point(int x, int y)
{
    printf("Point: (%d, %d)\n", x, y);
}

/* Function with many parameters */
void draw_rect(int x, int y, int width, int height, int color)
{
    printf("Rectangle: pos=(%d,%d) size=%dx%d color=0x%X\n",
           x, y, width, height, color);
}

/* Function with various types */
void create_window(const char *title, int width, int height,
                   int visible, int resizable)
{
    printf("Window: \"%s\" %dx%d visible=%d resizable=%d\n",
           title, width, height, visible, resizable);
}

int main(void)
{
    /* Test 1: All labeled arguments */
    printf("Test 1: All labeled\n");
    print_point(y => 20, x => 10);
    print_point(x => 5, y => 15);

    /* Test 2: Many parameters */
    printf("\nTest 2: Many parameters\n");
    draw_rect(
        width => 200,
        height => 100,
        x => 50,
        y => 75,
        color => 0xFF0000
    );

    /* Test 3: Mixed positional and labeled */
    printf("\nTest 3: Mixed positional and labeled\n");
    draw_rect(50, 75,           /* x, y positional */
              width => 200,
              height => 100,
              color => 0x00FF00);

    /* Test 4: Different types */
    printf("\nTest 4: Different types\n");
    create_window(
        title => "My App",
        width => 800,
        height => 600,
        visible => 1,
        resizable => 1
    );

    /* Test 5: Labeled in different order */
    printf("\nTest 5: Different order\n");
    create_window(
        resizable => 0,
        visible => 1,
        height => 480,
        width => 640,
        title => "Fixed Window"
    );

    return 0;
}
```

### Error Cases

Test file `test_labeled_errors.c`:

```c
void foo(int x, int y, int z);
void bar(int, int);  /* No parameter names */

void test_errors(void)
{
    /* Error: unknown parameter */
    foo(x => 1, y => 2, w => 3);

    /* Error: duplicate parameter */
    foo(x => 1, y => 2, x => 3);

    /* Error: missing parameter */
    foo(x => 1, y => 2);

    /* Error: positional after labeled */
    foo(x => 1, 2, z => 3);

    /* Error: no parameter names in declaration */
    bar(x => 1, y => 2);

    /* Error: conflict with positional */
    foo(1, 2, y => 3);  /* y is already 2nd argument */
}
```

### Build and Test

```bash
# Build modified PCC
cd cc/ccom
make clean
make

# Test labeled arguments
pcc test_labeled_args.c -o test_labeled_args
./test_labeled_args

# Test error cases
pcc -c test_labeled_errors.c
# Should produce appropriate error messages for each case
```

## Type Checking

Arguments must still type-check after reordering:

```c
void foo(int x, double y, const char *z);

/* Type checking after reordering */
foo(
    z => "hello",      /* const char * - OK */
    y => 3.14,         /* double - OK */
    x => 42            /* int - OK */
);

/* Type error */
foo(
    x => "hello",      /* Error: int expected, got const char * */
    y => 42,           /* OK: implicit int -> double conversion */
    z => 3.14          /* Error: const char * expected, got double */
);
```

## Function Pointers

Labeled arguments should work with function pointers:

```c
void (*func_ptr)(int x, int y);

func_ptr = print_point;
func_ptr(y => 20, x => 10);  /* Should work if pointer retains param names */
```

This requires function pointer types to include parameter names.

## Compatibility

### Old-Style Declarations

Handle functions without parameter names:

```c
/* Old style - no parameter names */
void foo(int, int, int);

/* Cannot use labeled arguments */
foo(x => 1, y => 2, z => 3);  /* Error: no parameter names */

/* Must use positional */
foo(1, 2, 3);  /* OK */
```

### Variadic Functions

Labeled arguments only for named parameters:

```c
void printf(const char *format, ...);

/* Can label 'format' but not variadic args */
printf(format => "Hello %d\n", 42);  /* OK? */
```

## Standards Compatibility

### K&R C

Not compatible with K&R function declarations:

```c
/* K&R style */
int foo(x, y)
int x;
int y;
{
    return x + y;
}

/* Should still work with positional args */
foo(1, 2);  /* OK */

/* Cannot use labeled args (no modern declaration) */
```

### ANSI C / C99 / C11

Full compatibility - labeled arguments are a pure extension.

## Performance Impact

**Zero runtime overhead** - Arguments are reordered at compile time. Generated code is identical to positional argument calls.

**Compile-time impact:**
- Extra symbol table lookups
- Argument reordering
- Minimal overhead (< 5% on function calls)

## Documentation

Update:
- Language reference - Document labeled argument syntax
- User manual - Show examples and best practices
- Coding style guide - When to use labeled arguments

## Related Features

Labeled arguments work well with:
1. **Default arguments** (future extension)
2. **Overloading** (if ever added to C)
3. **Named struct initialization** (similar to existing designated initializers)

## Estimated Effort

- **Lexer modifications:** 1-2 hours
- **Parser modifications:** 12-16 hours
- **Symbol table updates:** 16-24 hours (store param names)
- **Semantic analysis:** 24-32 hours (matching, reordering)
- **Type checking integration:** 8-12 hours
- **Testing:** 16-20 hours
- **Documentation:** 8-12 hours

**Total:** 3-4 weeks including thorough testing

## Summary

Labeled arguments are:
- **Complex to implement** (parser + semantic + symbol table)
- **Very high value** (self-documenting code, flexible APIs)
- **Zero runtime cost** (compile-time transformation)
- **MetaWare High C original** (1989, before Python!)

Recommended as **third syntax extension to implement** (after numeric separators and case ranges).

## Implementation Challenges

### Challenge 1: Function Pointers

Function pointer types need to store parameter names:

```c
/* Current */
typedef void (*func_t)(int, int);

/* Enhanced */
typedef void (*func_t)(int x, int y);

/* Usage */
func_t fp = foo;
fp(y => 20, x => 10);  /* Should work */
```

### Challenge 2: Forward Declarations

Parameter names must match across declarations:

```c
/* Declaration */
void foo(int x, int y);

/* Definition - same names */
void foo(int x, int y) { /* ... */ }  /* OK */

/* Definition - different names */
void foo(int a, int b) { /* ... */ }  /* Warning? Error? */
```

**Recommendation:** Warning if names differ, use declaration names for calls.

### Challenge 3: Header Files

Headers need parameter names:

```c
/* Old style header (no names) */
void foo(int, int);  /* Cannot use labeled args */

/* New style header (with names) */
void foo(int x, int y);  /* Can use labeled args */
```

**Migration:** Add parameter names to all function declarations in headers.

## Future Extensions

After basic labeled arguments:

1. **Default values:** `void foo(int x = 10, int y = 20)`
2. **Keyword-only args:** Force labeled usage after certain point
3. **Variadic labeled args:** Named variadic parameters

This makes the labeled arguments feature complete and comparable to Python/Swift.
