# Phase 5: C++ Exception Handling Implementation Plan

**Date:** 2025-10-26
**Status:** 🚧 PLANNING

---

## Overview

Implement C++ exception handling (try/catch/throw) for PCC's C++ compiler by integrating with the libseh (Structured Exception Handling) runtime library.

### Goals

1. Support try/catch/throw C++ exception syntax
2. Integrate with existing RAII destructor calling
3. Enable stack unwinding with automatic destructor invocation
4. Provide C++ exception type matching in catch blocks
5. Support exception re-throwing
6. Interoperate with SEH library for cross-platform support

---

## Prerequisites

### Completed:
✅ Phase 1: Class declarations and member functions
✅ Phase 2: 'this' pointer implementation
✅ Phase 3a: Constructor auto-invocation
✅ Phase 3b: RAII destructor auto-invocation
✅ Phase 4: ABI integration for name mangling

### Available:
✅ libseh - SEH runtime library with C++ interoperability
✅ RAII infrastructure (dtor_stack)
✅ ABI library for type information

---

## Architecture Overview

### Components

```
┌─────────────────────────────────────────────────┐
│           C++ Exception Handling                │
├─────────────────────────────────────────────────┤
│                                                 │
│  Parser (cgram.y, scan.l)                      │
│    ├─ try/catch/throw keywords                 │
│    ├─ Exception specifiers                     │
│    └─ Grammar rules                            │
│                                                 │
│  Code Generation (cxxcode.c)                   │
│    ├─ Try block setup (_seh_register)         │
│    ├─ Catch block matching (type info)        │
│    ├─ Throw implementation (_seh_raise)       │
│    └─ Destructor integration                   │
│                                                 │
│  Runtime (libseh)                              │
│    ├─ Exception frames                         │
│    ├─ DWARF unwinding                          │
│    ├─ Signal handlers                          │
│    └─ C++ exception objects                    │
│                                                 │
└─────────────────────────────────────────────────┘
```

---

## Implementation Phases

### Phase 5.1: Lexer and Parser Support

**Goal:** Add syntax support for try/catch/throw

#### Lexer Changes (scan.l)

Add keywords:
```c
"try"      { return(CXX_TRY); }
"catch"    { return(CXX_CATCH); }
"throw"    { return(CXX_THROW); }
```

#### Parser Changes (cgram.y)

Add tokens:
```yacc
%token CXX_TRY CXX_CATCH CXX_THROW
```

Add grammar rules:
```yacc
/* Try-catch statement */
try_block:
    CXX_TRY compound_statement handler_seq
    ;

handler_seq:
    handler
    | handler_seq handler
    ;

handler:
    CXX_CATCH '(' exception_declaration ')' compound_statement
    ;

exception_declaration:
    type_specifier_seq declarator
    | type_specifier_seq abstract_declarator
    | type_specifier_seq
    | ELLIPSIS                   /* catch(...) */
    ;

throw_expression:
    CXX_THROW
    | CXX_THROW assignment_expression
    ;
```

#### Files to Modify:
- cc/cxxcom/scan.l
- cc/cxxcom/cgram.y

---

### Phase 5.2: Code Generation Infrastructure

**Goal:** Generate code to set up exception handling frames

#### Try Block Generation

```c
/*
 * Generate code for try block:
 *
 * {
 *     struct _seh_registration reg;
 *     SEH_PROLOG(reg, handler_func, filter_func);
 *
 *     if (setjmp(reg.jmpbuf) == 0) {
 *         // Try block body
 *     } else {
 *         // Exception occurred - jump to catch
 *     }
 *
 *     SEH_EPILOG(reg);
 * }
 */

NODE *cxxtry(NODE *try_body, NODE *catch_list);
```

#### Catch Block Generation

```c
/*
 * Generate code for catch blocks:
 *
 * Each catch block needs:
 * 1. Type matching code
 * 2. Exception object extraction
 * 3. Handler body execution
 */

NODE *cxxcatch(NODE *exception_decl, NODE *handler_body);
```

#### Throw Statement Generation

```c
/*
 * Generate code for throw:
 *
 * throw expr;
 *   → Allocate exception object
 *   → Call _seh_raise_exception()
 *
 * throw;
 *   → Re-throw current exception
 */

NODE *cxxthrow(NODE *expr);
```

#### Exception Type Information

```c
/*
 * Generate RTTI (Runtime Type Information) for exception matching:
 *
 * struct _cxx_type_info {
 *     const char *type_name;
 *     struct symtab *type_symbol;
 *     int is_pointer;
 *     int is_reference;
 * };
 */

struct symtab *cxxgentype_info(TWORD type);
```

#### Files to Modify:
- cc/cxxcom/cxxcode.c
- cc/cxxcom/cxxdefs.h

---

### Phase 5.3: RAII Integration

**Goal:** Ensure destructors are called during stack unwinding

#### Destructor Unwinding

The existing `cxxcall_dtors(int level)` function needs to be called during exception unwinding.

```c
/*
 * Enhanced exception handler that calls destructors:
 *
 * int exception_handler(struct _seh_registration *reg,
 *                       struct _seh_exception_record *exc,
 *                       void *context)
 * {
 *     // Call destructors for current scope
 *     cxxcall_dtors(current_blevel);
 *
 *     // Check if this handler matches
 *     if (matches_catch_type(exc, catch_type)) {
 *         // Extract exception object
 *         // Jump to handler
 *         return EXCEPTION_EXECUTE_HANDLER;
 *     }
 *
 *     return EXCEPTION_CONTINUE_SEARCH;
 * }
 */
```

#### Integration Points:

1. **Try block entry:**
   - Set up exception frame with handler
   - Handler knows current block level

2. **Exception occurs:**
   - SEH walks exception chain
   - Calls handler for each try block
   - Handler calls `cxxcall_dtors(blevel)` before matching

3. **Catch block entry:**
   - Destructors for try block already called
   - Exception object available
   - Execute handler body

4. **Catch block exit:**
   - Normal scope cleanup
   - Call destructors for catch block scope

#### Files to Modify:
- cc/cxxcom/cxxcode.c (exception handler generation)
- Integration with existing dtor_stack

---

### Phase 5.4: SEH Library Integration

**Goal:** Build and link libseh, use runtime functions

#### Building libseh

```bash
cd libseh
make CC=gcc  # Build with gcc initially
make install PREFIX=/usr/local
```

#### Linking Changes

Modify Makefile to link libseh:

```makefile
# cc/cxxcom/Makefile
LIBS = ../../common/abi/libpccabi.a -lseh

# Or direct path:
LIBS = ../../common/abi/libpccabi.a ../../libseh/libseh.a
```

#### Runtime Function Usage

Functions needed from libseh:

```c
/* Exception frame management */
void _seh_register(struct _seh_registration *reg, void *handler, void *filter);
void _seh_unregister(struct _seh_registration *reg);

/* Exception raising */
void _seh_raise_exception(unsigned long code, unsigned long flags,
                          unsigned long nparams, unsigned long *params);

/* Exception information */
unsigned long _seh_get_exception_code(void);
struct _seh_exception_pointers *_seh_get_exception_info(void);

/* C++ interop */
int _seh_is_cxx_exception(void);
void *_seh_get_cxx_exception(void);
void _seh_translate_cxx_exception(void *cxx_exception);
```

#### Files to Modify:
- cc/cxxcom/Makefile
- cc/cxxcom/cxxdefs.h (include seh.h)
- cc/cxxcom/main.c (initialization)

---

### Phase 5.5: Exception Object Management

**Goal:** Allocate and manage C++ exception objects

#### Exception Object Structure

```c
struct _cxx_exception {
    void *exception_object;          /* User exception object */
    struct _cxx_type_info *type;     /* Type information */
    void (*destructor)(void *);       /* Exception object destructor */
    int reference_count;              /* For exception re-throwing */
};
```

#### Throw Implementation

```c
/*
 * throw MyException(args);
 *
 * Generates:
 * 1. Allocate exception object
 * 2. Call constructor
 * 3. Create type info
 * 4. Call _seh_raise_exception()
 */

NODE *cxxthrow_impl(NODE *expr) {
    // Allocate exception object on heap
    // Call constructor
    // Create _cxx_exception wrapper
    // Set up type info
    // Call _seh_raise_exception(EXCEPTION_CXX_EXCEPTION, ...)
}
```

#### Catch Implementation

```c
/*
 * catch (MyException &e)
 *
 * Generates:
 * 1. Get exception object from SEH
 * 2. Check type match
 * 3. Bind reference/copy
 * 4. Execute handler
 * 5. Destroy exception object if last handler
 */

NODE *cxxcatch_impl(NODE *type, NODE *name, NODE *body) {
    // Extract exception object
    // Type matching
    // Bind to catch variable
    // Execute body
}
```

#### Files to Modify:
- cc/cxxcom/cxxcode.c
- cc/cxxcom/cxxdefs.h

---

## Detailed Implementation Steps

### Step 1: Add Keywords (scan.l)

Location: cc/cxxcom/scan.l, around line 103-130

```c
"try"      { return(CXX_TRY); }
"catch"    { return(CXX_CATCH); }
"throw"    { return(CXX_THROW); }
```

### Step 2: Define Tokens (cgram.y)

Location: cc/cxxcom/cgram.y, token declarations section

```yacc
%token CXX_TRY CXX_CATCH CXX_THROW
```

### Step 3: Add Grammar Rules (cgram.y)

Add to statement rules:

```yacc
statement:
    /* existing rules */
    | try_block
    ;

try_block:
    CXX_TRY compound_statement handler_seq
    ;

handler_seq:
    handler
    | handler_seq handler
    ;

handler:
    CXX_CATCH '(' exception_declaration ')' compound_statement
    {
        $$ = cxxcatch($3, $5);
    }
    ;

exception_declaration:
    declaration_specifiers declarator
    {
        $$ = cxxexception_decl($1, $2);
    }
    | ELLIPSIS
    {
        $$ = NULL;  /* catch(...) */
    }
    ;
```

Add to expression rules:

```yacc
primary_expression:
    /* existing rules */
    | throw_expression
    ;

throw_expression:
    CXX_THROW
    {
        $$ = cxxthrow(NULL);  /* Re-throw */
    }
    | CXX_THROW assignment_expression
    {
        $$ = cxxthrow($2);
    }
    ;
```

### Step 4: Implement Code Generation Functions

In cc/cxxcom/cxxcode.c:

```c
/*
 * Generate try block code
 */
NODE *
cxxtry(NODE *try_body, NODE *catch_list)
{
    NODE *reg_var, *setup, *body, *cleanup, *result;

    /* Create local variable for SEH registration */
    reg_var = cxxcreate_seh_reg();

    /* Generate registration code */
    setup = cxxgen_seh_register(reg_var);

    /* Generate setjmp and conditional */
    body = cxxgen_try_body(reg_var, try_body, catch_list);

    /* Generate unregistration code */
    cleanup = cxxgen_seh_unregister(reg_var);

    /* Combine into block */
    result = cxxmake_try_block(setup, body, cleanup);

    return result;
}

/*
 * Generate catch block code
 */
NODE *
cxxcatch(NODE *exception_decl, NODE *handler_body)
{
    NODE *type_check, *binding, *body;

    /* Generate type matching code */
    type_check = cxxgen_type_match(exception_decl);

    /* Generate exception object binding */
    binding = cxxgen_exception_bind(exception_decl);

    /* Combine with handler body */
    body = cxxmake_catch_block(type_check, binding, handler_body);

    return body;
}

/*
 * Generate throw statement code
 */
NODE *
cxxthrow(NODE *expr)
{
    NODE *alloc, *construct, *raise;

    if (expr == NULL) {
        /* Re-throw current exception */
        raise = cxxgen_rethrow();
    } else {
        /* Allocate exception object */
        alloc = cxxgen_exception_alloc(expr);

        /* Construct exception */
        construct = cxxgen_exception_construct(alloc, expr);

        /* Raise exception */
        raise = cxxgen_exception_raise(construct);
    }

    return raise;
}
```

### Step 5: Function Prototypes (cxxdefs.h)

```c
/* Exception handling */
NODE *cxxtry(NODE *try_body, NODE *catch_list);
NODE *cxxcatch(NODE *exception_decl, NODE *handler_body);
NODE *cxxthrow(NODE *expr);
NODE *cxxexception_decl(NODE *spec, NODE *decl);

/* Exception helpers */
NODE *cxxcreate_seh_reg(void);
NODE *cxxgen_seh_register(NODE *reg);
NODE *cxxgen_seh_unregister(NODE *reg);
NODE *cxxgen_type_match(NODE *exception_type);
NODE *cxxgen_exception_bind(NODE *decl);
NODE *cxxgen_exception_alloc(NODE *expr);
NODE *cxxgen_exception_construct(NODE *alloc, NODE *expr);
NODE *cxxgen_exception_raise(NODE *exc);
NODE *cxxgen_rethrow(void);
```

---

## Testing Strategy

### Test 1: Basic Try-Catch

```cpp
#include <iostream>

int main() {
    try {
        throw 42;
    } catch (int e) {
        std::cout << "Caught: " << e << std::endl;
    }
    return 0;
}
```

### Test 2: Multiple Catch Blocks

```cpp
int main() {
    try {
        throw "error";
    } catch (int e) {
        // Not matched
    } catch (const char *e) {
        // Matched
    }
    return 0;
}
```

### Test 3: RAII with Exceptions

```cpp
class Resource {
public:
    Resource() { /* acquire */ }
    ~Resource() { /* release */ }
};

int main() {
    try {
        Resource r;
        throw 42;
        // Destructor should be called here
    } catch (int e) {
        // Handle
    }
    return 0;
}
```

### Test 4: Re-throw

```cpp
int main() {
    try {
        try {
            throw 42;
        } catch (...) {
            throw;  // Re-throw
        }
    } catch (int e) {
        // Caught re-thrown exception
    }
    return 0;
}
```

### Test 5: Catch-All

```cpp
int main() {
    try {
        throw SomeClass();
    } catch (...) {
        // Catch any exception
    }
    return 0;
}
```

---

## Milestones

### Milestone 1: Basic Syntax Support
- ✅ Add keywords to lexer
- ✅ Add grammar rules
- ✅ Parse try/catch/throw without errors
- **Target:** Parsing complete

### Milestone 2: Simple Code Generation
- ✅ Generate try block frame setup
- ✅ Generate throw for built-in types
- ✅ Generate catch for built-in types
- **Target:** Simple int throw/catch works

### Milestone 3: RAII Integration
- ✅ Call destructors during unwinding
- ✅ Test with class objects
- **Target:** Destructors called correctly

### Milestone 4: Full Implementation
- ✅ Class exception objects
- ✅ Exception type matching
- ✅ Multiple catch blocks
- ✅ Catch-all handler
- ✅ Re-throw support
- **Target:** Complete exception handling

---

## Dependencies

### Compiler Side:
- scan.l - Lexer
- cgram.y - Parser
- cxxcode.c - Code generation
- cxxdefs.h - Declarations

### Runtime Side:
- libseh - Exception runtime
- libpccabi - Type information
- Standard C library (malloc, etc.)

### Build System:
- Makefile - Link libseh

---

## Challenges and Solutions

### Challenge 1: Type Matching

**Problem:** C++ exception type matching requires RTTI

**Solution:**
- Use ABI library type information
- Generate type descriptors for exception types
- Compare type names/symbols in catch blocks

### Challenge 2: Exception Object Lifetime

**Problem:** Exception objects must survive stack unwinding

**Solution:**
- Allocate exception objects on heap
- Reference count for re-throwing
- Destroy after last handler

### Challenge 3: Destructor Calling Order

**Problem:** Destructors must be called during unwinding

**Solution:**
- Integrate with existing dtor_stack
- Call `cxxcall_dtors()` in exception handler
- Ensure correct block level tracking

### Challenge 4: Nested Try Blocks

**Problem:** Multiple try blocks can be active

**Solution:**
- SEH registration chain handles nesting
- Each try block has own frame
- Unwinder walks chain correctly

---

## Future Enhancements

### Not in Phase 5:

1. **Exception Specifications**
   - throw() specifier
   - noexcept specifier
   - Dynamic exception specifications

2. **std::exception Hierarchy**
   - Requires standard library
   - Can be added later

3. **Exception Safety Guarantees**
   - Basic, strong, no-throw
   - Requires careful design

4. **Performance Optimizations**
   - Zero-cost exception handling
   - Table-driven unwinding
   - Requires more advanced implementation

---

## Documentation Deliverables

1. **PHASE5_EXCEPTION_HANDLING.md** - Implementation details
2. **EXCEPTION_HANDLING_DESIGN.md** - Architecture and design
3. **EXCEPTION_HANDLING_TESTS.md** - Test cases and results
4. **EXCEPTION_HANDLING_API.md** - API reference

---

## Timeline Estimate

- **Phase 5.1:** 2-3 hours (syntax)
- **Phase 5.2:** 4-6 hours (code generation)
- **Phase 5.3:** 2-3 hours (RAII integration)
- **Phase 5.4:** 1-2 hours (SEH linking)
- **Phase 5.5:** 3-4 hours (exception objects)
- **Testing:** 2-3 hours
- **Documentation:** 2-3 hours

**Total Estimate:** 16-24 hours

---

## Success Criteria

✅ Parser accepts try/catch/throw syntax
✅ Code compiles without errors
✅ Simple throw/catch works for built-in types
✅ Class exceptions work
✅ RAII destructors called during unwinding
✅ Multiple catch blocks work
✅ Catch-all (...) works
✅ Re-throw works
✅ Nested try blocks work
✅ Tests pass
✅ Documentation complete

---

## Notes

- This is a substantial feature requiring careful implementation
- Start with simple cases, build up complexity
- Test incrementally at each step
- Document design decisions
- Consider performance implications
- Ensure compatibility with RAII work

---

**Status:** Ready to begin implementation
**Next Step:** Phase 5.1 - Add keywords and grammar rules
