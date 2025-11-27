# Complete MetaWare High C Extensions - All Features

## Overview

This document lists **ALL** MetaWare High C compiler extensions discovered through research. MetaWare High C (1989) was decades ahead of its time with features that influenced modern languages.

---

## Complete Feature Matrix

| # | Feature | Year | Modern Equivalent | PCC Status | Type |
|---|---------|------|-------------------|------------|------|
| 1 | Nested Functions | 1989 | GCC extension | ✅ Runtime | Library |
| 2 | Generator Coroutines | 1989 | Python (2001), C++20 (2020) | ✅ Runtime | Library |
| 3 | Function Values/Closures | 1989 | C++11 lambdas (2011) | ✅ Runtime | Library |
| 4 | Non-Local Goto | 1989 | C++ exceptions | ✅ Runtime | Library |
| 5 | **Labeled/Named Arguments** | 1989 | Python kwargs, Swift | 📋 Documented | **Compiler** |
| 6 | **Numeric Literal Separators** | 1989 | C++14 (2014) | 📋 Documented | **Compiler** |
| 7 | **Case Ranges** | 1989 | GCC extension | 📋 Documented | **Compiler** |

---

## 1-4: Implemented with Runtime Library ✅

See [METAWARE_EXTENSIONS.md](METAWARE_EXTENSIONS.md) for complete documentation of:
- Nested functions (libmetaware)
- Generator coroutines (libmetaware)
- Function values/closures (libmetaware)
- Non-local goto (libmetaware)

---

## 5. Labeled/Named Arguments (Keyword Arguments)

### Description

MetaWare High C supported Python-style keyword arguments in **1989**, allowing arguments to be passed by name in any order.

**Historical Significance:** This feature appeared 12 years before Python had it widely adopted, and decades before Swift (2014) and other modern languages.

### Syntax (Requires Compiler Support)

```c
/* Function declaration - same as normal */
void drawRect(int x, int y, int width, int height, int color);

/* Call with labeled arguments - arguments can be in ANY order! */
drawRect(
    width => 200,
    height => 100,
    x => 50,
    y => 75,
    color => 0xFF0000
);

/* Can mix labeled and positional (labeled must come after positional) */
drawRect(50, 75,  /* positional x, y */
    color => 0xFF0000,
    width => 200,
    height => 100
);

/* Especially useful for functions with many parameters */
createWindow(
    title => "My Application",
    width => 800,
    height => 600,
    resizable => 1,
    visible => 1,
    x => 100,
    y => 100
);
```

### Benefits

**Readability:**
```c
/* Without labels - unclear what parameters mean */
createButton(100, 200, 80, 25, "Click Me", 1, 0x0000FF, NULL);

/* With labels - self-documenting */
createButton(
    x => 100,
    y => 200,
    width => 80,
    height => 25,
    text => "Click Me",
    enabled => 1,
    bgColor => 0x0000FF,
    callback => NULL
);
```

**Flexibility:**
```c
/* These are equivalent: */
setColor(red => 255, green => 128, blue => 0);
setColor(blue => 0, red => 255, green => 128);
setColor(green => 128, blue => 0, red => 255);
```

**Safety:**
```c
/* Reduces errors from argument order confusion */

/* Without labels - easy to swap width/height */
resize(window, 100, 200);  /* Is this width=100,height=200 or vice versa? */

/* With labels - crystal clear */
resize(window, width => 100, height => 200);
```

### Comparison with Modern Languages

```c
/* MetaWare High C (1989) */
func(x => 10, y => 20, z => 30);

/* Python (2000s) */
func(x=10, y=20, z=30)

/* Swift (2014) */
func(x: 10, y: 20, z: 30)

/* C++ - still doesn't have this in C++23! */
/* Workaround with designated initializers: */
struct Params { int x, y, z; };
func((Params){.x=10, .y=20, .z=30});
```

### Compiler Implementation Requirements

#### Lexer (scan.l)
```c
/* Add => token */
"=>"            { return ARROW_LABEL; }
```

#### Parser (cgram.y)
```yacc
/* Function call with labeled arguments */
argument_list
    : argument
    | argument_list ',' argument
    ;

argument
    : assignment_expr
    | IDENTIFIER ARROW_LABEL assignment_expr   /* labeled argument */
    ;
```

#### Semantic Analysis
1. **Collect parameter names** from function declaration
2. **Parse labeled arguments** in function call
3. **Match labels to parameters** by name
4. **Reorder arguments** to match function signature
5. **Type check** as normal
6. **Error handling:**
   - Unknown label name
   - Duplicate label
   - Label for parameter already provided positionally

#### Code Generation
```c
/* Transform labeled call to positional call */

// Original:
foo(y => 20, x => 10, z => 30);

// After transformation:
foo(10, 20, 30);  // Reordered to match declaration: foo(int x, int y, int z)
```

#### Example Transformation

```c
/* Source code */
void setPixel(int x, int y, int color);

int main(void) {
    setPixel(color => 0xFF0000, y => 100, x => 50);
    return 0;
}

/* After semantic analysis - reordered to match signature */
int main(void) {
    setPixel(50, 100, 0xFF0000);  /* x=50, y=100, color=0xFF0000 */
    return 0;
}
```

---

## 6. Numeric Literal Separators

### Description

Allow underscores in numeric literals for readability, **25 years before C++14**.

### Syntax (Requires Compiler Support)

```c
/* Integer literals */
int million = 1_000_000;
int billion = 1_000_000_000;

/* Hexadecimal */
unsigned int color = 0xFF_00_FF;    /* RGB color */
unsigned int addr = 0xDEAD_BEEF;

/* Binary (if supported) */
unsigned char flags = 0b1111_0000_1111_0000;
unsigned char mask =  0b1010_1010;

/* Floating-point */
double pi = 3.141_592_653_589_793;
double avogadro = 6.022_140_76e23;
double planck = 6.626_070_15e-34;

/* Grouping for clarity */
long long credit_card = 1234_5678_9012_3456;  /* Card number */
unsigned int phone = 555_123_4567;            /* Phone number */
unsigned int ssn = 123_45_6789;               /* SSN format */

/* Byte groupings in hex */
unsigned int ipv6_part = 0x2001_0db8;
unsigned char bytes[] = { 0xDE, 0xAD, 0xBE, 0xEF };
```

### Rules

1. **Underscores allowed** between digits
2. **Not allowed:**
   - At start: `_123` ❌
   - At end: `123_` ❌
   - Consecutive: `1__000` ❌
   - After base prefix: `0x_FF` ❌

3. **Ignored by compiler** - purely for readability

### Comparison

```c
/* MetaWare High C (1989) */
int x = 1_000_000;

/* C++14 (2014) - 25 years later! */
int x = 1'000'000;  // Different separator character

/* C - still not standard in C23! */
/* Workaround: */
#define MILLION 1000000
```

### Compiler Implementation

#### Lexer Modifications (scan.l)

```c
/* Current (standard C) */
DIGIT          [0-9]
DIGITS         {DIGIT}+

/* Modified for separators */
DIGIT          [0-9]
DIGIT_SEQ      {DIGIT}("_"?{DIGIT})*

HEX_DIGIT      [0-9A-Fa-f]
HEX_SEQ        {HEX_DIGIT}("_"?{HEX_DIGIT})*

/* Integer literals */
0[xX]{HEX_SEQ}              { return process_hex_literal(yytext); }
0[bB][01]("_"?[01])*        { return process_bin_literal(yytext); }
{DIGIT_SEQ}                 { return process_dec_literal(yytext); }

/* Floating-point literals */
{DIGIT_SEQ}"."{DIGIT_SEQ}([eE][+-]?{DIGIT_SEQ})?  { return process_float(yytext); }
```

#### Processing Function

```c
long long process_dec_literal(const char *text) {
    char *stripped = malloc(strlen(text) + 1);
    char *p = stripped;

    /* Strip underscores */
    for (const char *s = text; *s; s++) {
        if (*s != '_') {
            *p++ = *s;
        }
    }
    *p = '\0';

    /* Convert to integer */
    long long value = strtoll(stripped, NULL, 10);
    free(stripped);

    return value;
}
```

#### Validation

```c
bool validate_numeric_separator(const char *text) {
    size_t len = strlen(text);

    /* Can't start or end with underscore */
    if (text[0] == '_' || text[len-1] == '_') {
        return false;
    }

    /* Can't have consecutive underscores */
    for (size_t i = 0; i < len - 1; i++) {
        if (text[i] == '_' && text[i+1] == '_') {
            return false;
        }
    }

    /* Can't immediately follow base prefix */
    if (len >= 3 && text[0] == '0') {
        if ((text[1] == 'x' || text[1] == 'X' ||
             text[1] == 'b' || text[1] == 'B') && text[2] == '_') {
            return false;
        }
    }

    return true;
}
```

---

## 7. Case Ranges in Switch Statements

### Description

Allow range syntax in case labels for more concise switch statements. Similar to GCC extension but available in 1989.

### Syntax (Requires Compiler Support)

```c
/* Basic range */
switch (c) {
    case 'a' ... 'z':  /* lowercase letters */
        handle_lowercase();
        break;

    case 'A' ... 'Z':  /* uppercase letters */
        handle_uppercase();
        break;

    case '0' ... '9':  /* digits */
        handle_digit();
        break;

    default:
        handle_other();
}

/* Integer ranges */
switch (grade) {
    case 90 ... 100:
        printf("A\n");
        break;

    case 80 ... 89:
        printf("B\n");
        break;

    case 70 ... 79:
        printf("C\n");
        break;

    case 60 ... 69:
        printf("D\n");
        break;

    case 0 ... 59:
        printf("F\n");
        break;
}

/* Enum ranges (if consecutive) */
enum Color { RED, ORANGE, YELLOW, GREEN, BLUE, PURPLE };

switch (color) {
    case RED ... YELLOW:    /* Warm colors */
        set_warm_palette();
        break;

    case GREEN ... PURPLE:  /* Cool colors */
        set_cool_palette();
        break;
}
```

### Benefits

**Before (standard C):**
```c
switch (c) {
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
        handle_lowercase();
        break;
}
```

**After (with ranges):**
```c
switch (c) {
    case 'a' ... 'z':
        handle_lowercase();
        break;
}
```

**Much more concise and readable!**

### Comparison with GCC

```c
/* MetaWare High C (1989) and GCC extension */
case 1 ... 10:

/* GCC also supports but MetaWare was first */
```

### Compiler Implementation

#### Lexer (scan.l)

```c
/* Add ... token for ranges */
"..."           { return ELLIPSIS; }
```

#### Parser (cgram.y)

```yacc
/* Modify case label to allow ranges */
case_label
    : CASE constant_expr ':'
    | CASE constant_expr ELLIPSIS constant_expr ':'  /* range */
    ;
```

#### Semantic Analysis

```c
/* Validate range */
void check_case_range(int start, int end) {
    if (start > end) {
        error("Case range start must be <= end");
    }

    if (end - start > MAX_CASE_RANGE) {
        warning("Very large case range may be inefficient");
    }
}
```

#### Code Generation

**Option 1: Expand to multiple cases (simple)**
```c
/* Source */
case 1 ... 5:
    action();
    break;

/* Expanded internally */
case 1:
case 2:
case 3:
case 4:
case 5:
    action();
    break;
```

**Option 2: Generate range check (efficient for large ranges)**
```c
/* Source */
case 1 ... 1000:
    action();
    break;

/* Generated code */
if (value >= 1 && value <= 1000) {
    action();
}
```

**Optimization:** Choose based on range size
- Small range (<= 10): Expand to individual cases
- Large range (> 10): Generate range check

---

## Implementation Status Summary

### ✅ Fully Implemented (Runtime Library)

**libmetaware** provides:
1. Nested functions (trampolines)
2. Generator coroutines (setjmp/longjmp)
3. Function values (closures)
4. Non-local goto (stack unwinding)

**Usage:**
```bash
pcc program.c -lmetaware -o program
```

### 🔧 Preprocessor-Based Workarounds (Available Now!)

**libmetaware/metaware_syntax.h** provides macro-based approximations for:
5. Labeled/named arguments (using C99 designated initializers)
6. Numeric literal separators (predefined constants and macros)
7. Case ranges (macro-generated case labels)

**Usage:**
```c
#include <metaware_syntax.h>

/* Labeled arguments */
DECLARE_LABELED_FUNC(drawRect, int x; int y; int width; int height;)
DEFINE_LABELED_FUNC(drawRect) { /* implementation */ }
CALL(drawRect, .width = 200, .height = 100, .x = 50, .y = 75);

/* Numeric separators */
int buffer = 8 * _1K;        /* 8 KB */
int max_size = 100 * _1M;    /* 100 MB */
int color = HEX8(FF, AA, 00, 00);

/* Case ranges */
switch (c) {
    CASE_LOWERCASE:  /* a-z */
        handle_lowercase();
        break;
    CASE_UPPERCASE:  /* A-Z */
        handle_uppercase();
        break;
    CASE_DIGIT:      /* 0-9 */
        handle_digit();
        break;
}
```

**Examples:**
- `libmetaware/examples/labeled_args_example.c`
- `libmetaware/examples/numeric_separators_example.c`
- `libmetaware/examples/case_ranges_example.c`
- `libmetaware/examples/combined_syntax_example.c`
- `test_metaware_syntax.c`

**Limitations:**
- Labeled arguments require wrapper macros (not true syntax)
- Numeric separators are predefined constants only
- Case ranges limited to predefined character classes
- See [Workarounds vs True Syntax](#workarounds-vs-true-syntax) below

### 📋 Fully Documented (Needs Compiler for True Syntax)

**Syntax features** require PCC frontend modifications for full support:
5. Labeled/named arguments (`arg => value`)
6. Numeric literal separators (`1_000_000`)
7. Case ranges (`case 1 ... 10:`)

**Implementation effort:** ~2-4 weeks per feature

**Priority recommendation:**
1. **Numeric separators** (2 weeks) - Easy, high value
2. **Case ranges** (2 weeks) - Medium difficulty
3. **Labeled arguments** (4 weeks) - Complex but powerful

---

## Complete Example Using All Features

```c
#include <metaware.h>
#include <stdio.h>

/* Labeled arguments would make this call clearer */
void drawRect(int x, int y, int width, int height, int color);

/* Generator with numeric separators */
MW_GENERATOR(range, int, int) {
    int count = *(int *)__args;
    for (int i = 0; i < count; i++) {
        MW_YIELD(__gen, i);
    }
}

int main(void) {
    /* With compiler support, these would work: */

    // drawRect(
    //     x => 100,
    //     y => 200,
    //     width => 300,
    //     height => 150,
    //     color => 0xFF_00_00  /* Red with separator */
    // );

    /* Generator works NOW with libmetaware */
    mw_generator_t *gen = range(1_000_000);  /* Would need separators */

    int value;
    int count = 0;
    MW_FOR_EACH(value, gen) {
        count++;

        /* Case ranges would work with compiler support */
        switch (value % 100) {
            case 0 ... 25:
                /* First quarter */
                break;
            case 26 ... 50:
                /* Second quarter */
                break;
            case 51 ... 75:
                /* Third quarter */
                break;
            case 76 ... 99:
                /* Fourth quarter */
                break;
        }

        if (count >= 10) break;  /* Just demo */
    }

    mw_free_generator(gen);
    return 0;
}
```

---

## Workarounds vs True Syntax

### What Can Be Done Now (Preprocessor Workarounds)

The `metaware_syntax.h` header provides macro-based approximations that work **today** with standard C99 compilers:

#### Labeled Arguments Workaround

**True MetaWare Syntax (requires compiler):**
```c
void drawRect(int x, int y, int width, int height, int color);

drawRect(
    width => 200,
    height => 100,
    x => 50,
    y => 75,
    color => 0xFF0000
);
```

**Workaround (works now with C99):**
```c
DECLARE_LABELED_FUNC(drawRect,
    int x;
    int y;
    int width;
    int height;
    int color;
)

DEFINE_LABELED_FUNC(drawRect) {
    printf("Drawing at (%d,%d) size %dx%d\n",
           ARGS->x, ARGS->y, ARGS->width, ARGS->height);
}

CALL(drawRect,
    .width = 200,
    .height = 100,
    .x = 50,
    .y = 75,
    .color = 0xFF0000
);
```

**How it works:** Uses C99 designated initializers to create a struct with named fields, then passes the struct to a wrapper function.

**Limitations:**
- Requires wrapper macros
- Can't mix with regular function declarations
- Extra indirection (minor performance cost)
- Type checking at compile-time but different error messages

#### Numeric Separators Workaround

**True MetaWare Syntax (requires compiler):**
```c
int population = 7_800_000_000;
int color = 0xFF_AA_00;
int binary = 0b1010_1010_1111_0000;
```

**Workaround (works now):**
```c
int population = 7 * _1B + 800 * _1M;
int color = HEX8(FF, AA, 00, 00);
int binary = _BIN_10101010;  /* predefined only */
```

**Predefined constants:**
- `_1K`, `_10K`, `_100K`, `_1M`, `_10M`, `_100M`, `_1B`, `_1T`
- `HEX2(a,b)`, `HEX4(a,b,c,d)`, `HEX8(a,b,c,d,e,f,g,h)`
- `_BIN_0000` through `_BIN_1111`, common byte patterns

**Limitations:**
- Not true separators in arbitrary positions
- Limited to predefined constants
- Binary patterns limited to common values
- No floating-point support

#### Case Ranges Workaround

**True MetaWare Syntax (requires compiler):**
```c
switch (c) {
    case 'a' ... 'z':
        handle_lowercase();
        break;
    case 0 ... 100:
        handle_range();
        break;
}
```

**Workaround (works now):**
```c
switch (c) {
    CASE_LOWERCASE:  /* expands to: case 'a': case 'b': ... case 'z': */
        handle_lowercase();
        break;
}

/* For numeric ranges, use RANGE_CASE macro (if-else style) */
RANGE_CASE(score, 0, 100) {
    handle_range();
}
```

**Predefined ranges:**
- `CASE_LOWERCASE` (a-z)
- `CASE_UPPERCASE` (A-Z)
- `CASE_DIGIT` (0-9)
- `CASE_ALPHA` (a-z, A-Z)
- `CASE_ALNUM` (a-z, A-Z, 0-9)
- `CASE_RANGE_2(start)` through `CASE_RANGE_26(start)`

**Limitations:**
- Expands to many case labels (larger code)
- Limited to predefined ranges or manual use of CASE_RANGE_n
- Can't use arbitrary expressions
- `RANGE_CASE` uses if-else, not true switch

### What Requires Compiler Changes (True Syntax)

For the **full MetaWare experience**, PCC frontend modifications are needed:

1. **Lexer changes** (scan.l):
   - Add `=>` token for labeled arguments
   - Modify number lexing to allow/strip underscores
   - Add `...` token for case ranges (if not already present)

2. **Parser changes** (cgram.y):
   - Extend argument lists to support `identifier => expression`
   - Extend case labels to support `case expr ... expr:`
   - No parser changes needed for numeric separators (lexer handles it)

3. **Semantic analysis**:
   - Match labeled arguments to parameter names
   - Reorder arguments to match function signature
   - Validate case range bounds
   - Strip underscores from numeric literals

4. **Code generation**:
   - Transform labeled calls to positional calls
   - Expand or optimize case ranges
   - Numeric separators already handled by lexer

### Migration Path

**Phase 1 (Now):** Use preprocessor workarounds
```c
#include <metaware_syntax.h>
/* Use DECLARE_LABELED_FUNC, CALL, _1M, CASE_LOWERCASE, etc. */
```

**Phase 2 (Compiler patches available):**
```c
/* Apply patches to PCC scanner/parser */
/* Use true MetaWare syntax */
drawRect(width => 200, height => 100);
int x = 1_000_000;
case 'a' ... 'z':
```

**Phase 3 (Future):** Standardization
```c
/* Features eventually standardized in C2x/C3x */
/* (Some already in C++14, C++20, etc.) */
```

### Testing

**Test the workarounds now:**
```bash
# Compile and run syntax workaround tests
pcc -I. test_metaware_syntax.c -o test_metaware_syntax
./test_metaware_syntax

# Run example programs
cd libmetaware/examples
make
make test
```

**When compiler support is added:**
```bash
# Run full syntax tests
pcc -fmetaware-extensions test_metaware_full.c -o test_metaware_full
./test_metaware_full
```

---

## References

1. **"The lost language extensions of MetaWare's High C Compiler"**
   - https://duriansoftware.com/joe/the-lost-language-extensions-of-metaware's-high-c-compiler

2. **MetaWare High C Programmer's Guide (1985)**
   - http://www.bitsavers.org/pdf/metaware/

3. **Comparison with Modern Languages:**
   - Python keyword arguments (2000)
   - C++14 digit separators (2014)
   - GCC case ranges extension
   - Swift labeled parameters (2014)

---

**Document Version:** 1.0
**Last Updated:** 2025
**Status:** Complete feature list with honest implementation status

**MetaWare High C was 12-35 years ahead of its time!**
