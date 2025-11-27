# Compiler Patch: Case Ranges in Switch Statements

## Feature Description

Add support for range syntax in `case` labels for cleaner switch statements:
```c
case 'a' ... 'z':    /* lowercase letters */
case 0 ... 100:      /* numbers 0 through 100 */
case RED ... BLUE:   /* enum range */
```

This feature was in MetaWare High C (1989) and is also a GCC extension.

## Implementation Difficulty

**Medium** - Lexer + Parser + Code generation, ~2-3 weeks of work

## Files to Modify

### 1. cc/ccom/scan.l (Lexer)

**Add ELLIPSIS token** (if not already present for variadic functions):

```lex
/* Operators */
"..."                   { return ELLIPSIS; }
```

**Note:** If `...` is already tokenized for variadic functions, verify it's defined as `ELLIPSIS` or similar. If it's a different token (like `VA_DCL`), you may need to disambiguate in the parser.

### 2. cc/ccom/cgram.y (Parser)

**Add token declaration:**

```yacc
%token ELLIPSIS
```

**Modify case label production:**

Current production (standard C):
```yacc
case_label:
      CASE constant_expr ':'
            { /* existing code */ }
    ;
```

Modified production with range support:
```yacc
case_label:
      CASE constant_expr ':'
            {
                /* Single case value */
                gen_case_label($2, $2);  /* start == end */
            }
    | CASE constant_expr ELLIPSIS constant_expr ':'
            {
                /* Case range */
                if ($2 > $4) {
                    uerror("case range start must be <= end");
                } else {
                    gen_case_label($2, $4);  /* start, end */
                }
            }
    ;
```

**Semantic checks:**

```yacc
case_label:
      CASE constant_expr ':'
            {
                $$ = make_case($2, $2);
            }
    | CASE constant_expr ELLIPSIS constant_expr ':'
            {
                /* Validate range */
                if (!is_constant($2) || !is_constant($4)) {
                    uerror("case range bounds must be constant expressions");
                    $$ = make_case($2, $2);  /* fallback to single case */
                } else if ($2->n_type != $4->n_type) {
                    uerror("case range bounds must have same type");
                    $$ = make_case($2, $2);
                } else if (get_const_val($2) > get_const_val($4)) {
                    uerror("case range start (%lld) must be <= end (%lld)",
                           get_const_val($2), get_const_val($4));
                    $$ = make_case($2, $2);
                } else {
                    $$ = make_case_range($2, $4);
                }
            }
    ;
```

### 3. cc/ccom/trees.c (Code Generation)

**Add case range support to AST:**

```c
/* In tree node structure (if not already present) */
struct case_label {
    long long start_val;
    long long end_val;     /* NEW: end value for ranges */
    int is_range;          /* NEW: flag indicating range vs single value */
    struct case_label *next;
};
```

**Modify case generation:**

```c
/*
 * Generate code for case label
 * If is_range is true, generates multiple labels or range check
 */
void
gen_case_label(NODE *start, NODE *end)
{
    long long start_val = get_const_val(start);
    long long end_val = get_const_val(end);

    if (start_val == end_val) {
        /* Single case - existing code */
        gen_single_case(start_val);
    } else {
        /* Range - choose strategy based on range size */
        long long range_size = end_val - start_val + 1;

        if (range_size <= MAX_CASE_EXPANSION) {
            /* Small range: expand to individual cases */
            gen_expanded_cases(start_val, end_val);
        } else {
            /* Large range: generate range check */
            gen_range_check(start_val, end_val);
        }
    }
}

#define MAX_CASE_EXPANSION 20  /* Tune this threshold */

/*
 * Expand case range to individual case labels
 * Used for small ranges (e.g., case 1 ... 5)
 */
static void
gen_expanded_cases(long long start, long long end)
{
    for (long long val = start; val <= end; val++) {
        gen_single_case(val);
    }
}

/*
 * Generate range check for large case ranges
 * Used for large ranges (e.g., case 1 ... 10000)
 */
static void
gen_range_check(long long start, long long end)
{
    /*
     * Instead of jump table, generate:
     *   if (switch_expr >= start && switch_expr <= end) {
     *       goto case_body;
     *   }
     */

    /* This integrates with existing switch code generation */
    /* Implementation depends on PCC's switch internals */
}
```

### 4. cc/ccom/pftn.c (Semantic Analysis)

**Check for overlapping case ranges:**

```c
/*
 * Check if two case ranges overlap
 */
static int
ranges_overlap(long long s1, long long e1, long long s2, long long e2)
{
    return !(e1 < s2 || e2 < s1);
}

/*
 * Validate case labels in switch statement
 * Check for duplicates and overlapping ranges
 */
static void
check_case_labels(struct case_label *labels)
{
    struct case_label *p, *q;

    for (p = labels; p != NULL; p = p->next) {
        for (q = p->next; q != NULL; q = q->next) {
            if (ranges_overlap(p->start_val, p->end_val,
                             q->start_val, q->end_val)) {
                uerror("duplicate or overlapping case values");
                werror("  first range: %lld ... %lld",
                       p->start_val, p->end_val);
                werror("  second range: %lld ... %lld",
                       q->start_val, q->end_val);
            }
        }
    }
}
```

## Code Generation Strategies

### Strategy 1: Expansion (Small Ranges)

**Best for:** Ranges with ≤ 20 values

**Generated code:**
```c
/* Source */
case 1 ... 5:
    action();
    break;

/* Expands to */
case 1:
case 2:
case 3:
case 4:
case 5:
    action();
    break;
```

**Advantages:**
- Simple to implement
- Uses existing jump table optimization
- Fast execution (jump table lookup)

**Disadvantages:**
- Large code size for big ranges
- Limited to contiguous integer ranges

### Strategy 2: Range Check (Large Ranges)

**Best for:** Ranges with > 20 values

**Generated code:**
```c
/* Source */
case 1 ... 1000:
    action();
    break;

/* Generates */
if (switch_expr >= 1 && switch_expr <= 1000) {
    action();
    goto end_switch;
}
```

**Advantages:**
- Compact code for large ranges
- Constant time range check

**Disadvantages:**
- No jump table optimization
- Slower than jump table for small ranges

### Strategy 3: Hybrid

**Best approach:** Combine both strategies

```c
/* Decision logic */
if (range_size <= 20) {
    expand_to_cases();
} else {
    generate_range_check();
}
```

## Testing

### Test Cases

Create `test_case_ranges.c`:

```c
#include <stdio.h>
#include <ctype.h>

/* Character classification */
const char *
classify(int c)
{
    switch (c) {
        case 'a' ... 'z':
            return "lowercase";

        case 'A' ... 'Z':
            return "uppercase";

        case '0' ... '9':
            return "digit";

        case ' ':
        case '\t':
        case '\n':
            return "whitespace";

        default:
            return "other";
    }
}

/* Grade classification */
char
get_grade(int score)
{
    switch (score) {
        case 90 ... 100:
            return 'A';
        case 80 ... 89:
            return 'B';
        case 70 ... 79:
            return 'C';
        case 60 ... 69:
            return 'D';
        case 0 ... 59:
            return 'F';
        default:
            return '?';
    }
}

/* Enum ranges */
enum Color { RED, ORANGE, YELLOW, GREEN, BLUE, INDIGO, VIOLET };

const char *
color_category(enum Color c)
{
    switch (c) {
        case RED ... YELLOW:
            return "warm";
        case GREEN ... VIOLET:
            return "cool";
    }
    return "unknown";
}

/* Large range */
int
categorize_port(int port)
{
    switch (port) {
        case 0 ... 1023:
            return 1;  /* System/well-known */
        case 1024 ... 49151:
            return 2;  /* Registered */
        case 49152 ... 65535:
            return 3;  /* Dynamic/private */
        default:
            return 0;  /* Invalid */
    }
}

int main(void)
{
    /* Test character classification */
    printf("Character Classification:\n");
    printf("  'a' -> %s\n", classify('a'));
    printf("  'Z' -> %s\n", classify('Z'));
    printf("  '5' -> %s\n", classify('5'));
    printf("  ' ' -> %s\n", classify(' '));
    printf("  '@' -> %s\n", classify('@'));

    /* Test grades */
    printf("\nGrades:\n");
    printf("  95 -> %c\n", get_grade(95));
    printf("  85 -> %c\n", get_grade(85));
    printf("  75 -> %c\n", get_grade(75));
    printf("  65 -> %c\n", get_grade(65));
    printf("  55 -> %c\n", get_grade(55));

    /* Test color categories */
    printf("\nColors:\n");
    printf("  RED -> %s\n", color_category(RED));
    printf("  YELLOW -> %s\n", color_category(YELLOW));
    printf("  GREEN -> %s\n", color_category(GREEN));
    printf("  VIOLET -> %s\n", color_category(VIOLET));

    /* Test port categorization */
    printf("\nPorts:\n");
    printf("  80 -> category %d\n", categorize_port(80));
    printf("  8080 -> category %d\n", categorize_port(8080));
    printf("  60000 -> category %d\n", categorize_port(60000));

    return 0;
}
```

### Error Cases

Test file `test_range_errors.c`:

```c
/* These should produce errors: */

void test_errors(int x)
{
    switch (x) {
        case 10 ... 5:    /* Error: start > end */
            break;

        case 1 ... 5:
        case 3 ... 7:     /* Error: overlapping ranges */
            break;

        case 'a' ... 100: /* Error: type mismatch? */
            break;
    }
}
```

### Build and Test

```bash
# Build modified PCC
cd cc/ccom
make clean
make

# Test case ranges
pcc test_case_ranges.c -o test_case_ranges
./test_case_ranges

# Test error cases
pcc -c test_range_errors.c
# Should produce appropriate error messages
```

## Assembly Output Verification

Verify generated code:

```bash
pcc -S test_case_ranges.c

# Check assembly output:
# - Small ranges should expand to multiple case labels
# - Large ranges should generate range checks
# - Verify jump table optimization still works
```

## Compatibility

### GCC Compatibility

GCC also supports case ranges with the same syntax:
```c
case 1 ... 10:  /* GCC extension */
```

**100% compatible** with GCC's case range syntax.

### Standards

- **Not in C standard** (C89, C99, C11, C17, C23)
- **GCC extension** since early versions
- **MetaWare High C** had it in 1989
- **Clang** supports it for GCC compatibility

## Performance Impact

### Small Ranges (Expanded)
- **Same as hand-written** fall-through cases
- **Jump table optimized** (if applicable)
- **Zero runtime overhead**

### Large Ranges (Range Check)
- **Two comparisons** instead of jump table
- **Still faster** than long fall-through list
- **Minimal overhead** (~2-4 instructions)

## Edge Cases

### Boundary Conditions
```c
case INT_MIN ... INT_MAX:  /* Entire integer range */
case 0 ... 0:              /* Single value (same as case 0:) */
case 'a' ... 'a':          /* Single character */
```

### Signed/Unsigned
```c
case -10 ... 10:           /* Signed range */
case 0U ... 100U:          /* Unsigned range */
case -5 ... 100U:          /* Warning: mixed signedness? */
```

### Type Promotion
```c
case (char)'a' ... (char)'z':    /* Character range */
case (short)1 ... (short)100:    /* Short range */
```

## Documentation

Update:
- User manual - Show case range examples
- Man pages - Document syntax and semantics
- Error messages - Clear messages for overlapping ranges

## Related Features

Case ranges work well with:
- **Pattern matching** (future extension)
- **Range checks** (runtime validation)
- **Enum ranges** (type-safe ranges)

## Estimated Effort

- **Lexer modifications:** 1-2 hours (trivial)
- **Parser modifications:** 8-12 hours
- **Code generation:** 16-24 hours (complex)
- **Semantic checks:** 8-12 hours
- **Testing:** 8-12 hours
- **Documentation:** 4-6 hours

**Total:** 2-3 weeks including thorough testing

## Summary

Case ranges are:
- **Medium complexity** (parser + codegen)
- **High value** (much cleaner code)
- **GCC compatible** (existing user expectations)
- **MetaWare High C original** (1989)

Recommended as **second syntax extension to implement** (after numeric separators).

## Implementation Notes

### Jump Table Optimization

Preserve existing jump table optimization:
- Expanded ranges participate in jump tables
- Range checks bypass jump tables (converted to if-else)
- Hybrid approach gives best performance

### Memory Usage

Watch out for:
- Extremely large ranges (e.g., `case 0 ... 1000000`)
- Use range check strategy for ranges > threshold
- Warn on very large ranges

### Switch Statement Structure

The implementation should integrate cleanly with PCC's existing switch handling:
- Track case labels during parsing
- Build case table during semantic analysis
- Generate optimal code based on case distribution
- Detect duplicate/overlapping cases

This may require understanding PCC's internal switch implementation in detail.
