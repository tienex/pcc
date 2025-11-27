# Compiler Patch: Numeric Literal Separators

## Feature Description

Add support for underscore separators in numeric literals for improved readability:
- `1_000_000` instead of `1000000`
- `0xFF_AA_00` instead of `0xFFAA00`
- `0b1010_1010` instead of `0b10101010`
- `3.141_592_653` instead of `3.141592653`

This feature was in MetaWare High C (1989), 25 years before C++14.

## Implementation Difficulty

**Easy** - Lexer-only changes, ~2 weeks of work

## Files to Modify

### 1. cc/ccom/scan.l (Lexer)

**Current digit patterns:**
```lex
D                       [0-9]
L                       [a-zA-Z_]
H                       [a-fA-F0-9]
E                       [Ee][+-]?{D}+
FS                      (f|F|l|L)
IS                      (u|U|l|L)*
```

**Modified digit patterns with separators:**
```lex
D                       [0-9]
L                       [a-zA-Z_]
H                       [a-fA-F0-9]

/* New: digit sequences with optional underscores */
DIGIT_SEQ               {D}("_"?{D})*
HEX_SEQ                 {H}("_"?{H})*
BIN_SEQ                 [01]("_"?[01])*

/* Exponent can have separators too */
E                       [Ee][+-]?{DIGIT_SEQ}

FS                      (f|F|l|L)
IS                      (u|U|l|L)*
```

**Modified literal rules:**

```lex
/* Integer literals */
0[xX]{HEX_SEQ}{IS}?         { yylval.number = process_hex_int(yytext); return NUMBER; }
0[bB]{BIN_SEQ}{IS}?         { yylval.number = process_bin_int(yytext); return NUMBER; }
0{DIGIT_SEQ}{IS}?           { yylval.number = process_oct_int(yytext); return NUMBER; }
{DIGIT_SEQ}{IS}?            { yylval.number = process_dec_int(yytext); return NUMBER; }

/* Floating-point literals */
{DIGIT_SEQ}"."{DIGIT_SEQ}?({E})?{FS}?  { yylval.number = process_float(yytext); return FNUMBER; }
{DIGIT_SEQ}{E}{FS}?                    { yylval.number = process_float(yytext); return FNUMBER; }
"."{DIGIT_SEQ}({E})?{FS}?              { yylval.number = process_float(yytext); return FNUMBER; }
```

### 2. cc/ccom/scan.l (Processing Functions)

Add new helper function:

```c
/*
 * Strip underscores from numeric literal string
 * Returns new malloced string without underscores
 */
static char *
strip_underscores(const char *text)
{
    size_t len = strlen(text);
    char *result = malloc(len + 1);
    char *p = result;

    if (result == NULL)
        cerror("out of memory");

    for (const char *s = text; *s; s++) {
        if (*s != '_') {
            *p++ = *s;
        }
    }
    *p = '\0';

    return result;
}
```

**Modify existing literal processing functions:**

```c
/* Decimal integer */
static NODE *
process_dec_int(const char *text)
{
    char *stripped = strip_underscores(text);

    /* Find suffix */
    char *endptr;
    long long value = strtoll(stripped, &endptr, 10);

    /* Parse suffix (u, U, l, L, ll, LL, etc.) */
    /* ... existing suffix parsing code ... */

    free(stripped);
    return create_int_node(value, type);
}

/* Hexadecimal integer */
static NODE *
process_hex_int(const char *text)
{
    char *stripped = strip_underscores(text);

    /* Skip 0x or 0X prefix */
    char *digits = stripped + 2;

    char *endptr;
    unsigned long long value = strtoull(digits, &endptr, 16);

    /* Parse suffix */
    /* ... existing suffix parsing code ... */

    free(stripped);
    return create_int_node(value, type);
}

/* Binary integer (if PCC doesn't have this, add it) */
static NODE *
process_bin_int(const char *text)
{
    char *stripped = strip_underscores(text);

    /* Skip 0b or 0B prefix */
    char *digits = stripped + 2;

    char *endptr;
    unsigned long long value = strtoull(digits, &endptr, 2);

    /* Parse suffix */
    /* ... existing suffix parsing code ... */

    free(stripped);
    return create_int_node(value, type);
}

/* Floating-point */
static NODE *
process_float(const char *text)
{
    char *stripped = strip_underscores(text);

    char *endptr;
    long double value = strtold(stripped, &endptr);

    /* Parse suffix (f, F, l, L) */
    /* ... existing suffix parsing code ... */

    free(stripped);
    return create_float_node(value, type);
}
```

### 3. Validation (Optional but Recommended)

Add validation to ensure underscores are used correctly:

```c
/*
 * Validate underscore placement in numeric literal
 * Returns 1 if valid, 0 if invalid
 */
static int
validate_separators(const char *text)
{
    size_t len = strlen(text);

    /* Can't start or end with underscore */
    if (text[len-1] == '_') {
        uerror("underscore at end of numeric literal");
        return 0;
    }

    /* Check for consecutive underscores */
    for (size_t i = 0; i < len - 1; i++) {
        if (text[i] == '_' && text[i+1] == '_') {
            uerror("consecutive underscores in numeric literal");
            return 0;
        }
    }

    /* Check for underscore after base prefix */
    if (len >= 3 && text[0] == '0') {
        if ((text[1] == 'x' || text[1] == 'X' ||
             text[1] == 'b' || text[1] == 'B') && text[2] == '_') {
            uerror("underscore immediately after base prefix");
            return 0;
        }
    }

    return 1;
}
```

Call `validate_separators()` in each processing function before stripping underscores.

## Testing

### Test Cases

Create `test_numeric_separators.c`:

```c
#include <stdio.h>

int main(void)
{
    /* Decimal separators */
    int thousand = 1_000;
    int million = 1_000_000;
    long long billion = 1_000_000_000;
    long long trillion = 1_000_000_000_000LL;

    printf("Decimal:\n");
    printf("  1_000 = %d\n", thousand);
    printf("  1_000_000 = %d\n", million);
    printf("  1_000_000_000 = %lld\n", billion);
    printf("  1_000_000_000_000 = %lld\n", trillion);

    /* Hexadecimal separators */
    unsigned int color = 0xFF_AA_00;
    unsigned int addr = 0xDEAD_BEEF;
    unsigned long long big_hex = 0x1234_5678_9ABC_DEF0ULL;

    printf("\nHexadecimal:\n");
    printf("  0xFF_AA_00 = 0x%06X\n", color);
    printf("  0xDEAD_BEEF = 0x%08X\n", addr);
    printf("  0x1234_5678_9ABC_DEF0 = 0x%016llX\n", big_hex);

    /* Binary separators */
    unsigned char flags = 0b1111_0000;
    unsigned char pattern = 0b1010_1010;

    printf("\nBinary:\n");
    printf("  0b1111_0000 = 0x%02X\n", flags);
    printf("  0b1010_1010 = 0x%02X\n", pattern);

    /* Floating-point separators */
    double pi = 3.141_592_653_589_793;
    double avogadro = 6.022_140_76e23;
    double planck = 6.626_070_15e-34;

    printf("\nFloating-point:\n");
    printf("  pi = %.15f\n", pi);
    printf("  avogadro = %e\n", avogadro);
    printf("  planck = %e\n", planck);

    /* Different groupings */
    int phone = 555_123_4567;
    long long credit_card = 1234_5678_9012_3456LL;

    printf("\nPractical:\n");
    printf("  Phone: %d\n", phone);
    printf("  Card: %lld\n", credit_card);

    return 0;
}
```

### Error Cases

Test file `test_separator_errors.c`:

```c
/* These should produce errors: */

int bad1 = 123_;        /* Error: trailing underscore */
int bad2 = _123;        /* Error: leading underscore (or identifier!) */
int bad3 = 12__34;      /* Error: consecutive underscores */
int bad4 = 0x_FF;       /* Error: underscore after prefix */
int bad5 = 0b_1010;     /* Error: underscore after prefix */
double bad6 = 3._14;    /* Error: underscore after decimal point */
double bad7 = 3.14_;    /* Error: trailing underscore */
```

### Build and Test

```bash
# Build modified PCC
cd cc/ccom
make clean
make

# Test with separators
pcc -c test_numeric_separators.c
./test_numeric_separators

# Expected output should show correct values

# Test error cases
pcc -c test_separator_errors.c
# Should produce appropriate error messages
```

## Compatibility

### Backward Compatibility

**100% backward compatible** - All existing code continues to work. Underscores are simply allowed where they weren't before.

### Standards Compliance

- **C standard:** Not in C11, proposed for future C standards
- **C++14:** Has digit separators with `'` instead of `_`
- **C++14 syntax:** `1'000'000` (apostrophe)
- **MetaWare/PCC:** `1_000_000` (underscore)

To support both:

```c
#ifdef __cplusplus
#define SEP '
#else
#define SEP _
#endif

int x = 1SEP000SEP000;  /* Works in both */
```

Better: Just use underscore (more intuitive, matches other languages)

## Performance Impact

**Zero runtime impact** - Underscores stripped at lexing time. Generated code is identical to code without separators.

**Minimal compile-time impact** - Extra string processing during lexing, negligible overhead.

## Documentation

Update:
- `cc/ccom/README` - Document new feature
- Man pages - Add examples with separators
- User guide - Show readability benefits

## Related Features

After implementing numeric separators, consider:
1. **Binary literals** (`0b1010`) if not already supported
2. **Hexadecimal floating-point** (`0x1.5p3`)
3. **Digit separators in PP numbers** (preprocessor number tokens)

## Estimated Effort

- **Lexer modifications:** 4-6 hours
- **Processing functions:** 4-6 hours
- **Validation:** 2-3 hours
- **Testing:** 4-6 hours
- **Documentation:** 2-3 hours

**Total:** 1.5-2 weeks including thorough testing

## Summary

Numeric literal separators are:
- **Easy to implement** (lexer-only)
- **High value** (greatly improves readability)
- **Zero runtime cost**
- **Backward compatible**
- **A MetaWare High C original feature from 1989!**

Recommended as **first syntax extension to implement**.
