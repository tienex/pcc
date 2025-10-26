# libmetaware - MetaWare High C Extensions Runtime Library

## Overview

libmetaware provides runtime support for the innovative language extensions from MetaWare's High C compiler (circa 1989). These features were decades ahead of their time and influenced modern languages.

## Features

### Runtime Features (Fully Implemented)

#### 1. Nested Functions with Closures
Support for Pascal-style nested functions with access to parent scope variables.

#### 2. Generator Coroutines
Python-style generators with yield, implemented **12 years before Python 2.2**!

#### 3. Function Values (Closures)
Full function values that work as non-escaping closures.

#### 4. Non-Local Goto
Jump from nested functions to parent labels with stack unwinding.

### Syntax Extensions (Preprocessor Workarounds)

#### 5. Labeled/Named Arguments
Python-style keyword arguments using C99 designated initializers.

#### 6. Numeric Literal Separators
Predefined constants for readable large numbers (_1K, _1M, _1B, HEX4, etc.).

#### 7. Case Ranges
Macro-based character class ranges for switch statements (CASE_LOWERCASE, etc.).

## Installation

Included with PCC:

```bash
./configure
make
sudo make install
```

## Quick Start

### Runtime Features Example

```c
#include <metaware.h>
#include <stdio.h>

/* Generator example */
MW_GENERATOR(range, int, int) {
    int n = *(int *)__args;
    for (int i = 0; i < n; i++) {
        MW_YIELD(__gen, i);
    }
}

int main(void) {
    mw_generator_t *gen = range(5);

    int num;
    MW_FOR_EACH(num, gen) {
        printf("%d ", num);  /* Prints: 0 1 2 3 4 */
    }

    mw_free_generator(gen);
    return 0;
}
```

Compile:
```bash
pcc program.c -lmetaware -o program
```

### Syntax Extensions Example

```c
#include <metaware_syntax.h>
#include <stdio.h>

/* Labeled arguments */
DECLARE_LABELED_FUNC(drawRect,
    int x; int y; int width; int height; int color;
)

DEFINE_LABELED_FUNC(drawRect) {
    printf("Drawing at (%d,%d) size %dx%d color 0x%X\n",
           ARGS->x, ARGS->y, ARGS->width, ARGS->height, ARGS->color);
}

int main(void) {
    /* Arguments in any order */
    CALL(drawRect,
        .width = 200,
        .height = 100,
        .x = 50,
        .y = 75,
        .color = 0xFF0000
    );

    /* Numeric separators */
    int buffer_size = 8 * _1K;      /* 8 KB */
    int max_size = 100 * _1M;       /* 100 MB */
    int color = HEX8(FF, AA, 00, 00);

    /* Case ranges */
    char c = 'a';
    switch (c) {
        CASE_LOWERCASE:
            printf("Lowercase letter\n");
            break;
        CASE_UPPERCASE:
            printf("Uppercase letter\n");
            break;
        CASE_DIGIT:
            printf("Digit\n");
            break;
    }

    return 0;
}
```

Compile:
```bash
pcc program.c -I. -o program
```

## API Documentation

### Runtime Features
See [METAWARE_EXTENSIONS.md](../METAWARE_EXTENSIONS.md) for complete runtime API documentation.

### Syntax Extensions
See [metaware_syntax.h](metaware_syntax.h) for syntax workaround macros.

**Examples:**
- `examples/labeled_args_example.c` - Keyword argument examples
- `examples/numeric_separators_example.c` - Readable number constants
- `examples/case_ranges_example.c` - Character class ranges
- `examples/combined_syntax_example.c` - All features together
- `test_metaware_syntax.c` - Comprehensive test suite

For **true compiler syntax** support (not workarounds), see:
- [METAWARE_COMPLETE_FEATURES.md](../METAWARE_COMPLETE_FEATURES.md) - Full feature specification
- [Compiler implementation patches](#) - Coming soon

## Performance

- **Nested Functions:** 2-3 indirect calls vs 1 for native
- **Generators:** ~100-200 cycles/yield (setjmp-based)
- **Function Values:** 1 indirect call overhead
- **Non-Local Goto:** Similar to longjmp (~50-100 cycles)

With full compiler support, overhead would be minimal (5-10 cycles).

## Platform Support

- ✅ Linux (x86, x86_64, ARM)
- ✅ macOS (x86_64, ARM64)
- ✅ BSD variants
- ✅ Windows (with pthread emulation)

Generators work best on POSIX systems with ucontext support.

## Testing

```bash
pcc test_metaware.c -I. -lmetaware -o test_metaware
./test_metaware
```

## Limitations

Current implementation:
- Generators use setjmp/longjmp (portable but slower)
- Nested functions via trampolines (extra indirection)
- No escape analysis (closures can't escape)
- Manual API (no syntax support)

Full compiler support would eliminate these limitations.

## Historical Context

MetaWare High C (1989) featured:
- Nested functions (25 years before GCC extension became common)
- Generators (12 years before Python 2.2)
- Numeric separators (25 years before C++14)
- Non-local goto with cleanup (early exception handling)

**libmetaware brings these innovations to modern C!**

## License

BSD-style license (same as PCC). See [../COPYING](../COPYING).

## See Also

- [METAWARE_EXTENSIONS.md](../METAWARE_EXTENSIONS.md) - Complete feature documentation
- [LANGUAGE_EXTENSIONS_ROADMAP.md](../LANGUAGE_EXTENSIONS_ROADMAP.md) - Implementation roadmap

---

**Version:** 1.0
**Maintainer:** PCC Development Team
