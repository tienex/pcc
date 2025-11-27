# libunicode - Portable C11 Unicode Support Library

## Overview

This library provides a portable implementation of C11 Unicode character utilities
(`uchar.h`) for systems that lack native C11 Unicode support.

## Features

- **C11 Unicode Types**: Provides `char16_t` and `char32_t` types
- **UTF-16 Support**: Conversion between multibyte characters and UTF-16
- **UTF-32 Support**: Conversion between multibyte characters and UTF-32
- **Portable**: Works on any system with a C99 compiler
- **Standards Compliant**: Implements C11 ISO/IEC 9899:2011 standard

## API

The library implements the following C11 functions from `<uchar.h>`:

### Functions

- `mbrtoc16()` - Convert a multibyte character to a 16-bit wide character (UTF-16)
- `c16rtomb()` - Convert a 16-bit wide character (UTF-16) to a multibyte character
- `mbrtoc32()` - Convert a multibyte character to a 32-bit wide character (UTF-32)
- `c32rtomb()` - Convert a 32-bit wide character (UTF-32) to a multibyte character

### Types

- `char16_t` - 16-bit character type for UTF-16 encoding
- `char32_t` - 32-bit character type for UTF-32 encoding

## Usage

```c
#include <uchar.h>
#include <stdio.h>

int main(void) {
    const char *utf8 = "Hello, 世界!";
    char32_t c32;
    mbstate_t state = {0};
    size_t len;

    len = mbrtoc32(&c32, utf8, 4, &state);
    printf("First character: U+%04X\n", (unsigned)c32);

    return 0;
}
```

## Building

This library is automatically built by the PCC build system if your system
lacks native C11 Unicode support. The configure script will detect this and
include the library in the build.

## Compatibility

The library provides UTF-8 to UTF-16/UTF-32 conversion and vice versa. It
properly handles:

- Surrogate pairs in UTF-16
- Invalid UTF-8 sequences
- Overlong encodings
- Code points outside the Unicode range

## License

This library is part of the Portable C Compiler (PCC) project and is distributed
under the same BSD-style license as PCC.
