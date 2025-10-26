# Xbase++ Runtime Library (libxbrt)

The Xbase++ Runtime Library provides the implementation of all built-in functions and runtime support for compiled Xbase++ programs.

## Features

### Core Data Types
- **xb_value_t**: Universal value container supporting all Xbase++ types
- **xb_array_t**: Dynamic array implementation
- **xb_object_t**: Object/class instance support
- **xb_codeblock_t**: Code block support

### String Functions
- **LEN()**: String length
- **SUBSTR()**, **LEFT()**, **RIGHT()**: Substring extraction
- **UPPER()**, **LOWER()**: Case conversion
- **TRIM()**, **LTRIM()**, **RTRIM()**, **ALLTRIM()**: Space trimming
- **SPACE()**, **REPLICATE()**: String generation
- **STUFF()**: String insertion/replacement
- **AT()**, **RAT()**: Substring search
- **STRTRAN()**: String replacement
- **CHR()**, **ASC()**: Character conversion
- **ISALPHA()**, **ISDIGIT()**: Character classification

### Numeric Functions
- **ABS()**, **INT()**, **ROUND()**: Basic math
- **SQRT()**, **EXP()**, **LOG()**, **LOG10()**: Advanced math
- **SIN()**, **COS()**, **TAN()**: Trigonometry
- **MIN()**, **MAX()**: Comparison
- **MOD()**: Modulo
- **RAND()**: Random numbers
- **STR()**, **VAL()**: Number/string conversion

### Array Functions
- **ALEN()**: Array length
- **ASIZE()**: Resize array
- **AADD()**: Add element
- **AINS()**, **ADEL()**: Insert/delete element
- **ASORT()**: Sort array
- **ASCAN()**: Search array
- **AFILL()**: Fill array
- **ACLONE()**: Clone array

### Date/Time Functions
- **DATE()**: Current date
- **YEAR()**, **MONTH()**, **DAY()**: Extract date components
- **DOW()**: Day of week
- **CDOW()**, **CMONTH()**: Day/month names
- **CTOD()**, **DTOC()**: Date/string conversion
- **DTOS()**, **STOD()**: Date/string (YYYYMMDD format)
- **TRANSFORM()**: Format values

### I/O Functions
- **FOPEN()**, **FCREATE()**: Open/create files
- **FCLOSE()**: Close file
- **FREAD()**, **FWRITE()**: Read/write files
- **FSEEK()**: File positioning
- **FERROR()**: Get last error
- **FILE()**: Check file existence
- **FERASE()**, **FRENAME()**: Delete/rename files
- **MEMOREAD()**, **MEMOWRIT()**: Read/write entire files

### Type Checking Functions
- **VALTYPE()**: Get value type
- **ISNIL()**, **ISNUMBER()**, **ISCHARACTER()**: Type checking
- **ISLOGICAL()**, **ISDATE()**, **ISARRAY()**: More type checking
- **ISOBJECT()**, **ISBLOCK()**: Object/codeblock checking
- **EMPTY()**: Check if value is empty

### Miscellaneous
- **QOUT()**: Quick output to console

## Building

The library is built as part of the PCC build system:

```bash
cd pxc/libxbrt
make
```

This produces `libxbrt.a` which can be linked with compiled Xbase++ programs.

## Usage

Include the header in your C code:

```c
#include <pxc/xbrt.h>
```

Link with the library:

```bash
gcc program.c -L/path/to/lib -lxbrt -lm
```

## Implementation Notes

- **Julian Dates**: Dates are stored internally as Julian day numbers
- **Dynamic Typing**: All values use the xb_value_t union type
- **Memory Management**: Callers are responsible for freeing returned strings
- **Thread Safety**: Functions are not currently thread-safe
- **Error Handling**: File I/O functions use errno for error reporting

## Example

```c
#include <pxc/xbrt.h>

int main() {
    /* Create values */
    xb_value_t *str = xb_value_new_string("Hello, World!");
    xb_value_t *num = xb_value_new_numeric(42.0);

    /* String operations */
    char *upper = xb_upper("xbase++");
    int len = xb_len("test");

    /* Numeric operations */
    double result = xb_round(3.14159, 2);

    /* Date operations */
    int32_t today = xb_date();
    int year = xb_year(today);

    /* Cleanup */
    xb_value_free(str);
    xb_value_free(num);
    free(upper);

    return 0;
}
```

## License

Copyright (c) 2025 PCC Xbase++ Runtime Library

Part of the PCC (Portable C Compiler) project.
