# Paradox PAL and ObjectPAL Compiler Frontend

This directory contains the frontend compiler for Paradox Application Language (PAL) and ObjectPAL, targeting the PCC intermediate representation.

## Overview

**PAL** (Paradox Application Language) was the scripting language used in Borland's Paradox database management system from versions 1.0 through 4.x. **ObjectPAL** is the object-oriented successor introduced in Paradox for Windows, adding support for objects, methods, inheritance, and modern programming features.

This compiler frontend allows compilation of PAL and ObjectPAL programs into the PCC IR, enabling cross-platform execution and optimization.

## Features

### Language Support

- **Multiple dialect support**: Compile for different Paradox/ObjectPAL versions
- **Classic PAL** (versions 1.0, 3.0, 4.5):
  - Procedures and functions
  - Local and global variables
  - Control flow (if, while, for)
  - Database operations (scan, edit, locate)
  - Built-in functions (string, math, date/time)

- **ObjectPAL** (versions 1.0, 7.0, 9.0+):
  - Object-oriented programming (objects, methods, inheritance)
  - Properties and events
  - Exception handling (try-except)
  - Advanced loops (foreach)
  - Switch statements
  - Private/protected/public visibility

### Data Types

- **Numeric**: SmallInt, ShortInt, LongInt, Number (BCD), Currency
- **String**: String (Alphanumeric), Memo, Formatted Memo
- **Date/Time**: Date, Time, DateTime, TimeStamp
- **Binary**: Blob, Graphic, Bytes
- **Logical**: Logical (Boolean)
- **Special**: Variant (any type), AutoIncrement
- **Structured**: Array, Record, Object
- **Pointers**: References to objects and data

### Built-in Functions

The compiler includes support for 60+ built-in PAL/ObjectPAL functions:

- **String functions**: upper, lower, substr, strlen, trim, concat, format
- **Math functions**: abs, sqrt, sin, cos, exp, ln, round, random
- **Date/time**: today, now, year, month, day, datetostr, strtodate
- **Database**: moveto, locate, scan, edit, lockrecord, post
- **UI**: msginfo, msgwarning, msgerror, msgquestion
- **System**: execute, shell, fileexists, filecopy

## Directory Structure

```
paradox/
├── palcom/          # PAL/ObjectPAL compiler implementation
│   ├── pass1.h      # Frontend data structures
│   ├── error.h      # Error reporting system
│   ├── dialect.h    # Dialect configuration
│   ├── scan.l       # Lexical analyzer (Flex)
│   ├── palgram.y    # Parser grammar (Yacc)
│   ├── main.c       # Compiler entry point
│   ├── error.c      # Error implementation
│   ├── dialect.c    # Dialect configuration
│   ├── symtab.c     # Symbol table
│   ├── types.c      # Type system
│   ├── builtins.c   # Built-in functions
│   └── Makefile.in  # Build configuration
├── tests/           # Test programs
│   ├── hello.pal    # Simple example
│   ├── objectpal_example.pal  # Advanced ObjectPAL
│   └── loops.pal    # Loop examples
└── README.md        # This file
```

## Building

The PAL compiler is built as part of the PCC build process:

```bash
./configure
make
```

The compiler binary will be created as `paradox/palcom/palcom`.

## Usage

### Basic Compilation

```bash
palcom -o output.o input.pal
```

### Specify Dialect

```bash
palcom -d objectpal-7.0 -o output.o input.pal
```

### Available Dialects

- `pal-1.0` - Paradox 1.0 PAL (original, limited features)
- `pal-3.0` - Paradox 3.0 PAL (added functions, SQL)
- `pal-4.5` - Paradox 4.5 PAL (added switch, events, graphics)
- `objectpal-1.0` - ObjectPAL 1.0 (added objects, inheritance)
- `objectpal-7.0` - ObjectPAL 7.0 (added visibility, destructors)
- `objectpal-latest` - Latest ObjectPAL version (default)

### Command-Line Options

```
-h, --help              Show help message
-v, --version           Show version information
-o, --output <file>     Output file (default: stdout)
-d, --dialect <dialect> Target dialect (default: objectpal-latest)
-V, --verbose           Verbose output
-g, --debug             Enable debug output
```

## Language Examples

### Classic PAL (Paradox 3.0)

```pascal
; Calculate factorial
proc factorial(n : LongInt) : LongInt
var
  result : LongInt
  i : LongInt
begin
  result = 1
  for i = 2 to n
    result = result * i
  endfor
  return result
end
endproc
```

### ObjectPAL (Paradox 7.0)

```pascal
// Event handler for button click
method cmdCalculate_OnClick()
var
  quantity : Number
  price : Currency
  total : Currency
begin
  // Get form values
  quantity = self.getvalue("Quantity")
  price = self.getvalue("Price")

  // Calculate total
  total = quantity * price

  // Apply discount
  if total > 1000 then
    total = total * 0.9  // 10% discount
  endif

  // Update form
  self.setvalue("Total", total)
end
endmethod
```

## Technical Details

### Architecture

The PAL compiler follows the PCC frontend architecture:

1. **Lexical Analysis** (scan.l): Tokenizes PAL source code
2. **Parsing** (palgram.y): Builds abstract syntax tree (AST)
3. **Semantic Analysis** (types.c, symtab.c): Type checking and symbol resolution
4. **IR Generation**: Converts AST to PCC intermediate representation

### Dialect System

The compiler uses a feature flag system to enable/disable language features based on the target dialect. This allows accurate compilation for different Paradox versions while maintaining a single codebase.

### Symbol Table

The symbol table uses hash chaining and supports:
- Nested scopes (procedures, methods)
- Forward declarations
- Object member lookup
- Built-in function recognition

### Type System

The type system handles:
- Basic types (numeric, string, date/time)
- Structured types (arrays, records, objects)
- Type compatibility checking
- Automatic type conversions (numeric types)
- Variant type (compatible with all types)

## References

- [Paradox Database Management System](https://en.wikipedia.org/wiki/Paradox_(database))
- [Borland Paradox Documentation](https://en.wikipedia.org/wiki/Borland)
- PAL Language Reference (various Paradox versions)
- ObjectPAL Programming Guide (Paradox for Windows)

## License

Copyright (c) 2025 PCC Project

This compiler frontend is part of the Portable C Compiler (PCC) project.
