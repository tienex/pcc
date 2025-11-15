# libmangle - Universal Symbol Mangler/Demangler

A comprehensive, reusable library for symbol name mangling and demangling across multiple programming languages.

## Features

- **Multi-Language Support**: C, C++, Pascal, Delphi, Modula-2/3, Oberon, Ada, and more
- **Multiple Mangling Schemes**:
  - C (no mangling)
  - C++ Itanium ABI
  - C++ MSVC
  - Pascal/Delphi
  - Modula-2/Modula-3
  - Oberon
  - Ada
  - PCC Universal (cross-language)
- **Bidirectional**: Full mangling and demangling support
- **Type-Aware**: Encodes function signatures and type information
- **Auto-Detection**: Automatically detects mangling schemes
- **Scheme Conversion**: Convert between different mangling schemes

## Usage

```c
#include "mangle.h"

/* Mangle a function */
symbol_info_t *info = symbol_info_create("myFunction", "MyClass");
info->sym_type = SYM_FUNCTION;
char *mangled = mangle_symbol(info, MANGLE_CXX_ITANIUM);
// Result: _ZN7MyClass10myFunctionEv

/* Demangle a symbol */
demangle_info_t *result = demangle_symbol("_ZN7MyClass10myFunctionEv");
printf("Name: %s\n", result->name);  // myFunction
printf("Scope: %s\n", result->scope); // MyClass

/* PCC Universal mangling (works across all languages) */
char *universal = mangle_pcc_universal(info);

/* Cleanup */
symbol_info_free(info);
demangle_info_free(result);
free(mangled);
free(universal);
```

## PCC Universal Mangling Format

Format: `_P_<lang><scope_len><scope><name_len><name>[<type_encoding>]`

Language codes: C, X (C++), P (Pascal), D (Delphi), M (Modula-2), N (Modula-3), O (Oberon), A (Ada)

Example: `_P_X7MyClass10myFunction` for C++ MyClass::myFunction

## Reusability

This library is designed to be reusable in any compiler or linker project. It has no dependencies on PCC-specific code.
