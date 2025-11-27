# C++ Compiler State Evaluation Report

**Date:** 2025-10-26
**Evaluated by:** Claude
**Repository:** Portable C Compiler (PCC)
**Branch:** claude/check-cpp-compiler-011CUUyhNVWojebhDLo8Faon

---

## Executive Summary

The Portable C Compiler (PCC) includes a C++ compiler component (`cxxcom`) that is **currently non-functional** due to a critical build dependency issue. The codebase exists and appears structurally complete, but the build fails due to a missing lexical analyzer generator (flex/lex).

**Status:** ❌ NON-FUNCTIONAL - Build blocked by missing dependency

---

## Build Configuration

### Configuration Status: ✅ SUCCESS

```
Target CPU:            amd64
Target ABI:            elf
Target OS:             linux
Compiler name:         pcc
GCC compatibility:     yes
PCC debugging:         yes
Native floating point: yes
TLS support:           no
Multi-Arch path:       x86_64-linux-gnu
```

The autoconf configuration completed successfully and generated all required Makefiles including `cc/cxxcom/Makefile`.

---

## Build Status: ❌ FAILED

### Error Details

**Location:** `cc/cxxcom/Makefile:86` (and `cc/ccom/Makefile:103`)
**Command:** `: ./scan.l`
**Error:** `mv: cannot stat '.c': No such file or directory`

### Root Cause

The configure script could not locate `flex` or `lex` (lexical analyzer generators):

```
checking for flex... no
checking for lex... no
LEX_OUTPUT_ROOT=''
```

This causes `LEX_OUTPUT_ROOT` to be set to an empty string in the Makefile, which results in:
```makefile
mv -f .c scan.c    # Invalid: missing source filename
```

The build cannot proceed past the lexer generation stage for either the C or C++ compiler.

---

## C++ Compiler Source Code Analysis

### Code Statistics

- **Total lines:** ~23,085 lines of code
- **Source files:** 20 files (.c, .h, .y, .l)

### File Inventory

**Core Implementation:**
- `cxxcode.c` - C++ specific code generation and namespace handling
- `cgram.y` - Grammar parser (yacc/bison) - 56,590 bytes
- `scan.l` - Lexical scanner (flex/lex) - **Build blocker**
- `builtins.c` - Built-in function support
- `gcc_compat.c` - GCC compatibility layer
- `pftn.c` - Function prototype handling
- `trees.c` - Abstract syntax tree manipulation (68,378 bytes)
- `symtabs.c` - Symbol table management
- `init.c` - Initializer handling (27,655 bytes)
- `inline.c` - Inline function support
- `main.c` - Main compiler entry point
- `optim.c` - Optimization passes
- `stabs.c` - STABS debugging format support

**Headers:**
- `cxxdefs.h` - C++ specific definitions
- `pass1.h` - Compiler pass 1 interface (17,952 bytes)

---

## C++ Language Features

Based on `cxxdefs.h` analysis, the compiler implements:

### Operators
- `const_cast` - Const-correctness casting
- `dynamic_cast` - Runtime type checking
- `reinterpret_cast` - Low-level type reinterpretation
- `static_cast` - Compile-time type casting

### Memory Management
- `new` operator (5 variants: new, new[], delete, delete[], normal)
- `delete` operator with array support

### Object-Oriented Features
- Classes (CLNAME symbol table type)
- Namespaces (NSPACE symbol table type)
- Member functions
- Name mangling/decoration

### Linkage
- C++ linkage (default)
- C linkage (`extern "C"`)

### API Functions (cxxdefs.h)
```c
char *decoratename(struct symtab *sp, int type);
NODE *cxx_new(NODE *p);
NODE *cxx_delete(NODE *p, int del);
void dclns(NODE *attr, char *n);
struct symtab *cxxlookup(NODE *p, int declare);
void cxxsetname(struct symtab *sp);
struct symtab *cxxdclstr(char *n);
struct symtab *cxxftnfind(NODE *p, int flags);
NODE *cxxrstruct(int soru, NODE *attr, NODE *t, char *tag);
NODE *cxxstructref(NODE *p, int f, char *name);
```

---

## Build Dependencies

### Present ✅
- GCC C compiler
- Bison (yacc) - Grammar parser generator
- Standard C headers and libraries
- GNU Make
- Autoconf/configure

### Missing ❌
- **Flex or Lex** - Lexical analyzer generator (CRITICAL)

---

## Impact Assessment

### Affected Components
- ❌ C++ compiler backend (`cxxcom`) - Cannot build
- ❌ C compiler backend (`ccom`) - Cannot build (same issue)
- ❓ Fortran compiler (`fcom`) - Not tested
- ❓ C preprocessor (`cpp`) - Not tested

### Compilation Progress
The build successfully completes:
1. ✅ Build tool compilation (`mkext`)
2. ✅ Parser generation from `cgram.y` (with POSIX yacc warnings)
3. ✅ External symbol table generation
4. ✅ Most object file compilation (~20 .o files created)
5. ❌ **FAILS** at lexer generation from `scan.l`

---

## Warnings Observed

### Non-Critical Warnings

1. **Deprecated BSD Source Flag:**
   ```
   warning: "_BSD_SOURCE and _SVID_SOURCE are deprecated, use _DEFAULT_SOURCE"
   ```
   All compiled files show this warning. Non-blocking.

2. **POSIX Yacc Compatibility:**
   ```
   cgram.y:252.24-31: warning: POSIX yacc reserves %type to nonterminals
   ```
   Multiple instances. Non-blocking.

3. **Unused Value Warnings:**
   ```
   softfloat.c:480:67: warning: right-hand operand of comma expression has no effect
   ```
   In macro expansion. Non-blocking.

---

## Recommendations

### Immediate Actions

1. **Install flex dependency:**
   ```bash
   # Debian/Ubuntu
   apt-get install flex

   # RHEL/CentOS
   yum install flex

   # Alpine
   apk add flex
   ```

2. **Rebuild after installing flex:**
   ```bash
   make clean
   ./configure
   make
   ```

### Project Improvements

1. **Update configure.ac** to fail hard when flex/lex is not found:
   ```
   AC_PROG_LEX
   if test "$LEX" = ":"; then
     AC_MSG_ERROR([flex or lex is required to build pcc])
   fi
   ```

2. **Update documentation** to list flex as a mandatory build dependency

3. **Add .gitignore** to prevent build artifacts from being tracked

4. **Consider checking in pre-generated `scan.c`** to allow builds without flex (some projects do this)

5. **Fix deprecation warnings** by replacing `-D_BSD_SOURCE` with `-D_DEFAULT_SOURCE` in configure.ac

---

## Compiler Architecture Notes

### Design
- C++ compiler shares significant infrastructure with C compiler
- Structured as separate executable (`cxxcom`) with C++-specific extensions
- Uses traditional compiler phases: lex → parse → AST → codegen

### Target Feature Level
- Appears to target early C++ (pre-C++11)
- No visible support for: templates, exceptions (unverified), STL
- Focus on basic OOP: classes, namespaces, basic operators

### Code Quality
- Well-structured codebase with clear separation of concerns
- BSD licensed
- Active debugging support (`-DPCC_DEBUG`)
- GCC compatibility mode available

---

## Conclusion

The PCC C++ compiler (`cxxcom`) has a complete and reasonably well-structured implementation spanning ~23K lines of code. The source includes support for fundamental C++ features including classes, namespaces, the four C++ cast operators, and new/delete operators.

**However, the compiler is completely non-functional** due to a trivial but blocking build dependency issue: the absence of flex/lex prevents the lexical scanner from being generated. This is easily resolved by installing the flex package, after which the build should proceed to completion.

The same issue affects the C compiler (`ccom`), indicating this is a systemic build environment problem rather than a C++-specific issue.

**Overall Assessment:** Code exists and appears viable, but build infrastructure needs flex to be functional.

---

## Build Log Summary

```
Configuration:  ✅ SUCCESS
Parser Gen:     ✅ SUCCESS (bison)
Lexer Gen:      ❌ FAILED (no flex)
Object Files:   ⚠️  PARTIAL (stopped at lexer)
Linking:        ❌ NOT REACHED
Final Binary:   ❌ NOT CREATED
```

---

**End of Report**
