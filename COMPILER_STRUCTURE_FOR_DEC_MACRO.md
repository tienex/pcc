# Portable C Compiler (PCC) - Project Structure Analysis

## Executive Summary

This is a **multi-language compiler framework** based on the Portable C Compiler (PCC), an architecture from the 1970s that predates modern GCC. The PCC framework has been enhanced with:
- Multiple language frontends (C, C++, Pascal, and more)
- Support for 18+ target architectures 
- Multiple backends and code generation strategies
- Comprehensive debug symbol support across multiple formats

---

## 1. PROJECT TYPE & NATURE

**Type**: Portable, modular compiler infrastructure
**Architecture**: 3-stage pipeline (Frontend → Middle-end IR → Backend)
**Primary Language**: C (with some C++ for modern features)
**Build System**: GNU Autotools (configure, make)
**Original License**: BSD

### Project Structure Overview
```
/home/user/pcc/
├── cc/                 # C compiler frontend and driver
├── pascal/             # Pascal compiler frontend
├── mip/                # Machine-Independent Part (IR representation)
├── arch/               # Architecture-specific backends
├── common/             # Common utilities and libraries
├── libx86asm/          # Universal x86 assembly emitter
└── include/            # Public headers
```

---

## 2. EXISTING FRONTENDS & BACKENDS

### Implemented Frontends

#### A. C Compiler (cc/ccom)
- **Status**: Mature and complete
- **Location**: `/home/user/pcc/cc/ccom/`
- **Entry Point**: `main.c`
- **Lexer**: Embedded in ccom, uses C preprocessor (cpp)
- **Parser**: `cgram.y` (65KB Yacc grammar)
- **Output**: Machine-Independent Intermediate Representation (MIR)

#### B. C++ Compiler (cc/cxxcom)
- **Status**: Implemented
- **Location**: `/home/user/pcc/cc/cxxcom/`
- **Features**: Full C++ support, including OOP features

#### C. Pascal Compiler (pascal/pcom)
- **Status**: Recently implemented (modern reference implementation)
- **Location**: `/home/user/pcc/pascal/pcom/`
- **Dialects Supported**: 8 different Pascal dialects
  - ISO 7185 Standard Pascal
  - ISO 10206 Extended Pascal
  - Microsoft Pascal 4.0
  - Clascal (Apple Classic Object Pascal)
  - MacPascal (MPW Pascal)
  - Borland/Turbo Pascal 7.0
  - Delphi Object Pascal
  - Free Pascal
- **Key Files**:
  - `main.c` - Entry point and driver
  - `scan.l` - Flex lexical analyzer
  - `pgram.y` - Yacc parser grammar
  - `error.c/h` - Clang-style error reporting
  - `dialect.c/h` - Dialect configuration system
  - `symtab.c` - Symbol table management
  - `types.c` - Type system
  - `builtins.c` - Built-in functions
  - `pass1.h` - Frontend data structures

#### D. Fortran Compiler (f77/)
- **Status**: Legacy, partial support
- **Location**: `/home/user/pcc/f77/`

### Supported Backend Architectures (18+)

Each architecture has its own code generation module at `/home/user/pcc/arch/{ARCH}/`:

| Architecture | Files | Status | Notes |
|-------------|-------|--------|-------|
| AMD64 (x86-64) | code.c, local.c, local2.c, table.c | Complete | Modern, most tested |
| i386 (x86-32) | code.c, local.c, local2.c, table.c | Complete | Extensive DOS/Windows support |
| i86 (16-bit x86) | code.c, local.c, local2.c, table.c | Complete | DOS, BIOS support |
| ARM | code.c, local.c, local2.c, table.c | Complete | ARM32 and variants |
| MIPS | code.c, local.c, local2.c, table.c | Complete | MIPS32/MIPS64 |
| PowerPC | code.c, local.c, local2.c, table.c | Complete | PPC32/PPC64 |
| SPARC64 | code.c, local.c, local2.c, table.c | Complete | UltraSPARC |
| HPPA | code.c, local.c, local2.c, table.c | Complete | HP PA-RISC |
| VAX | code.c, local.c, local2.c, table.c | Complete | Classic DEC VAX |
| PDP-11 | code.c, local.c, local2.c, table.c | Complete | Classic Unix |
| PDP-7 | code.c, local.c, local2.c, table.c | Complete | Early Unix |
| PDP-10 | code.c, local.c, local2.c, table.c | Complete | Classic DEC system |
| M68K | code.c, local.c, local2.c, table.c | Complete | Motorola 68000 |
| M16C | code.c, local.c, local2.c, table.c | Complete | Renesas 16-bit |
| NOVA | code.c, local.c, local2.c, table.c | Complete | Data General |
| WebAssembly (WASM) | code.c, local.c, local2.c, table.c | Complete | Browser/server targets |
| C90 | C source code | Generator | Code generator backend |

### Key Backend Components

**Each arch directory contains**:
- `code.c` - Instruction selection and emitting
- `local.c` - Register allocation and local optimizations
- `local2.c` - Additional code generation passes
- `table.c` - Instruction patterns and machine-specific rules
- `order.c` - Instruction ordering (optional)
- `macdefs.h` - Machine definitions and constants

---

## 3. COMPILER ARCHITECTURE & PIPELINE

### Three-Stage Compilation Pipeline

```
Source Code (.c, .pas, .f90, etc.)
        ↓
    [Frontend/Lexer/Parser]
        ↓ (generates Parse Tree + Symbol Table)
    [Pass 1: Semantic Analysis]
        ↓ (generates Intermediate Representation)
    [MIP: Machine-Independent IR]
        ↓
    [Pass 2: Backend Code Generation]
        ↓ (selects instructions for target arch)
    [Architecture-Specific Code (code.c, local.c, table.c)]
        ↓
    [Assembly Emission]
        ↓ (uses libx86asm for 9 assembly formats)
    [Assembly Output (.s/.asm)]
        ↓
    [External Assembler] (gnu-as, nasm, masm, etc.)
        ↓
    [Object File] (.o/.obj)
        ↓
    [Linker]
        ↓
    [Executable]
```

### The Compiler Driver

**Location**: `/home/user/pcc/cc/cc/cc.c` and `/home/user/pcc/cc/driver/driver.c`

The driver orchestrates the pipeline:
1. Preprocessor (cpp) - handles `#include`, `#define`, etc.
2. Compiler Frontend - syntax/semantic analysis
3. Assembler - converts assembly to object code
4. Linker - links object files to executables

### Key IR/Intermediate Representation

**MIP (Machine-Independent Part)** - Located in `/home/user/pcc/mip/`

Key header files:
- `manifest.h` - Type definitions, node types, operators
- `node.h` - Tree node structures for IR
- `pass2.h` - Pass 2 (codegen) definitions

The IR uses an expression tree representation:
```c
typedef struct p1node {
    int n_op;           // Operator
    TWORD n_type;       // Type
    TWORD n_qual;       // Qualifiers
    union {
        P1ND *_left;    // Left child
        P1ND *_right;   // Right child
    } children;
    // ... more fields
} P1ND;
```

---

## 4. LANGUAGE & FRAMEWORKS USED

### Implementation Languages

**Primary**: C (original PCC core)
- Compiler internals: Pure C
- Symbol table management: C
- Type system: C struct-based

**Modern Enhancements**: C with C++
- Some modern features built as libraries
- Exception handling (SEH) has C++ components
- ABI support uses C++

### Build Tools & Frameworks

- **Yacc/Bison**: Parser generator for grammars
  - C: `cc/ccom/cgram.y` (65KB)
  - Pascal: `pascal/pcom/pgram.y` (11KB)
  
- **Flex/Lex**: Lexical analyzer generator
  - Pascal: `pascal/pcom/scan.l` (7KB)
  
- **Autotools**: Build system
  - configure script for cross-platform support
  - Makefile.in for modular builds

- **Standard Libraries**:
  - stdio.h, stdlib.h, string.h
  - POSIX APIs (unistd.h, sys/types.h)

---

## 5. FRONTEND IMPLEMENTATIONS LOCATION

### Directory Structure

```
/home/user/pcc/
├── cc/                                 # C compiler suite
│   ├── cc/                             # C compiler driver
│   │   └── cc.c (51KB - main driver)
│   ├── ccom/                           # C compiler proper (frontend)
│   │   ├── main.c (10KB - entry point)
│   │   ├── cgram.y (65KB - parser)
│   │   ├── pftn.c (75KB - function/type handling)
│   │   ├── trees.c (73KB - expression tree building)
│   │   ├── symtabs.c - symbol table operations
│   │   ├── init.c - initialization handling
│   │   ├── builtins.c - built-in functions
│   │   ├── inline.c - inline function support
│   │   └── optim.c - first-pass optimizations
│   ├── cxxcom/                         # C++ compiler proper
│   ├── cpp/                            # C preprocessor
│   └── driver/                         # Compiler driver utilities
│       ├── driver.c (20KB)
│       ├── platform.c (10KB)
│       └── cc.c (driver front-end)
│
└── pascal/                             # Pascal compiler suite
    ├── pascal/                         # Driver
    │   └── pascal.c (driver)
    └── pcom/                           # Pascal compiler proper (frontend)
        ├── main.c (4KB - entry point)
        ├── scan.l (7KB - Flex lexer)
        ├── pgram.y (11KB - Yacc parser)
        ├── error.c/h (8KB - error reporting)
        ├── dialect.c/h (14KB - dialect management)
        ├── symtab.c (3KB - symbol table)
        ├── types.c (4KB - type system)
        ├── builtins.c (1.7KB - builtins)
        ├── pass1.h (6KB - data structures)
        └── tests/                      # Test programs
            └── (sample Pascal programs)
```

### How Frontend is Organized

Each frontend has this standard structure:

1. **Entry Point** (`main.c`)
   - Parses command-line arguments
   - Opens source files
   - Initializes subsystems
   - Calls parser
   - Manages output

2. **Lexical Analyzer** (`.l` file with Flex)
   - Tokenizes input stream
   - Returns tokens to parser
   - Tracks line/column numbers
   - Handles dialect-specific syntax

3. **Parser** (`.y` file with Yacc/Bison)
   - Builds abstract syntax tree (AST)
   - Performs syntax validation
   - Calls semantic functions
   - Generates IR for code generation

4. **Semantic Analyzer** (various `.c` files)
   - Symbol table management
   - Type checking
   - Scope management
   - Built-in function definitions
   - IR generation functions

5. **Type System** (`types.c` or similar)
   - Type representations
   - Type compatibility checking
   - Type conversions

6. **Symbol Table** (`symtab.c` or similar)
   - Symbol storage and lookup
   - Scope level tracking
   - Storage class management

7. **Error Handling** (`error.c/h` or integrated)
   - Clang-style diagnostics (in Pascal)
   - Line/column tracking
   - Error recovery

---

## 6. HOW TO IMPLEMENT A DEC MACRO ASSEMBLY FRONTEND

### Phase 1: Project Setup

**1.1 Create Directory Structure**
```bash
mkdir -p /home/user/pcc/dec_macro/macro
mkdir -p /home/user/pcc/dec_macro/mcom  # macro compiler proper
cd /home/user/pcc/dec_macro/mcom
```

**1.2 Create Makefile.in**
Pattern from Pascal:
```makefile
OBJS= main.o error.o scan.o mgram.o symtab.o
DEST=mcom$(EXEEXT)

all: $(DEST)

$(DEST): $(OBJS)
    $(CC) $(LDFLAGS) $(OBJS) -o $@ $(LIBS)

scan.c: scan.l
    $(LEX) -t scan.l > scan.c

mgram.c: mgram.y
    $(YACC) -d mgram.y

clean:
    rm -f $(OBJS) $(DEST) scan.c mgram.c y.tab.h y.tab.c
```

**1.3 Create Main Entry Point (main.c)**
```c
#include <stdio.h>
#include <stdlib.h>
#include "pass1.h"

extern int yyparse(void);
extern FILE *yyin;

int lineno = 1;
char *ftitle = "<stdin>";
FILE *outfile = NULL;

int main(int argc, char **argv) {
    // Parse args
    // Initialize subsystems
    // Call yyparse()
    // Handle errors
    // Output IR to backend
    return 0;
}
```

### Phase 2: Lexical Analysis

**2.1 Create Flex Lexer (scan.l)**

Structure for DEC MACRO:
```lex
%{
#include <stdio.h>
#include "pass1.h"
#include "y.tab.h"
%}

%option noyywrap
%option yylineno

DIGIT [0-9]
LETTER [a-zA-Z]
WHITESPACE [ \t\r]

%%

    /* DEC MACRO keywords */
".MACRO"        { return MACRO; }
".ENDM"         { return ENDM; }
".TITLE"        { return TITLE; }
".IDENT"        { return IDENT_STMT; }
... more keywords ...

    /* Register names (VAX, PDP-11, etc.) */
"R0"|"R1"|...   { return REGISTER; }

    /* Labels */
[A-Za-z_][A-Za-z0-9_]*: { return LABEL; }

    /* Operators and delimiters */
    ... operator rules ...

    /* Comments */
^";".*$         { /* ignore comment */ }

    /* Numbers */
{DIGIT}+        { return NUMBER; }

    /* Identifiers */
[A-Za-z_][A-Za-z0-9_]* { return IDENTIFIER; }

<<EOF>>        { return 0; }

%%
```

Key DEC MACRO lexical features:
- Labels end with colon
- Comments start with semicolon
- Registers: R0-R15 (VAX), R0-R7 (PDP-11)
- Special prefixes: `.MACRO`, `.TITLE`, `.IDENT`
- Hex/octal literals: `^X'...`, `^O'...'`
- String literals: `^A'...'`

### Phase 3: Parsing

**3.1 Create Yacc Grammar (mgram.y)**

```yacc
%{
#include <stdio.h>
#include "pass1.h"

int yylex(void);
void yyerror(char *);
%}

%union {
    int ival;
    char *sval;
    struct symtab *symptr;
}

%token MACRO ENDM TITLE IDENT_STMT REGISTER IDENTIFIER NUMBER
%token COLON SEMICOLON COMMA

%type <symptr> macro_def
%type <sval> identifier

%%

program:
    program_items
    ;

program_items:
    program_items program_item
    | program_item
    ;

program_item:
    macro_definition
    | label statement
    | statement
    ;

macro_definition:
    MACRO identifier parameters ENDM
        { /* Create macro symbol */ }
    ;

statement:
    instruction
    | assembler_directive
    ;

instruction:
    mnemonic operand_list
        { /* Generate IR for instruction */ }
    ;

%%
```

DEC MACRO grammar considerations:
- Macros with parameters
- Labels and jumps
- Instruction formats (varies by target)
- Assembler directives (`.TITLE`, `.IDENT`, etc.)
- Conditional assembly

### Phase 4: Data Structures (pass1.h)

**4.1 Create Frontend Header**

```c
#ifndef PASS1_H
#define PASS1_H

#include "../../mip/manifest.h"

/* Symbol table for macros and labels */
typedef struct symtab {
    char *sname;
    int sclass;     /* MACRO, LABEL, REGISTER, etc. */
    int slevel;
    struct tnode *stype;
    int soffset;
    void *svalue;   /* Macro body, register number, etc. */
    struct symtab *snext;
} SYMTAB;

/* Storage classes */
#define SNULL    0
#define MACRO    1
#define LABEL    2
#define REGISTER 3
#define INSTRUCTION 4

/* Instruction representation */
typedef struct instr {
    int opcode;          /* Instruction type */
    int argc;            /* Argument count */
    void **argv;         /* Argument list */
    struct instr *next;
} INSTR;

/* Global state */
extern int lineno;
extern char *ftitle;
extern FILE *outfile;

/* Functions */
void symtab_init(void);
SYMTAB *lookup(char *name);
SYMTAB *install(char *name, int class);
void emit_instruction(int opcode, ...);
void emit_label(char *name);

#endif
```

### Phase 5: Symbol Table & Type System

**5.1 Create symtab.c**

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

#define HASHSIZE 256
static SYMTAB *symtab[HASHSIZE];

static int hash(const char *name) {
    unsigned int h = 0;
    while (*name)
        h = h * 31 + *name++;
    return h % HASHSIZE;
}

SYMTAB *lookup(char *name) {
    int h = hash(name);
    SYMTAB *sp = symtab[h];
    
    while (sp != NULL) {
        if (strcmp(sp->sname, name) == 0)
            return sp;
        sp = sp->snext;
    }
    return NULL;
}

SYMTAB *install(char *name, int class) {
    int h = hash(name);
    SYMTAB *sp = malloc(sizeof(*sp));
    
    sp->sname = strdup(name);
    sp->sclass = class;
    sp->snext = symtab[h];
    symtab[h] = sp;
    
    return sp;
}

void symtab_init(void) {
    memset(symtab, 0, sizeof(symtab));
}
```

### Phase 6: Error Handling

**6.1 Create error.c/h**

```c
#include <stdio.h>
#include <stdarg.h>
#include "pass1.h"

int nerrors = 0;
int nwarnings = 0;

void error(const char *fmt, ...) {
    va_list ap;
    
    va_start(ap, fmt);
    fprintf(stderr, "%s:%d: error: ", ftitle, lineno);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    
    nerrors++;
    if (nerrors > 20) {
        fprintf(stderr, "too many errors\n");
        exit(1);
    }
}

void warning(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "%s:%d: warning: ", ftitle, lineno);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    va_end(ap);
    nwarnings++;
}
```

### Phase 7: Integration with PCC Backend

**7.1 Key Integration Points**

The frontend must generate IR that the PCC backend understands:

```c
/* Call backend functions */
void myp2tree(P1ND *tree);  /* Send tree to Pass 2 (codegen) */
void ejobcode(int);         /* End of compilation unit */
void bjobcode(void);        /* Beginning of compilation unit */
```

**7.2 Instruction Emission Strategy**

DEC MACRO maps to the target architecture's IR:
```
DEC MACRO mov (register)  →  PCC ASSIGN IR
DEC MACRO jbr label       →  PCC JUMP IR
DEC MACRO clrl register   →  PCC constant 0 IR
```

### Phase 8: Build Integration

**8.1 Add to Root Configure**

In `/home/user/pcc/configure.ac`:
```sh
# Near the end, add:
AC_CONFIG_FILES([dec_macro/Makefile dec_macro/mcom/Makefile])
```

**8.2 Update Root Makefile.in**

Add `dec_macro` to subdirs if appropriate.

---

## 7. RECOMMENDED IMPLEMENTATION APPROACH

### Step-by-Step Implementation Plan

**Week 1: Foundation**
1. Set up directory structure
2. Create Makefile.in
3. Implement command-line parsing (main.c)
4. Create pass1.h with basic data structures

**Week 2: Lexer & Basic Parser**
1. Implement Flex lexer for DEC MACRO syntax
2. Create basic Yacc grammar
3. Test lexer/parser on simple programs
4. Add error reporting

**Week 3: Semantic Analysis**
1. Implement symbol table (macros, labels)
2. Implement type system (registers, operands)
3. Add built-in macros and directives
4. Implement macro expansion

**Week 4: Code Generation**
1. Design IR generation strategy
2. Implement instruction-to-IR mapping
3. Test on different target architectures
4. Optimize for specific backends

**Week 5: Testing & Polish**
1. Create test suite
2. Verify output on multiple architectures
3. Document implementation
4. Performance tuning

### Architecture-Specific Considerations

**For VAX/PDP-11 targets** (most relevant for DEC MACRO):
- Located in `/home/user/pcc/arch/vax/` and `/home/user/pcc/arch/pdp11/`
- Study `code.c` and `table.c` for instruction patterns
- Map DEC MACRO instructions to PCC IR nodes

**For x86 targets** (cross-platform testing):
- Located in `/home/user/pcc/arch/amd64/` and `/home/user/pcc/arch/i86/`
- Use libx86asm for multiple assembly syntax support

---

## 8. KEY FILES TO STUDY

### Reference Implementation (Pascal)
- `/home/user/pcc/pascal/pcom/main.c` - Entry point pattern
- `/home/user/pcc/pascal/pcom/scan.l` - Lexer template
- `/home/user/pcc/pascal/pcom/pgram.y` - Parser template
- `/home/user/pcc/pascal/pcom/error.c/h` - Error handling
- `/home/user/pcc/pascal/pcom/pass1.h` - Data structures

### C Compiler Reference
- `/home/user/pcc/cc/ccom/main.c` - C compiler entry point
- `/home/user/pcc/cc/ccom/cgram.y` - Complex grammar example
- `/home/user/pcc/cc/ccom/pftn.c` - Function handling
- `/home/user/pcc/cc/ccom/trees.c` - Expression tree building

### Backend Reference
- `/home/user/pcc/arch/vax/code.c` - Instruction emission
- `/home/user/pcc/arch/vax/table.c` - Instruction patterns
- `/home/user/pcc/mip/manifest.h` - Type definitions
- `/home/user/pcc/mip/node.h` - IR node structures

### Build System Reference
- `/home/user/pcc/configure.ac` - Build configuration
- `/home/user/pcc/pascal/pcom/Makefile.in` - Makefile template
- `/home/user/pcc/cc/ccom/Makefile.in` - C compiler makefile

---

## 9. PRACTICAL EXAMPLE: COMPILING WITH THE FRAMEWORK

```bash
# Build Pascal compiler
cd /home/user/pcc
./configure
make

# Compile a Pascal program
./pascal/pascal -d iso myprogram.pas

# Compile to specific architecture
./pascal/pascal -march=vax -o myprogram myprogram.pas

# Assembly output
./pascal/pascal -S myprogram.pas > myprogram.s
```

For your DEC MACRO compiler:
```bash
# After implementation
./dec_macro/macro -o myprog.o myprog.mac
./dec_macro/macro -S myprog.mac > myprog.s
./dec_macro/macro -march=vax -march=pdp11 myprog.mac
```

---

## 10. TESTING STRATEGY

### Unit Tests
- Lexer: Token recognition
- Parser: Grammar rules
- Semantic: Symbol table, type checking
- Code Gen: IR generation

### Integration Tests
- End-to-end compilation
- Cross-architecture testing
- Macro expansion
- Directive handling

### Reference Programs
- Create test suite for different macro patterns
- Test macro calls with parameters
- Test conditional assembly
- Verify output on multiple architectures

---

## SUMMARY TABLE

| Aspect | Detail | Location |
|--------|--------|----------|
| **Compiler Type** | Multi-language, multi-target | PCC root |
| **Primary Language** | C (with C++ libraries) | cc/ccom, common/ |
| **IR Format** | Expression trees (P1ND) | mip/ |
| **Frontends** | C, C++, Pascal, Fortran | cc/ccom, cc/cxxcom, pascal/pcom, f77/ |
| **Backends** | 18+ architectures | arch/{amd64,i386,vax,pdp11,...}/ |
| **Build System** | GNU Autotools | configure.ac, Makefile.in |
| **Lexer Tool** | Flex | *.l files |
| **Parser Tool** | Yacc/Bison | *.y files |
| **Frontend Template** | Pascal compiler | pascal/pcom/ |
| **Documentation** | Extensive MD files | root directory |
| **Testing** | Unit and integration | test_*.c files |

