# PCC Compiler Architecture - Comprehensive Overview

## Executive Summary

The Portable C Compiler (PCC) is a modular, multi-pass compiler infrastructure that:

1. **Supports 2 Language Frontends**: C (cc/ccom/) and C++ (cc/cxxcom/)
2. **Targets 18 Architectures**: From modern (x86-64, ARM, WASM) to classic (PDP-11, VAX)
3. **Uses Two-Pass Compilation**: High-level IR (P1ND) → Low-level IR (NODE) → Assembly
4. **Integrates Seamlessly**: Ruby frontend can reuse all 18 backends without modification
5. **Excellent Debug Support**: 24+ debug format families for comprehensive debugging

---

## Directory Structure at a Glance

```
/home/user/pcc/
├── cc/                    # Compiler components
│   ├── ccom/             # C frontend (lexer, parser, semantic analysis)
│   ├── cxxcom/           # C++ frontend
│   ├── cpp/              # Preprocessor
│   ├── driver/           # Driver (manages compilation pipeline)
│   └── cc/               # cc wrapper command
│
├── arch/                 # Architecture-specific backends (18 total)
│   ├── amd64/            # x86-64 (most mature)
│   ├── arm/              # ARM
│   ├── wasm/             # WebAssembly
│   └── [15 more...]      # Classic architectures
│
├── mip/                  # Machine-Independent Part (shared IR framework)
│   ├── manifest.h        # Operator definitions
│   ├── pass2.h          # Code generation framework
│   ├── common.c          # Shared utilities
│   ├── match.c           # Instruction selection
│   ├── regs.c            # Register allocation
│   └── [other utilities] # reader.c, optim2.c, order.c, table.c
│
├── common/               # Shared utilities
│   ├── softfloat.c/.h   # Software floating-point
│   ├── compat.c/.h      # Compatibility layer
│   └── abi/             # ABI definitions
│
└── libx86asm/           # x86 assembly emitter (9 formats, multiple ABIs)
```

---

## Compiler Pipeline

```
Ruby Source (.rb)
    ↓
[Driver] (cc/driver/driver.c)
    ├── Preprocessor (optional)
    ↓
[Front-End] (cc/rcom/ - future)
    ├── Lexer (scan.l)
    ├── Parser (cgram.y) → P1ND IR (high-level)
    ├── Semantic Analysis (trees.c)
    ├── Type Checking & Symbol Resolution
    ↓
[IR Lowering] (mip/common.c)
    ├── myp2tree() function
    ├── Convert P1ND → Serialized IR
    ↓
[Code Generator] (pass 2)
    ├── Read IR (reader.c)
    ├── Register Allocation (regs.c)
    ├── Instruction Selection (match.c + arch/*/table.c)
    ├── Code Generation (arch/*/code.c, arch/*/local2.c)
    ↓
[Assembly Output] (.s file)
    ↓
[System Assembler] (as)
    ↓
[Object File] (.o)
    ↓
[System Linker] (ld)
    ↓
[Executable or Library]
```

---

## Intermediate Representation (IR)

### Pass 1 IR: P1ND (High-Level)
- **Location**: cc/ccom/pass1.h
- **Structure**: Tree of P1ND nodes
- **Contains**: Type info, symbols, semantic structure
- **Operators**: ~80 (PLUS, MINUS, CALL, ASSIGN, etc.)

```c
typedef struct p1node {
    int n_op;              // Operator type
    TWORD n_type;          // Data type
    TWORD n_qual;          // Type qualifiers
    union { char *_name; union dimfun *_df; } n_5;
    struct attr *n_ap;     // Attributes
    union {
        struct { union { P1ND *_left; CONSZ _val; } n_l;
                 union { P1ND *_right; int _rval; struct symtab *_sp; } n_r; } n_u;
        struct { struct flt *_dcon; struct flt *_ccon; };
    } n_f;
} P1ND;
```

### Pass 2 IR: NODE (Low-Level, Architecture-Dependent)
- **Location**: mip/node.h
- **Structure**: Lower-level IR with register info
- **Contains**: Register allocation, addressing modes
- **Operators**: 30-50 per architecture

---

## Key Integration Points for Ruby Frontend

### 1. Frontend Entry Point
**Location**: cc/ccom/main.c
- Shows how to initialize compiler
- How to open input files and manage output
- Where to call the parser (yyparse())

### 2. Lexer Template
**Location**: cc/ccom/scan.l
- Lex/Flex format
- Token definitions
- Keyword and operator definitions
- Literal handling (strings, numbers, etc.)

### 3. Parser Grammar
**Location**: cc/ccom/cgram.y
- Yacc format
- Grammar rules
- Semantic actions building P1ND trees
- Examples of all statement and expression types

### 4. Semantic Analysis
**Location**: cc/ccom/trees.c (1800+ lines)
- Type checking and coercion
- Symbol resolution
- Tree construction (buildtree(), mkty(), block(), etc.)
- Operator handling

### 5. Symbol Table
**Location**: cc/ccom/symtabs.c
- Symbol lookup (lookup())
- Symbol creation (getsymtab())
- Scope management
- Type information storage

### 6. Critical Function: myp2tree()
**Location**: mip/common.c
- Called to transition P1ND → Pass 2 IR
- Writes serialized IR to file
- All Ruby constructs must be lowered here
- This is the KEY INTEGRATION POINT

---

## Architecture Backends Explained

Each backend in `/arch/<arch>/` consists of:

### 1. macdefs.h - Machine Definitions
```c
#define SZCHAR      8           // Character size in bits
#define SZINT       32          // Integer size in bits
#define SZLONG      64          // Long size in bits
#define SZPOINT(t)  64          // Pointer size
#define ALINT       32          // Integer alignment
#define ALLONG      64          // Long alignment
#define MAXREGS     16          // Number of registers
```

### 2. code.c - High-Level Code Generation
- Assembly output functions
- Prologue/epilogue generation
- Assembler format selection
- Entry/exit code generation

### 3. local2.c - Code Generation Details
- Register allocation
- Instruction emission
- Address mode selection
- Stack frame management

### 4. table.c - Instruction Selection Patterns
- Pattern matching rules
- Instruction costs
- Rewrite rules
- Constraints (1000-2000 lines per architecture)

### 5. order.c - Instruction Ordering
- Register dependency ordering
- Memory access ordering

### 6. local.c - Pass 1 Optimizations
- Machine-dependent tree transformations

---

## Build System

**Type**: GNU Autotools (autoconf + automake)

**Main Configuration**: configure.ac

**Key Build Targets**:
- `make all` - Standard build
- `make bootstrap` - Multi-stage bootstrap verification
- `make install` - Install binaries

**Build Artifacts**:
- `ccom` - C compiler
- `cc0`, `cc1` - Split-pass compiler versions
- `rcom` - Ruby compiler (future)

---

## How to Add Ruby Frontend

### Step 1: Create Directory Structure
```bash
mkdir -p /home/user/pcc/cc/rcom
```

### Step 2: Create Core Files
```
cc/rcom/
├── main.c       - Entry point
├── scan.l       - Lexer (Lex/Flex format)
├── cgram.y      - Grammar (Yacc format)
├── pass1.h      - Ruby IR definitions
├── trees.c      - Semantic analysis
├── symtabs.c    - Symbol table
├── pftn.c       - Function/method handling
├── init.c       - Initialization
└── Makefile.in  - Build configuration
```

### Step 3: Key Requirements
1. **P1ND IR Trees**: All Ruby constructs → P1ND nodes
2. **Symbol Table**: Populate struct symtab entries
3. **Type System**: Map Ruby types to TWORD
4. **myp2tree() Compatibility**: Ensure all P1ND operators are handled

### Step 4: Update Build System
1. Edit `configure.ac`: Add Ruby option and detection
2. Edit `cc/Makefile.in`: Add rcom target
3. Create `cc/rcom/Makefile.in`: Mirror ccom structure

### Step 5: Test Integration
1. Compile cgram.y and scan.l
2. Test parsing simple Ruby
3. Verify myp2tree() is called
4. Compile to assembly on each backend
5. Test on multiple architectures

---

## Supported Operators

### Arithmetic
```
PLUS, MINUS, MUL, DIV, MOD
```

### Logical & Bitwise
```
AND, OR, XOR, COMPL, LSHIFT, RSHIFT
```

### Comparison
```
EQ, NE, LT, LE, GT, GE, ULT, ULE, UGT, UGE
```

### Assignment
```
ASSIGN, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, MODEQ
AND_EQ, OR_EQ, XOR_EQ, LSHIFTEQ, RSHIFTEQ
```

### Memory/Addressing
```
DEREF (*p), ADDROF (&x), INDIRECT, OREG, NAME
```

### Control Flow
```
CALL, RETURN, GOTO, LABEL, COMOP (comma operator)
```

### Type Conversion
```
CAST, CONVERT, SCONV
```

---

## Type System

### Atomic Types
```
CHAR, SHORT, INT, LONG, LONGLONG
FLOAT, DOUBLE, LDOUBLE
VOID, FARG (function argument)
STRTY, UNIONTY (struct/union)
```

### Type Modifiers
```
INCREF - add pointer level (e.g., INT → INT*)
Array dimension information
Function prototype information
Attributes (const, volatile, aligned, etc.)
```

### Symbol Table Entry
```c
struct symtab {
    struct symtab *snext;  // Link to next symbol
    int soffset;           // Offset or value
    char sclass;           // Storage class (AUTO, EXTERN, STATIC, etc.)
    char slevel;           // Scope level
    short sflags;          // Flags
    char *sname;           // Symbol name
    TWORD stype;           // Type word
    TWORD squal;           // Qualifiers
    union dimfun *sdf;     // Dimension/function info
    struct attr *sap;      // Attributes
};
```

---

## Critical Files Summary

| File | Lines | Purpose |
|------|-------|---------|
| cc/ccom/cgram.y | 1500+ | C grammar/parser template |
| cc/ccom/scan.l | 650 | Lexer template |
| cc/ccom/pass1.h | 630 | IR structures |
| cc/ccom/trees.c | 1800+ | Semantic analysis patterns |
| cc/ccom/main.c | 200+ | Compiler entry point |
| mip/manifest.h | 200+ | Operator definitions |
| mip/pass2.h | 400+ | Code generation framework |
| mip/common.c | 500+ | myp2tree() and utilities |
| arch/amd64/table.c | 2000+ | x86-64 instruction patterns |
| configure.ac | 500+ | Build configuration |

---

## Development Phases

### Phase 1: Minimal (1-2 weeks)
- Variables, assignments, arithmetic
- Simple functions
- Basic output (puts)

### Phase 2: Control Flow (1 week)
- if/else, while, for loops
- break, next, return

### Phase 3: Functions (1-2 weeks)
- Function definitions with parameters
- Scope and local variables
- Return values

### Phase 4: Data Structures (2 weeks)
- Arrays (basic)
- Hashes (as arrays)
- Strings

### Phase 5: OOP (2-3 weeks)
- Classes and instance variables
- Methods and method dispatch
- Constructors (initialize)
- Inheritance

### Phase 6: Blocks (1-2 weeks)
- Block syntax
- Block parameters
- Common iterators (each, map, select)
- Yield

### Phase 7: Advanced (ongoing)
- Modules and mixins
- Metaclasses and reflection
- String/numeric operators
- Range operators

---

## Testing Strategy

### Test Levels

1. **Lexer/Parser**: Parse Ruby code → AST
2. **IR Generation**: AST → P1ND trees → myp2tree() called
3. **Backend**: P1ND → Assembly generation
4. **Integration**: Full pipeline (Ruby → Executable)
5. **Multi-arch**: Test on amd64, ARM, WASM

### Test Files
```
cc/rcom/tests/
├── test_arithmetic.rb
├── test_functions.rb
├── test_loops.rb
├── test_classes.rb
├── test_blocks.rb
└── test_multiarch.sh
```

---

## Success Criteria

1. **Parser Works**: Can parse Ruby code without errors
2. **IR Generation**: P1ND trees created correctly
3. **Code Generation**: Compiles to assembly for all backends
4. **Execution**: Generated code runs correctly
5. **Multi-arch**: Same Ruby code compiles to amd64, ARM, WASM
6. **Feature Coverage**: Incrementally add Ruby features

---

## Key Files to Read First

1. `/home/user/pcc/cc/ccom/main.c` - Compiler structure
2. `/home/user/pcc/cc/ccom/scan.l` - Lexer patterns
3. `/home/user/pcc/cc/ccom/cgram.y` - Grammar examples
4. `/home/user/pcc/cc/ccom/pass1.h` - IR structures
5. `/home/user/pcc/configure.ac` - Build system
6. `/home/user/pcc/mip/common.c` - myp2tree() function
7. `/home/user/pcc/arch/amd64/macdefs.h` - Backend example

---

## Architecture Strengths

1. **Multi-Pass Design**: Clean separation of concerns
2. **Portable IR**: Same IR supports 18 architectures
3. **Modular**: Each component has clear responsibilities
4. **Extensible**: Easy to add languages and targets
5. **Battle-Tested**: Original PCC is 40+ years old
6. **Debug Support**: Comprehensive debug symbol formats
7. **ABI Support**: Multiple calling conventions and ABIs

---

## Notes for Ruby Integration

1. **IR Compatibility**: Ruby constructs must map to P1ND operators
2. **Type System**: Ruby dynamic types → Target static types
3. **Blocks**: Convert to function pointers or special nodes
4. **Methods**: Map to function calls with receiver
5. **Classes**: Handle through initialization code
6. **Performance**: Can use backend optimizations

---

## Additional Resources

- **Original PCC**: http://pcc.ludd.ltu.se/
- **Bootstrap Docs**: /home/user/pcc/BOOTSTRAP.md
- **WASM Backend**: /home/user/pcc/arch/wasm/
- **Debug Symbols**: /home/user/pcc/cc/ccom/DEBUGSYM_README.md
- **Pragma Support**: /home/user/pcc/WATCOM_PRAGMAS.md

---

**Document Generated**: 2025-10-26
**Compiler Version**: PCC 1.2.0.DEVEL with enhancements
**Target**: Ruby frontend design and integration

