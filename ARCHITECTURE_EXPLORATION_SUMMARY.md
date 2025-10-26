# PCC Compiler Architecture Exploration - Complete Summary

## Overview

This document summarizes the comprehensive exploration of the Portable C Compiler (PCC) architecture, which is essential for designing and implementing a Ruby front-end that integrates properly with the compiler infrastructure.

**Exploration Date**: October 26, 2025
**Thoroughness Level**: Medium
**Generated Documents**: 4 comprehensive guides

---

## Key Findings

### 1. Overall Compiler Architecture

The PCC is a **modular, two-pass compiler** with clean separation of concerns:

- **Pass 1 (Front-End)**: Lexical analysis → Parsing → Semantic analysis → P1ND IR
- **Pass 2 (Code Generation)**: Register allocation → Instruction selection → Assembly generation

**Key Insight**: A Ruby front-end only needs to produce valid P1ND IR trees. All 18 backends automatically work without any modifications.

### 2. Directory Structure

```
/home/user/pcc/
├── cc/                  # Compiler components (2 languages: C, C++)
├── arch/                # 18 architecture backends (amd64, ARM, WASM, etc.)
├── mip/                 # Machine-Independent Part (shared IR framework)
├── common/              # Utilities (softfloat, unicode, ABI defs)
└── libx86asm/           # X86 assembly emitter (9 formats)
```

### 3. Front-Ends (Existing)

**C Frontend** (`cc/ccom/`)
- **Files**: cgram.y (parser), scan.l (lexer), trees.c (semantic), pass1.h (IR defs)
- **Output**: P1ND IR trees (high-level, ~80 operators)
- **Size**: ~1500 lines grammar, 650 lines lexer, 1800 lines semantic analysis

**C++ Frontend** (`cc/cxxcom/`)
- Similar structure with C++ extensions
- Same IR output (P1ND)

**Ruby Frontend** (To be created: `cc/rcom/`)
- Should follow exact same pattern as C frontend
- Produce P1ND IR compatible with all 18 backends

### 4. Intermediate Representation (IR)

**Pass 1 IR: P1ND (High-Level)**
- **Structure**: Tree of nodes with:
  - `n_op`: Operator type (~80 types like PLUS, CALL, ASSIGN)
  - `n_type`: Data type (CHAR, INT, FLOAT, etc.)
  - `n_left`, `n_right`: Child nodes
  - `n_sp`: Symbol pointer
  - Type qualifiers and attributes

**Pass 2 IR: NODE (Low-Level)**
- Generated automatically from P1ND
- Architecture-specific details (registers, addressing modes)
- 30-50 operators per architecture

### 5. Architecture Backends

**18 Supported Architectures**:
- **Modern**: amd64, ARM, WASM
- **Classic RISC**: MIPS, MIPS64, PowerPC, SPARC64, PA-RISC, PDPll
- **Classic CISC**: i386, i86, Motorola 68k, VAX
- **Historic**: PDP-7, PDP-10, Nova, M16C

**Each Backend Has 6 Components**:
1. `macdefs.h` - Type sizes, alignment, registers, ABI
2. `code.c` - High-level code generation
3. `local2.c` - Register allocation and instruction emission
4. `table.c` - Instruction selection patterns (1000-2000 lines)
5. `order.c` - Instruction ordering
6. `local.c` - Pass 1 optimizations

### 6. Build System

- **Type**: GNU Autotools (autoconf + Makefile)
- **Key Files**: configure.ac, cc/Makefile.in, cc/ccom/Makefile.in
- **Build Process**:
  1. Generate mkext utility
  2. Generate parser (yacc) and lexer (lex)
  3. Compile source files
  4. Link to produce compiler executable
- **Output**: ccom, cc0, cc1 binaries (and future: rcom)

### 7. Critical Integration Point

**Function**: `myp2tree()` in `/home/user/pcc/mip/common.c`

This is THE KEY FUNCTION where:
- P1ND IR trees from Ruby frontend are **lowered** to Pass 2 IR
- Output is **serialized** to a temporary file
- All Ruby constructs must be expressible in P1ND operators

### 8. Supported Operators (~80 types)

**Categories**:
- Arithmetic: PLUS, MINUS, MUL, DIV, MOD
- Logical: AND, OR, NOT, ANDAND, OROR
- Comparison: EQ, NE, LT, LE, GT, GE
- Assignment: ASSIGN, PLUSEQ, MINUSEQ, etc.
- Memory: DEREF, ADDROF, INDIRECT, NAME
- Control: CALL, RETURN, GOTO, LABEL
- Conversion: CAST, CONVERT

All operators are backend-agnostic.

### 9. Type System

**Atomic Types**: CHAR, SHORT, INT, LONG, LONGLONG, FLOAT, DOUBLE, LDOUBLE
**Compound Types**: Pointers (INCREF), Arrays (dimension info), Structs/unions
**Type Words (TWORD)**: 32-bit integer encoding type and qualifiers

### 10. Symbol Table

**struct symtab**: Stores symbol information
- `sname`: Symbol name
- `stype`: Type word
- `sclass`: Storage class (AUTO, EXTERN, STATIC, etc.)
- `soffset`: Offset or value
- `slevel`: Scope level (for nested scopes)
- `sflags`: Flags for special attributes

---

## Ruby Front-End Design Implications

### What Must Be Done

1. **Create `/home/user/pcc/cc/rcom/`** directory structure
2. **Implement Scanner** (`scan.l`): Tokenize Ruby source
3. **Implement Parser** (`cgram.y`): Parse tokens to AST, build P1ND trees
4. **Implement Semantic Analysis** (`trees.c`): Type checking, tree ops
5. **Implement Symbol Table** (`symtabs.c`): Symbol management
6. **Update Build System**: Add rcom to configure.ac and Makefile.in
7. **Test Integration**: Verify P1ND → All backends works

### What You Get For Free

- **Code Generation**: All backends automatically work
- **Register Allocation**: Handled by mip/regs.c
- **Instruction Selection**: Handled by match.c + architecture tables
- **Assembly Output**: Handled by architecture-specific code.c
- **Assembler/Linker**: Uses system tools (as, ld)
- **Debug Symbols**: 24+ format families available
- **Optimization**: Pass 1 and Pass 2 optimizations included

### Key Design Decisions

1. **Ruby Blocks**:
   - Option 1: Convert to function pointers (simpler)
   - Option 2: Create special block IR nodes
   - Both work with existing backends

2. **Ruby Methods**:
   - Map to function calls with implicit receiver parameter
   - Use CALL operator in P1ND IR

3. **Ruby Classes**:
   - Handle via initialization code (struct descriptors)
   - Instance methods → regular functions with receiver
   - Class methods → static functions

4. **Type System**:
   - Ruby's dynamic types → static types at compile time
   - Type inference for optimization
   - Some runtime dispatch if needed

---

## Documentation Files Generated

All files are saved in `/home/user/pcc/`:

1. **COMPILER_OVERVIEW.md** (13 KB)
   - Comprehensive one-document overview
   - All key information in markdown format
   - Good for quick reference and understanding

2. **arch_summary.txt** (18 KB)
   - Detailed architecture breakdown
   - Directory structure with full descriptions
   - All compiler stages explained
   - IR format details with code
   - Backend implementation guide

3. **architecture_diagram.txt** (13 KB)
   - Visual ASCII diagrams
   - Pipeline flowcharts
   - Data flow through IR layers
   - Compilation modes
   - Architecture customization points

4. **ruby_frontend_guide.txt** (18 KB)
   - Step-by-step Ruby frontend implementation guide
   - 13 detailed steps from directory setup to testing
   - Code skeletons and templates
   - Grammar and lexer examples
   - Build configuration templates
   - Development roadmap (7 phases)
   - Debugging infrastructure guide

---

## Quick Start for Ruby Frontend Development

### Phase 0: Understanding (1 day)
```bash
cd /home/user/pcc

# Read in order:
1. COMPILER_OVERVIEW.md         # High-level overview
2. arch_summary.txt              # Detailed architecture
3. ruby_frontend_guide.txt       # Step-by-step guide

# Study existing code:
1. cc/ccom/main.c               # Compiler entry point
2. cc/ccom/scan.l               # Lexer template
3. cc/ccom/cgram.y              # Grammar template
4. cc/ccom/pass1.h              # IR definitions
5. mip/common.c                 # myp2tree() function
```

### Phase 1: Setup (1 day)
```bash
# Create directory structure
mkdir -p /home/user/pcc/cc/rcom

# Copy template files from ccom
cp cc/ccom/Makefile.in cc/rcom/
cp cc/ccom/main.c cc/rcom/
# ... copy other templates and adapt for Ruby
```

### Phase 2: Minimal Lexer/Parser (2-3 days)
- Create `scan.l` with Ruby keywords
- Create `cgram.y` with basic Ruby grammar
- Test parsing simple Ruby code
- Build P1ND trees for basic constructs

### Phase 3: Integration (1-2 days)
- Call myp2tree() to lower to Pass 2 IR
- Verify serialized IR is generated
- Test compilation through amd64 backend

### Phase 4: Expansion (ongoing)
- Add more Ruby features incrementally
- Test on multiple backends (ARM, WASM)
- Build test suite

---

## Key Files to Study

### Must Read
1. `/home/user/pcc/cc/ccom/main.c` - Compiler structure (200 lines)
2. `/home/user/pcc/cc/ccom/scan.l` - Lexer patterns (650 lines)
3. `/home/user/pcc/cc/ccom/cgram.y` - Grammar rules (1500 lines)
4. `/home/user/pcc/cc/ccom/pass1.h` - IR structures (630 lines)

### Should Read
5. `/home/user/pcc/mip/common.c` - myp2tree() (500 lines)
6. `/home/user/pcc/configure.ac` - Build config (500 lines)
7. `/home/user/pcc/arch/amd64/macdefs.h` - Backend example (150 lines)

### Reference
8. `/home/user/pcc/cc/ccom/trees.c` - Semantic analysis (1800 lines)
9. `/home/user/pcc/cc/ccom/symtabs.c` - Symbol table (300 lines)
10. `/home/user/pcc/arch/amd64/table.c` - Instruction patterns (2000 lines)

---

## Architecture Strengths for Language Design

1. **Simplicity**: Clear two-pass structure, easy to understand
2. **Modularity**: Each component has single responsibility
3. **Extensibility**: Easy to add new languages and targets
4. **Battle-Tested**: Original PCC is 40+ years old, proven architecture
5. **Portable**: Same IR works across 18 architectures
6. **Well-Documented**: Source is readable, good examples exist
7. **Debug Support**: Comprehensive debug symbol formats
8. **Multi-ABI**: Multiple calling conventions supported

---

## Success Criteria

Your Ruby frontend will be successful when:

1. Parser can parse Ruby code without errors
2. P1ND IR trees are correctly generated
3. myp2tree() is called and IR is serialized
4. Code generation works (Ruby → Assembly)
5. Compilation succeeds on amd64, ARM, WASM
6. Generated executables run correctly
7. Features are incrementally added and tested

---

## Next Steps

1. **Review Documentation**: Read the 4 generated documents
2. **Study Existing Code**: Examine C frontend implementation
3. **Set Up Directory**: Create cc/rcom/ structure
4. **Start Small**: Implement minimal lexer/parser first
5. **Test Early**: Integrate with backends as soon as possible
6. **Iterate**: Add features incrementally, test continuously

---

## Additional Resources

- Original PCC: http://pcc.ludd.ltu.se/
- Bootstrap documentation: `/home/user/pcc/BOOTSTRAP.md`
- WASM backend: `/home/user/pcc/arch/wasm/` (reference modern backend)
- Debug symbols: `/home/user/pcc/cc/ccom/DEBUGSYM_README.md`
- Pragma support: `/home/user/pcc/WATCOM_PRAGMAS.md`

---

## Summary

The PCC compiler is an excellent foundation for adding a Ruby frontend:

- **Architecture is clean** and well-separated into phases
- **IR design** is proven and battle-tested
- **Backends are ready** (18 of them!)
- **Documentation exists** in code form
- **Extensibility** is built-in

By following the patterns established in the C frontend and using the provided guides, you can successfully integrate Ruby compilation into PCC, targeting all modern and classic architectures.

**Key Insight**: You only need to create P1ND IR trees from Ruby code. Everything else (code generation, optimization, assembly, linking) is already implemented.

---

**Exploration Complete**: October 26, 2025
**Total Documentation Generated**: 62 KB across 4 files
**Key Files Analyzed**: 30+
**Backends Documented**: 18
**Key Integration Points Identified**: 6+

