# PCC (Portable C Compiler) Codebase Architecture Analysis

## Executive Summary

PCC is a well-structured compiler with a clear separation between:
- **Frontend (Pass 1)**: Parsing and semantic analysis
- **Backend (Pass 2)**: Code generation and optimization
- **Architecture-specific code**: Pluggable backend implementations

The compiler supports 16 different target architectures and multiple operating systems through a modular, table-driven design.

## 1. Overall Directory Structure and Organization

```
/home/user/pcc/
├── arch/                    # Architecture-specific backends (16 targets)
│   ├── amd64/              # x86-64 backend
│   ├── arm/                # ARM backend
│   ├── i386/               # x86 (32-bit) backend
│   ├── mips/               # MIPS backend
│   ├── powerpc/            # PowerPC backend
│   ├── sparc64/            # SPARC-64 backend
│   ├── m68k/               # Motorola 68k
│   ├── vax/                # VAX
│   └── [10 more architectures...]
├── cc/                     # C compiler frontend
│   ├── cc/                 # Driver
│   ├── ccom/               # C compiler (pass 1)
│   ├── cpp/                # C preprocessor
│   ├── cxxcom/             # C++ compiler (pass 1)
│   └── driver/             # Compiler driver
├── mip/                    # Middle-level Intermediate Processor (pass 2)
│   ├── pass2.h             # Pass 2 definitions
│   ├── manifest.h          # Node type definitions
│   ├── node.h              # AST node structure
│   ├── match.c             # Instruction matching
│   ├── reader.c            # Intermediate format reader
│   ├── regs.c              # Register allocation
│   ├── common.c            # Common code generation
│   └── optim2.c            # Pass 2 optimizations
├── os/                     # OS-specific code
│   ├── linux/              # Linux configuration
│   ├── freebsd/            # FreeBSD configuration
│   └── [20+ OS variants...]
├── common/                 # Shared utilities
│   ├── compat.c            # Compatibility functions
│   ├── softfloat.c         # Software floating point
│   └── unicode.c           # Unicode support
└── configure.ac            # Autoconf configuration (maps OS/CPU to targets)
```

## 2. Supported Architectures

Currently implemented backends (in `/home/user/pcc/arch/`):

| Architecture | Directory | Type | Status |
|---|---|---|---|
| AMD64 (x86-64) | amd64/ | Modern 64-bit | Full |
| ARM | arm/ | Modern 32-bit | Full |
| I386 (x86) | i386/ | Legacy 32-bit | Full |
| MIPS | mips/ | Embedded | Full |
| MIPS64 | mips64/ | Embedded 64-bit | Full |
| PowerPC | powerpc/ | Embedded/Legacy | Full |
| SPARC-64 | sparc64/ | Server | Full |
| M68k (Motorola 68k) | m68k/ | Vintage | Full |
| VAX | vax/ | Vintage | Full |
| HPPA | hppa/ | Server | Full |
| I86 (8086) | i86/ | Vintage | Full |
| M16C | m16c/ | Microcontroller | Full |
| Nova | nova/ | Vintage | Full |
| PDP7 | pdp7/ | Vintage | Full |
| PDP10 | pdp10/ | Vintage | Full |
| PDP11 | pdp11/ | Vintage | Full |

## 3. Architecture-Specific Backend Structure

Each architecture directory contains exactly 6 files (for most architectures):

### File Purposes:

1. **macdefs.h** - Machine Definitions Header
   - Type sizes (SZCHAR, SZINT, SZLONG, etc.)
   - Type alignments (ALCHAR, ALINT, etc.)
   - Min/max values for types
   - Register definitions and constants
   - **Example**: `/home/user/pcc/arch/amd64/macdefs.h` defines amd64 as 64-bit little-endian

2. **table.c** - Instruction Selection Table
   - Contains `struct optab table[]` - mapping of IR nodes to machine instructions
   - ~1,000-1,800 lines per architecture
   - Each entry specifies:
     - Operator type (PLUS, MUL, SCONV, etc.)
     - Target "cookie" (INAREG, FORCC, FOREFF)
     - Left operand shape and type constraints
     - Right operand shape and type constraints
     - Resource requirements (registers needed)
     - Rewrite rules
     - Assembly instruction template
   - **Example**: amd64/table.c has 30,066 bytes of instruction templates

3. **code.c** - Code Generation (Pass 1)
   - Called `bjobcode()` - beginning of job code
   - Called `ejobcode()` - end of job code
   - Called `bfcode()` - beginning of function code
   - Called `efcode()` - end of function code
   - Generates function prologue/epilogue
   - Handles calling conventions and argument passing
   - Emits architecture-specific assembly
   - **Example**: amd64/code.c handles System V AMD64 ABI calling convention

4. **local.c** - Local Optimizations (Pass 1)
   - Declarations of architecture-specific passes
   - Symbol handling and labeling
   - Section directives (.text, .data, etc.)
   - Symbol export/visibility directives
   - Called for each external symbol
   - **Example**: amd64/local.c emits ELF-specific directives and PIC support

5. **local2.c** - Local Optimizations (Pass 2)
   - Register allocation and assignment
   - Instruction scheduling
   - Peephole optimizations
   - Assembly output formatting
   - Called during instruction matching phase
   - **Example**: amd64/local2.c (~25,158 bytes) handles register color assignment

6. **order.c** - Expression Evaluation Order (Pass 2)
   - Sethi-Ullman numbers calculation
   - Determines optimal evaluation order for expressions
   - Influences register allocation decisions
   - Smaller files (4,000-8,000 bytes)
   - **Example**: amd64/order.c defines register priority and cost metrics

## 4. Code Generation Pipeline and Intermediate Representation

### Multi-Pass Compilation Flow:

```
Source C Code
    ↓
[Pass 1: Frontend - cc/ccom/]
  - Lexical analysis (scan.l)
  - Parsing (cgram.y)
  - Semantic analysis
  - Type checking
  - Optimizations (optim.c)
  - Generate intermediate format (trees)
    ↓
[Intermediate Format]
  - Text-based format output to stdout
  - Each construct encoded as:
    * '&' - filename change
    * '#' - line number
    * '"' - NODE (tree node)
    * '^' - label definition
    * '!' - function prologue
    * '%' - function epilogue
    * '*' - passthrough line
    ↓
[Pass 2: Backend - mip/]
  - Read intermediate format (reader.c)
  - Build SSA/control flow graphs (regs.c)
  - Instruction matching (match.c)
  - Register allocation (regs.c)
  - Optimize (optim2.c)
  - Generate assembly (arch-specific code.c, local2.c)
    ↓
Assembly Output
    ↓
[Assembler/Linker]
```

### Intermediate Representation (IR) Structure:

**NODE Structure** (`mip/node.h`):
```c
typedef struct node {
    int n_op;                  // Operator type (PLUS, MUL, CALL, etc.)
    int n_reg;                 // Register assignment
    TWORD n_type;              // Type word
    TWORD n_qual;              // Type qualifiers
    int n_su;                  // Size/usage flags
    char *n_name;              // Name for NAME nodes
    int n_label;               // Label for label nodes
    struct attr *n_ap;         // Attributes
    struct node *n_left;       // Left child (for binary ops)
    struct node *n_right;      // Right child (for binary ops)
    CONSZ n_val;               // Constant value
} NODE;
```

**Operator Types** (MAXOP=58):
- Leaf nodes: NAME, ICON (int const), FCON (float const), REG, OREG, TEMP
- Arithmetic: PLUS, MINUS, MUL, DIV, MOD
- Bitwise: AND, OR, ER (XOR), LS (left shift), RS (right shift)
- Logic: EQ, NE, LE, LT, GE, GT, ULE, ULT, UGE, UGT
- Control: CBRANCH, GOTO, RETURN
- Special: CALL, ASSIGN, SCONV, PCONV, ADDROF, XASM

### Instruction Selection Mechanism:

The **table.c** files implement pattern matching:
```c
struct optab {
    int op;           // Operator to match
    int goal;         // Target (INAREG, FORCC, etc.)
    int lshape;       // Left operand shape (SAREG, SOREG, etc.)
    int ltype;        // Left operand type
    int rshape;       // Right operand shape
    int rtype;        // Right operand type
    int rewrite;      // Cost/rewrite rules
    int result;       // Result specification
    char *code;       // Assembly instruction template
};
```

Template matching uses:
- **Shapes** - operand location classes (SAREG=reg, SOREG=offset reg, SNAME=symbol, etc.)
- **Types** - type constraints (TINT, TFLOAT, TPOINT, etc.)
- **Cookies** - target goals (INAREG, INBREG, FORCC, FOREFF)

## 5. Target Platform Configuration

### Configuration Flow (configure.ac):

The build system (`configure.ac`) maps target triples to backend selection:

```
Target Triple (e.g., x86_64-linux-gnu)
    ↓
Target OS (targos) + Target Machine (targmach)
    ↓
[Example]
    linux-gnu + x86_64 → targos=linux, targmach=amd64
    freebsd + arm → targos=freebsd, targmach=arm
    openbsd + sparc64 → targos=openbsd, targmach=sparc64
    ↓
Backend Selection
    MDIR = $(top_srcdir)/arch/$(TARGMACH)
    targmach determines which arch/ directory is used
```

### Makefile Configuration:

The build system passes target info as C preprocessor flags:
```makefile
CPPFLAGS = ... -Dos_$(TARGOS) -Dmach_$(TARGMACH) ...
```

This allows conditional compilation:
```c
#ifdef mach_amd64
    // amd64-specific code
#elif defined(mach_arm)
    // ARM-specific code
#endif

#ifdef os_linux
    // Linux-specific code
#elif defined(os_freebsd)
    // FreeBSD-specific code
#endif
```

### Build Targets:

- **CCOM**: Single-pass compiler (cc/cc → direct assembly)
- **CC0**: Pass 1 only (cc → intermediate format)
- **CC1**: Pass 2 only (intermediate format → assembly)
- **CF0/CF1**: Feature flags controlling optimizations

## 6. Main Entry Points and Compilation Flow

### Pass 1 Entry Point: `/home/user/pcc/cc/ccom/main.c`

```c
main()
{
    // 1. Parse command line
    // 2. Initialize (mkdope() - operator attributes)
    
    #ifndef PASS2
    lineno = 1;
    bjobcode();          // Architecture-specific job init
    
    // 3. Parse and process C code (yyparse from cgram.y)
    
    #ifdef PASS2
    mainp2();            // Pass 2 processing
    #endif
    
    ejobcode();          // Architecture-specific cleanup
}
```

### Pass 2 Entry Point: `/home/user/pcc/mip/reader.c`

```c
mainp2()
{
    while ((p = rdline()) != NULL) {
        // Parse intermediate format
        switch (*b) {
            case '"':  // NODE
                ip->ip_node = rdnode(b);
                pass2_compile(ip);
                break;
            case '!':  // Function prologue
                pass2_compile(ipp);
                break;
            case '%':  // Function epilogue
                pass2_compile(ipp);
                break;
        }
    }
}
```

### Code Generation (match.c):

```c
pass2_compile(struct interpass *ip)
{
    // 1. Instruction matching (match.c)
    //    - Find best rule in table[] that matches tree
    //    - Calculate costs (Sethi-Ullman)
    //    - Generate assignment code
    
    // 2. Register allocation (regs.c)
    //    - Graph coloring algorithm
    //    - Assign physical registers
    
    // 3. Optimization (optim2.c)
    //    - Redundancy elimination
    //    - Peephole optimization
    
    // 4. Output (arch-specific code.c, local2.c)
    //    - printf() to stdout
}
```

## 7. Example Backend File Sizes and Complexity

```
amd64 Backend:
  - code.c:      30,215 bytes (function prologue/epilogue, calling convention)
  - local.c:     20,370 bytes (symbol handling, ELF directives, PIC support)
  - local2.c:    25,158 bytes (register allocation, peephole optimization)
  - table.c:     30,066 bytes (1,200+ instruction selection rules)
  - order.c:      8,044 bytes (evaluation order, cost metrics)
  Total:        ~114,000 bytes for backend

arm Backend:
  - code.c:      30,000+ bytes
  - local.c:     ~20,000 bytes
  - local2.c:    ~25,000 bytes
  - table.c:     ~30,000 bytes
  - order.c:     ~8,000 bytes
  Total:        ~113,000 bytes for backend
```

## 8. How to Add a New CLR Backend

### High-Level Steps:

1. **Create Architecture Directory**
   ```bash
   mkdir /home/user/pcc/arch/clr
   ```

2. **Create macdefs.h** - Machine definitions
   - Define type sizes (SZINT, SZLONG, etc.)
   - Define type alignments
   - Define register constants
   - Define calling convention offsets
   - Define pointer/float representation

3. **Create code.c** - Code generation
   - Implement `bjobcode()` - generate CLR assembly preamble
   - Implement `efcode()` - function epilogue
   - Implement `bfcode()` - function prologue (handle argument passing)
   - Implement `ejobcode()` - cleanup code
   - Emit CLR IL (Intermediate Language) instructions
   - Handle stack frame setup and calling conventions

4. **Create local.c** - Symbol/section handling
   - Implement `setseg()` - emit .NET assembly directives
   - Handle symbol declarations (.method, .field, etc.)
   - Generate .NET assembly metadata
   - Support PIC/position-independent code if needed

5. **Create local2.c** - Register allocation and optimization
   - Implement register assignment (or map to CLR virtual stack)
   - Handle peephole optimizations for CLR IL
   - Implement stack slot allocation

6. **Create table.c** - Instruction selection
   - Define `struct optab table[]` with CLR IL instructions
   - Map C operators to CLR IL operations:
     - PLUS → add
     - MUL → mul
     - CALL → call (virtual method calls)
     - RETURN → ret
     - etc.
   - Define operand shapes (registers, constants, etc.)
   - Approximately 1,000-1,500 entries

7. **Create order.c** - Evaluation order
   - Define Sethi-Ullman costs for CLR IL operations
   - Specify register priority (though CLR uses virtual stack)
   - Control expression evaluation order for optimal stack usage

8. **Update configure.ac**
   - Add pattern matching for CLR target
   - Example: `*clr*) targos=clr; targmach=clr ;;`

9. **Update Makefile.in files**
   - Files are automatically picked up from arch/clr/
   - MDIR is set based on targmach variable

### Key Architectural Considerations for CLR:

1. **CLR IL is Stack-Based**
   - No traditional register allocation needed
   - Instead, use evaluation stack
   - Map register concepts to stack slots

2. **Type System**
   - CLR IL has strong type system
   - Need to emit proper type annotations for operations
   - Handle .NET primitive types (int32, int64, float64, etc.)

3. **Calling Convention**
   - CLR handles calling conventions automatically
   - Focus on passing arguments and returning values
   - Use `call` instruction for method calls

4. **Memory Access**
   - Use `ldsfld`/`stsfld` for static fields
   - Use `ldfld`/`stfld` for instance fields
   - Use `ldloc`/`stloc` for local variables

5. **Assembly Structure**
   - Emit `.assembly` directives
   - Emit `.namespace` declarations
   - Emit `.class` definitions
   - Emit `.method` definitions within classes

### Minimal table.c Entry Example (CLR):

```c
struct optab table[] = {
    // First entry must be empty
    { -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },
    
    // Integer addition
    { PLUS, INAREG,
        SAREG, TINT,
        SAREG, TINT,
        NAREG, RESC1,
        "add\n", },
    
    // Integer constant to register
    { ICON, INAREG,
        SCON, TINT,
        SANY, TANY,
        0, RESC1,
        "ldc.i4 AL\n", },
    
    // Memory load
    { NAME, INAREG,
        SNAME, TINT,
        SANY, TANY,
        0, RESC1,
        "ldsfld AL\n", },
    
    // Store to memory
    { ASSIGN, FOREFF,
        SNAME, TINT,
        SAREG, TINT,
        0, RLEFT,
        "stsfld AL\n", },
};
```

## 9. Key Files for Understanding the System

| File | Purpose | Lines |
|---|---|---|
| `/home/user/pcc/mip/node.h` | AST node structure | 228 |
| `/home/user/pcc/mip/pass2.h` | Pass 2 definitions | 16,252 |
| `/home/user/pcc/mip/manifest.h` | Node and type definitions | 10,802 |
| `/home/user/pcc/mip/match.c` | Instruction matching algorithm | 28,819 |
| `/home/user/pcc/mip/reader.c` | Intermediate format reader | 42,041 |
| `/home/user/pcc/mip/regs.c` | Register allocation | 73,732 |
| `/home/user/pcc/configure.ac` | Build configuration | 17,749 |
| `/home/user/pcc/cc/ccom/Makefile.in` | Compiler build rules | Shows backend integration |

## 10. Testing Strategy for New Backend

1. **Start with simple operations**
   - Integer constants, addition, subtraction
   - Variable assignment and access

2. **Test compilation stages**
   - Verify table.c entries match compiler expectations
   - Check code.c prologue/epilogue generation
   - Validate local.c symbol output

3. **Use existing test programs**
   - Build and compile simple C programs
   - Compare output format with reference backend (e.g., amd64)

4. **Incremental development**
   - Focus on core operators first (PLUS, MINUS, MUL)
   - Add control flow (CBRANCH, GOTO)
   - Add function calls (CALL, RETURN)
   - Add complex type conversions (SCONV, PCONV)

## Summary

PCC's architecture supports adding new backends through:

1. **Modular Design**: Each arch/ directory is self-contained
2. **Table-Driven Code Generation**: Pattern matching via optab tables
3. **Clear Interfaces**: Well-defined functions (code.c, local.c, etc.)
4. **Configuration System**: Automatic backend selection via configure.ac
5. **Build Integration**: Makefile automatically includes backend files

For a CLR backend, focus on:
- Understanding CLR IL instruction set
- Mapping C operators to CLR IL operations
- Handling CLR's stack-based execution model
- Implementing proper type annotations
- Managing .NET assembly metadata

The existing 16 architectures provide excellent reference implementations for pattern matching and code generation strategies.
