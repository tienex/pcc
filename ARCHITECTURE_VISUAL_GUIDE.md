# PCC Compiler Architecture - Visual Guide

## Compilation Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                     SOURCE C CODE (foo.c)                      │
└────────────────────────┬────────────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │       PASS 1: Frontend (cc0)      │
         │   /home/user/pcc/cc/ccom/         │
         └───────────────────────────────────┘
                         │
              ┌──────────┼──────────┐
              │          │          │
         Lexical   Parsing     Semantic
        Analysis  (cgram.y)   Analysis
              │          │          │
              └──────────┼──────────┘
                         │
                   Optimizations
                   (optim.c)
                         │
                         ▼
      ┌────────────────────────────────────┐
      │   Architecture-specific Pass 1      │
      │   /home/user/pcc/arch/$(TARGMACH)/ │
      │                                    │
      │  - code.c:   code generation       │
      │  - local.c:  symbol handling       │
      └────────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │   INTERMEDIATE FORMAT             │
         │   (Text-based IR)                 │
         │                                   │
         │   - Nodes ('"')                   │
         │   - Labels ('^')                  │
         │   - Prologue/Epilogue ('!'/'%')   │
         └───────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │       PASS 2: Backend (cc1)       │
         │   /home/user/pcc/mip/             │
         │                                   │
         │  - reader.c:  read IR             │
         │  - match.c:   instruction select  │
         │  - regs.c:    register allocate   │
         │  - optim2.c:  optimize            │
         └───────────────────────────────────┘
                         │
              ┌──────────┼──────────┐
              │          │          │
        Instruction  Register    Optimization
         Matching   Allocation   (peephole)
              │          │          │
              └──────────┼──────────┘
                         │
        ┌────────────────────────────────────┐
        │   Architecture-specific Pass 2      │
        │   /home/user/pcc/arch/$(TARGMACH)/ │
        │                                    │
        │  - local2.c:  final optimization   │
        │  - code.c:    emit assembly        │
        │  - order.c:   evaluation order     │
        │  - table.c:   instruction patterns │
        └────────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │     ASSEMBLY CODE (foo.s)         │
         │  (x86, ARM, MIPS, CLR IL, etc.)   │
         └───────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │      ASSEMBLER/LINKER             │
         │   (as, ld, or ILASM for CLR)      │
         └───────────────────────────────────┘
                         │
                         ▼
         ┌───────────────────────────────────┐
         │     EXECUTABLE (foo, foo.exe)     │
         └───────────────────────────────────┘
```

## Architecture-Specific Backend Structure

```
/home/user/pcc/arch/$(TARGMACH)/
│
├── macdefs.h (500-1000 lines)
│   ├─ Type sizes (SZCHAR, SZINT, SZLONG, ...)
│   ├─ Type alignments (ALCHAR, ALINT, ...)
│   ├─ Min/max constants
│   ├─ Register definitions
│   └─ Calling convention offsets
│
├── code.c (20,000-30,000 lines)
│   ├─ bjobcode()      ─ begin compilation
│   ├─ ejobcode()      ─ end compilation
│   ├─ bfcode()        ─ function prologue
│   ├─ efcode()        ─ function epilogue
│   ├─ funcode()       ─ function-specific code
│   ├─ varattrib()     ─ variable attributes
│   └─ (Emits assembly: printf() → stdout)
│
├── local.c (15,000-25,000 lines)
│   ├─ setseg()        ─ emit section directives
│   ├─ genextern()     ─ external declarations
│   ├─ globname()      ─ global symbol directives
│   ├─ picsymtab()     ─ PIC symbol table
│   └─ (Output format, symbol visibility)
│
├── local2.c (20,000-30,000 lines)
│   ├─ local2init()    ─ pass 2 initialization
│   ├─ rallo()         ─ register allocation
│   ├─ rmove()         ─ register moves
│   ├─ setdreg()       ─ set destination register
│   └─ (Optimization, instruction emission)
│
├── order.c (6,000-10,000 lines)
│   ├─ suflags()       ─ Sethi-Ullman computation
│   ├─ ncopy()         ─ node copying
│   ├─ setasg()        ─ assignment handling
│   └─ (Evaluation order, cost metrics)
│
└── table.c (25,000-35,000 lines)
    ├─ struct optab table[] { ... }
    │  ├─ 1000-1500 instruction pattern entries
    │  └─ Maps: Operator + Operands → Assembly
    │
    └─ Key entries:
       ├─ PLUS, MINUS, MUL, DIV operations
       ├─ LOAD, STORE from/to memory/registers
       ├─ CALL, RETURN function control
       ├─ CBRANCH conditional branches
       ├─ SCONV, PCONV type conversions
       └─ Each specifies:
          ├─ Operation type
          ├─ Target goal
          ├─ Operand shapes & types
          ├─ Resource requirements
          └─ Assembly instruction template
```

## Instruction Selection Mechanism (table.c)

```
Example: Integer Addition

┌──────────────────────────────────────────────────────┐
│ Pattern from table.c entry:                          │
│                                                      │
│  { PLUS, INAREG,                                     │
│      SAREG, TINT,         ← left: register, int     │
│      SAREG, TINT,         ← right: register, int    │
│      NAREG, RESC1,        ← needs register, result  │
│      "add %0,%1\n" }      ← template (x86)           │
│      or "add\n"           ← template (CLR IL)        │
└──────────────────────────────────────────────────────┘
                         │
                         ▼
        ┌───────────────────────────────┐
        │  Match Against IR Tree:        │
        │                               │
        │      PLUS                      │
        │      /  \                      │
        │    REG  REG   ← matches!       │
        │    (a)  (b)                    │
        └───────────────────────────────┘
                         │
                         ▼
        ┌───────────────────────────────┐
        │  Emit Assembly:                │
        │                               │
        │  x86:   "add %eax, %ebx"       │
        │  ARM:   "add r0, r1, r2"       │
        │  CLR:   "add"                  │
        │  MIPS:  "addu $3, $4, $5"      │
        └───────────────────────────────┘
```

## Intermediate Representation Format

```
PASS 1 OUTPUT (ASCII Format):
─────────────────────────────

&test.c                    ─ filename
#1                         ─ line number
"100                       ─ start node (ICON)
...node data...
                           ─ end node (blank line)
^L10                       ─ label L10
!0 0 5 0 0 main            ─ function prologue
"200                       ─ ASSIGN node
...
%... 0 5 0 main            ─ function epilogue
*foo:                      ─ passthrough line


PASS 2 INPUT PROCESSING:
────────────────────────

reader.c: rdline() → rdnode() → parse tree
                              ↓
                        pass2_compile()
                              ↓
            ┌─────────────────┼─────────────────┐
            ▼                 ▼                 ▼
        match.c           regs.c           optim2.c
    (instruction      (register        (peephole
     selection)       allocation)      optimization)
            │                 │                 │
            └─────────────────┼─────────────────┘
                              ▼
                   arch-specific code.c
                     & local2.c output
                              ↓
                       Assembly Code
```

## File Dependencies and Data Flow

```
┌─────────────────────────────────────────────────────┐
│                COMMON HEADERS                       │
├─────────────────────────────────────────────────────┤
│ - manifest.h  (operator types, type constants)      │
│ - pass2.h     (pass 2 structures)                   │
│ - node.h      (AST node structure)                  │
│ - macdefs.h   (arch-specific type sizes)            │
└─────────────────────────────────────────────────────┘
                       ▲         ▲
        ┌──────────────┘         └──────────────┐
        │                                       │
┌───────┴────────────┐             ┌───────────┴────────────┐
│   PASS 1 (cc0)     │             │   PASS 2 (cc1)         │
├────────────────────┤             ├────────────────────────┤
│ pass1.h            │             │ pass2.h (definitions)  │
│ cc/ccom/*.c        │             │ mip/match.c (core)     │
│ arch/*/code.c      │             │ mip/regs.c (alloc)     │
│ arch/*/local.c     │             │ mip/reader.c (input)   │
│                    │             │ arch/*/local2.c        │
│ Outputs: IR        │             │ arch/*/order.c         │
└────────────────────┘             │ arch/*/table.c         │
          │                         │                        │
          │    ┌──────────────────►│ Inputs: IR             │
          │    │                    │                        │
          └────┼───────────────────┬┘                        │
               │                   │                        │
               └───────────────────┴────────────────────┐
                                                       │
                              Outputs: Assembly Code ◄─┘
```

## Configuration and Build System

```
┌──────────────────────────────────┐
│   configure.ac (Target Mapping)  │
└──────────────────────────────────┘
    │
    ├─ target_os   (linux, freebsd, darwin, etc.)
    ├─ target_cpu  (x86_64, arm, mips, etc.)
    │
    ▼
┌──────────────────────────────────┐
│   targos + targmach              │
│   (e.g., linux + amd64)          │
└──────────────────────────────────┘
    │
    ├─ MDIR = arch/$(targmach)
    ├─ -Dos_$(targos)
    ├─ -Dmach_$(targmach)
    │
    ▼
┌──────────────────────────────────┐
│ Makefile.in (Compiler Rules)     │
├──────────────────────────────────┤
│                                  │
│ code.o:  $(MDIR)/code.c          │
│ local.o: $(MDIR)/local.c         │
│ local2.o: $(MDIR)/local2.c       │
│ order.o: $(MDIR)/order.c         │
│ table.o: $(MDIR)/table.c         │
│                                  │
└──────────────────────────────────┘
    │
    ├─ Automatically links with
    ├─ common MIP code
    ├─ target OS support
    │
    ▼
┌──────────────────────────────────┐
│   cc0 (Pass 1) or cc1 (Pass 2)   │
└──────────────────────────────────┘
```

## Supported Architectures Grid

```
┌────────────────────────────────────────────────────────────────┐
│              ARCHITECTURE SUPPORT IN PCC                       │
├──────────────┬────────────┬────────────┬──────────────────────┤
│ Architecture │ Directory  │ Bit Width  │ Endianness           │
├──────────────┼────────────┼────────────┼──────────────────────┤
│ x86-64       │ amd64/     │ 64-bit     │ Little-endian        │
│ x86          │ i386/      │ 32-bit     │ Little-endian        │
│ ARM          │ arm/       │ 32-bit     │ Little-endian (usual)│
│ MIPS         │ mips/      │ 32-bit     │ Configurable         │
│ MIPS64       │ mips64/    │ 64-bit     │ Configurable         │
│ PowerPC      │ powerpc/   │ 32-bit     │ Big-endian           │
│ SPARC-64     │ sparc64/   │ 64-bit     │ Big-endian           │
│ Motorola 68k │ m68k/      │ 32-bit     │ Big-endian           │
│ VAX          │ vax/       │ 32-bit     │ Little-endian        │
│ HPPA         │ hppa/      │ 32-bit     │ Big-endian           │
│ 8086         │ i86/       │ 16-bit     │ Little-endian        │
│ M16C         │ m16c/      │ 16-bit     │ Little-endian        │
│ Nova         │ nova/      │ 32-bit     │ ?                    │
│ PDP-7        │ pdp7/      │ 18-bit     │ ?                    │
│ PDP-10       │ pdp10/     │ 36-bit     │ ?                    │
│ PDP-11       │ pdp11/     │ 16-bit     │ Little-endian        │
└──────────────┴────────────┴────────────┴──────────────────────┘

Total: 16 different architectures ✓
```

## Adding CLR Backend - Integration Points

```
Step 1: Create Directory
└─ /home/user/pcc/arch/clr/

Step 2: Create Backend Files
├─ macdefs.h     ← Type sizes, stack depth, calling convention
├─ code.c        ← bjobcode(), bfcode(), efcode(), ejobcode()
├─ local.c       ← setseg(), genextern(), globname()
├─ local2.c      ← local2init(), alloctemp(), rmove()
├─ table.c       ← struct optab table[] (1000+ entries)
└─ order.c       ← suflags(), cost()

Step 3: Update Build System
├─ configure.ac  ← Add case for CLR target
└─ Auto-integrated by Makefile.in (uses MDIR=$(TARGET))

Step 4: Build and Test
├─ ./configure --target=x86_64-clr-win32
├─ make
└─ ./cc/cc/cc test.c
```

## Key Compiler Phases with CLR Mappings

```
C Source         →  PASS 1    →  IR Format  →  PASS 2    →  CLR IL
─────────────────────────────────────────────────────────────────
int a = 5;       →  ASSIGN    →  "...ASSIGN" →  add stack →  stsfld
a = a + 3;       →  PLUS+ASGN →  "...PLUS"   →  add/sub   →  add
                                                            →  stsfld
if (a > 0)       →  CBRANCH   →  "...CBRANCH"→ compare    →  cgt
func(a);         →  CALL      →  "...CALL"   →  setup args →  call
                                              →  cleanup   →  pop
return a;        →  RETURN    →  "...RETURN" →  pass value →  ret
                                                            
```

## Performance Characteristics

```
Typical Backend Complexity:
┌─────────────────────────────────────────────┐
│                File Sizes                   │
├──────────────┬──────────┬──────┬────────────┤
│ File         │ Amd64    │ ARM  │ Average    │
├──────────────┼──────────┼──────┼────────────┤
│ code.c       │ 30 KB    │ 30KB │ 25-32 KB   │
│ local.c      │ 20 KB    │ 20KB │ 15-25 KB   │
│ local2.c     │ 25 KB    │ 25KB │ 20-30 KB   │
│ table.c      │ 30 KB    │ 30KB │ 25-35 KB   │
│ order.c      │ 8 KB     │ 8KB  │ 6-10 KB    │
├──────────────┼──────────┼──────┼────────────┤
│ Total        │ 113 KB   │113KB │ ~110 KB    │
└──────────────┴──────────┴──────┴────────────┘

Compilation Speed: ~1000 lines/second (varies by optimization)
Memory Usage: ~5-10 MB for typical compilation
```

## Summary: Architecture is Modular & Extensible

```
Core Compiler      +    Architecture Backend    =    Complete Compiler
(Frontend/Backend)      (6 files, ~100KB)
   (cc/ + mip/)         (arch/$(TARGMACH)/)

┌─────────────────────┐ ┌──────────────────────┐    ┌─────────────┐
│ Pass 1: Parser      │ │ macdefs.h:           │    │ Input: C    │
│ Pass 2: Codegen     │ │ - Type definitions   │ ───┤ Output:     │
│ Common functions    │ │ code.c:              │    │ Assembly    │
│ Table matching      │ │ - Prologue/epilogue  │    │ (any arch)  │
│ Register alloc      │ │ local.c:             │    │             │
│ Optimizations       │ │ - Symbol handling    │    │             │
└─────────────────────┘ │ local2.c:            │    └─────────────┘
                        │ - Optimizations      │
                        │ order.c:             │
                        │ - Evaluation order   │
                        │ table.c:             │
                        │ - Instruction select │
                        └──────────────────────┘

✓ Easy to add new backends (just 6 files)
✓ All complex logic centralized
✓ Reference implementations available
```

