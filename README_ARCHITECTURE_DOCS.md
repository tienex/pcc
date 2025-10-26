# PCC Compiler Architecture Documentation Index

This directory contains comprehensive documentation of the Portable C Compiler architecture, specifically focused on designing and implementing a Ruby front-end.

## Generated Documentation Files

### 1. START HERE: ARCHITECTURE_EXPLORATION_SUMMARY.md
**Length**: 348 lines | **Best for**: Quick overview and next steps

The executive summary of the entire exploration. Contains:
- Key findings from the architecture analysis
- Quick reference to all important components
- Design implications for Ruby frontend
- Quick start guide for development
- List of files to study first

**Start here if you**: Want a 10-minute overview of the entire architecture

---

### 2. COMPILER_OVERVIEW.md
**Length**: 480 lines | **Best for**: Comprehensive reference

A complete one-document overview with:
- Executive summary and directory structure
- Compiler pipeline explanation
- IR structures and definitions
- All 18 backends described
- Build system details
- How to add Ruby frontend
- Supported operators and type system

**Start here if you**: Want everything in one markdown document

---

### 3. arch_summary.txt
**Length**: 534 lines | **Best for**: Detailed technical reference

The most comprehensive technical breakdown covering:
- Overall directory structure with descriptions
- Existing front-ends (C and C++)
- IR format with code examples
- All 18 architecture backends in detail
- Complete compiler pipeline and stage connections
- Build system details and bootstrap process
- Key components and files
- Design patterns and abstractions
- Debug symbol support
- Ruby frontend integration patterns

**Start here if you**: Need detailed technical understanding of each component

---

### 4. architecture_diagram.txt
**Length**: 308 lines | **Best for**: Visual learners

ASCII diagrams and visual representations:
- Compiler architecture flowchart
- Backend architecture diagram
- Data flow through IR layers
- Compilation modes (unified vs split)
- File type flow in build system
- Architecture-specific customization points

**Start here if you**: Learn better with diagrams and visualizations

---

### 5. ruby_frontend_guide.txt
**Length**: 640 lines | **Best for**: Practical implementation guide

Step-by-step guide to creating a Ruby frontend:
- 13 detailed steps from setup to testing
- Directory structure and file organization
- How to understand existing C frontend
- Key integration points explained
- Minimal Ruby grammar skeleton
- Ruby lexer template (scan.l)
- Main entry point template (main.c)
- Handling Ruby-specific constructs
- Testing strategy
- Symbol table extensions
- Backend compatibility checklist
- Build configuration templates
- 7-phase development roadmap
- Debugging infrastructure guide

**Start here if you**: Want to start implementing the Ruby frontend

---

## Reading Order Recommendations

### For Quick Understanding (1-2 hours)
1. ARCHITECTURE_EXPLORATION_SUMMARY.md
2. architecture_diagram.txt
3. COMPILER_OVERVIEW.md (skim)

### For Deep Understanding (4-6 hours)
1. ARCHITECTURE_EXPLORATION_SUMMARY.md
2. architecture_diagram.txt
3. COMPILER_OVERVIEW.md (full read)
4. arch_summary.txt (reference)
5. ruby_frontend_guide.txt (for implementation planning)

### For Implementation (ongoing reference)
1. ruby_frontend_guide.txt (main guide)
2. arch_summary.txt (for details)
3. COMPILER_OVERVIEW.md (for lookups)
4. Actual source code (cc/ccom/, mip/, arch/)

---

## Key Documentation Sections

### Understanding the Architecture
- See: COMPILER_OVERVIEW.md - "Compiler Pipeline" section
- See: architecture_diagram.txt - all diagrams
- See: arch_summary.txt - sections 1-5

### Learning the IR
- See: COMPILER_OVERVIEW.md - "Intermediate Representation (IR)" section
- See: arch_summary.txt - section 3
- See: ARCHITECTURE_EXPLORATION_SUMMARY.md - "Key Findings" #4

### Understanding Backends
- See: COMPILER_OVERVIEW.md - "Architecture Backends Explained" section
- See: arch_summary.txt - section 4
- See: architecture_diagram.txt - "Backend Architecture" diagram

### Starting Implementation
- See: ruby_frontend_guide.txt - entire document
- See: ARCHITECTURE_EXPLORATION_SUMMARY.md - "Ruby Front-End Design Implications"
- See: COMPILER_OVERVIEW.md - "How to Add Ruby Frontend" section

---

## Key Files in Source Code

### Must Read
1. `/home/user/pcc/cc/ccom/main.c` (200 lines)
   - Compiler entry point and initialization
   
2. `/home/user/pcc/cc/ccom/scan.l` (650 lines)
   - Lexer template and token definitions
   
3. `/home/user/pcc/cc/ccom/cgram.y` (1500 lines)
   - Grammar rules and semantic actions
   
4. `/home/user/pcc/cc/ccom/pass1.h` (630 lines)
   - P1ND IR structure definitions
   
5. `/home/user/pcc/mip/common.c` (500 lines)
   - myp2tree() function (CRITICAL integration point)

### Should Read
6. `/home/user/pcc/configure.ac` (500 lines)
   - Build configuration and target selection
   
7. `/home/user/pcc/arch/amd64/macdefs.h` (150 lines)
   - Example backend machine definitions
   
8. `/home/user/pcc/cc/ccom/trees.c` (1800 lines)
   - Semantic analysis and tree operations

### Reference
9. `/home/user/pcc/arch/amd64/table.c` (2000 lines)
   - Instruction selection patterns example
   
10. `/home/user/pcc/cc/ccom/symtabs.c` (300 lines)
    - Symbol table implementation

---

## Quick Facts

### Compiler Structure
- **Type**: Two-pass compiler
- **Pass 1 Output**: P1ND IR (high-level, ~80 operators)
- **Pass 2 Output**: Assembly code (architecture-specific)

### Supported Architectures
- **Total**: 18 backends
- **Modern**: amd64, ARM, WASM
- **Classic**: MIPS, PowerPC, SPARC, VAX, PDP-11, etc.

### Key Integration Point
- **Function**: `myp2tree()` in `/home/user/pcc/mip/common.c`
- **Purpose**: Converts P1ND IR → Serialized IR for pass 2
- **Critical for**: All Ruby constructs must be expressible in P1ND

### Development Effort
- **Phase 0 (Understanding)**: 1 day
- **Phase 1 (Setup)**: 1 day
- **Phase 2 (Minimal Lexer/Parser)**: 2-3 days
- **Phase 3 (Integration)**: 1-2 days
- **Phase 4 (Expansion)**: Ongoing

### Expected Result
- Ruby code → P1ND IR → All 18 backends → Working executables
- No backend modifications needed
- Full compiler pipeline automatically works

---

## Starting the Ruby Frontend Project

### Step 1: Understanding (Today)
```bash
cd /home/user/pcc
# Read these in order:
cat ARCHITECTURE_EXPLORATION_SUMMARY.md
cat architecture_diagram.txt
cat COMPILER_OVERVIEW.md
```

### Step 2: Setup (Tomorrow)
```bash
# Create directory structure
mkdir -p /home/user/pcc/cc/rcom

# Copy C frontend as template
cp /home/user/pcc/cc/ccom/main.c /home/user/pcc/cc/rcom/
cp /home/user/pcc/cc/ccom/Makefile.in /home/user/pcc/cc/rcom/
# ... copy other templates and adapt
```

### Step 3: Study Existing Code (This week)
```bash
# Read key source files in this order:
cat /home/user/pcc/cc/ccom/main.c              # Entry point
cat /home/user/pcc/cc/ccom/scan.l              # Lexer
cat /home/user/pcc/cc/ccom/cgram.y             # Grammar (portions)
cat /home/user/pcc/cc/ccom/pass1.h             # IR definitions
cat /home/user/pcc/mip/common.c                # myp2tree() function
```

### Step 4: Implementation (Next week and beyond)
```bash
# Follow ruby_frontend_guide.txt step by step
# Create minimal Ruby lexer and parser
# Test integration with existing backends
# Iteratively expand Ruby feature support
```

---

## Documentation Statistics

- **Total Lines**: 2,310
- **Total Coverage**: Complete compiler architecture
- **Diagrams**: 8 ASCII diagrams
- **Code Examples**: 15+ code snippets
- **Implementation Templates**: 5+ ready-to-use templates
- **Development Phases**: 7 planned phases
- **Backends Documented**: 18 architectures
- **Integration Points Identified**: 6+ critical points

---

## Key Insights

1. **Clean Architecture**: Two-pass design makes it easy to add frontends
2. **Portable IR**: P1ND IR works across all 18 backends unchanged
3. **Well-Proven**: Original PCC is 40+ years old, battle-tested design
4. **Easy Integration**: Ruby frontend only needs to produce valid P1ND trees
5. **Free Benefits**: Register allocation, instruction selection, assembly all automatic
6. **Extensible**: Clear patterns for adding new languages and targets

---

## Questions and Answers

**Q: Do I need to modify the backends for Ruby?**
A: No! All backends work automatically once you produce valid P1ND IR.

**Q: What's the hardest part of implementing a Ruby frontend?**
A: Mapping Ruby's dynamic semantics to static P1ND IR trees.

**Q: Can I test on multiple architectures?**
A: Yes! Test on amd64, ARM, and WASM with the same Ruby code.

**Q: How long will it take?**
A: Minimal compiler: 1 week. Full-featured: 3-6 weeks (depends on scope).

**Q: Where do Ruby blocks and classes go?**
A: Convert to C-like constructs in P1ND IR (see ruby_frontend_guide.txt).

**Q: What's myp2tree() and why is it important?**
A: It's the critical function that converts high-level P1ND to low-level IR. All Ruby constructs must map to valid P1ND operators.

---

## Contact & Support

- **PCC Homepage**: http://pcc.ludd.ltu.se/
- **Documentation in Repo**: See other .md files in `/home/user/pcc/`
- **Source Code Quality**: Very readable, good for learning

---

**Documentation Generated**: October 26, 2025
**Total Exploration Time**: Comprehensive (medium thoroughness)
**Ready for Implementation**: Yes
**Estimated Ruby Frontend Effort**: 3-6 weeks for full-featured compiler

---

Start with: **ARCHITECTURE_EXPLORATION_SUMMARY.md**
