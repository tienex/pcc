# PCC Codebase Documentation Index

This directory now contains comprehensive documentation of the PCC (Portable C Compiler) codebase structure and architecture.

## Documentation Files

### 1. **CODEBASE_ARCHITECTURE.md** (537 lines)
**Comprehensive technical overview of the entire PCC compiler**

- Executive summary
- Overall directory structure
- 16 supported target architectures
- Architecture-specific backend file purposes
- Code generation pipeline and IR (Intermediate Representation)
- Target platform configuration system
- Main entry points and compilation flow
- Backend file sizes and complexity metrics
- Step-by-step guide to adding a CLR backend
- Key files for understanding the system
- Testing strategies

**Read this for:** Deep understanding of how PCC works end-to-end

### 2. **ARCHITECTURE_VISUAL_GUIDE.md** (200+ lines)
**Visual diagrams and ASCII art explaining compiler structure**

- Compilation flow diagram (Pass 1 → IR → Pass 2)
- Architecture-specific backend structure tree
- Instruction selection mechanism explained
- Intermediate representation format details
- File dependencies and data flow
- Configuration and build system flow
- Supported architectures grid
- CLR backend integration points
- Key compiler phases
- Performance characteristics
- Modular architecture summary

**Read this for:** Quick visual understanding of architecture and data flow

### 3. **CLR_BACKEND_GUIDE.md** (300+ lines)
**Detailed implementation guide for adding a CLR (Common Language Runtime) backend**

- Quick reference for each of 6 backend files
  - macdefs.h - type definitions (600-800 lines)
  - code.c - code generation (2000-3000 lines)
  - local.c - symbol handling (1500-2000 lines)
  - local2.c - register allocation (1500-2000 lines)
  - table.c - instruction selection (2000-3000+ lines)
  - order.c - evaluation order (600-1000 lines)
- Complete code templates with examples
- Minimal viable implementation checklist
- Testing procedures
- CLR IL instruction reference
- Common pitfalls and solutions

**Read this for:** Practical implementation details for new backends

## Key Findings Summary

### Compiler Structure
- **Two-pass design**: Pass 1 (frontend/parsing) → IR → Pass 2 (backend/codegen)
- **Modular backends**: Each architecture is self-contained in `/home/user/pcc/arch/<name>/`
- **Table-driven code generation**: Pattern matching via `struct optab table[]`
- **16 supported architectures**: From x86-64 to vintage PDP machines

### Backend Requirements
Each architecture directory must contain exactly 6 files:

1. **macdefs.h** - Type sizes, alignments, and machine constants
2. **code.c** - Function prologue/epilogue and job-level code
3. **local.c** - Symbol declarations and section directives
4. **local2.c** - Register allocation and optimization
5. **table.c** - Instruction selection rules (1000-1500 entries)
6. **order.c** - Expression evaluation order and cost metrics

### Typical Backend Size
- **100-120 KB total** per architecture
- code.c: 25-32 KB
- table.c: 25-35 KB
- local.c: 15-25 KB
- local2.c: 20-30 KB
- order.c: 6-10 KB
- macdefs.h: <10 KB

### Build Integration
- **Automatic**: Just create arch/<name>/ directory with 6 files
- **Configuration**: Update configure.ac to map target → architecture
- **Makefile**: Automatically uses MDIR=$(TARGET) to find files
- **Preprocessor flags**: -Dos_$(TARGOS) -Dmach_$(TARGMACH) for conditional compilation

## Quick Navigation

### For understanding PCC overall:
1. Start with ARCHITECTURE_VISUAL_GUIDE.md (visual overview)
2. Then read CODEBASE_ARCHITECTURE.md (detailed explanation)
3. Reference specific architecture in `/home/user/pcc/arch/`

### For implementing CLR backend:
1. Read CLR_BACKEND_GUIDE.md (templates and examples)
2. Study an existing backend (ARM or amd64)
3. Create `/home/user/pcc/arch/clr/` with 6 files
4. Update configure.ac
5. Test with `./configure --target=clr-win32 && make`

### For specific information:
- **Intermediate Representation**: See "Code Generation Pipeline" section in CODEBASE_ARCHITECTURE.md
- **Instruction Selection**: See "Instruction Selection Mechanism" in ARCHITECTURE_VISUAL_GUIDE.md
- **Type System**: See macdefs.h reference in existing architectures
- **Calling Conventions**: See code.c bfcode() and efcode() in existing backends

## Files by Complexity Level

### Beginner-Friendly
- ARCHITECTURE_VISUAL_GUIDE.md (easy visual overview)
- CLR_BACKEND_GUIDE.md section 1-3 (simple templates)

### Intermediate
- CODEBASE_ARCHITECTURE.md sections 1-5 (architecture overview)
- CLR_BACKEND_GUIDE.md sections 4-6 (practical implementation)
- Study arch/arm/ or arch/i386/ backends (medium complexity)

### Advanced
- CODEBASE_ARCHITECTURE.md sections 6-10 (compilation flow details)
- Study arch/amd64/ backend (complex optimization)
- Read mip/match.c and mip/regs.c (core algorithms)

## Reference Quick Links

### Key Directories
```
/home/user/pcc/arch/          - 16 architecture backends
/home/user/pcc/mip/           - Middle-level IR processor (Pass 2)
/home/user/pcc/cc/ccom/       - C compiler frontend (Pass 1)
/home/user/pcc/os/            - OS-specific configurations
/home/user/pcc/common/        - Shared utilities
```

### Most Important Files
```
/home/user/pcc/mip/node.h              - AST node structure definition
/home/user/pcc/mip/pass2.h             - Pass 2 data structures
/home/user/pcc/configure.ac            - Target mapping configuration
/home/user/pcc/cc/ccom/Makefile.in     - Build system integration
/home/user/pcc/arch/amd64/table.c      - Example instruction table
/home/user/pcc/arch/arm/code.c         - Example code generation
```

## Implementation Roadmap for CLR Backend

1. **Create directory**: `mkdir /home/user/pcc/arch/clr`
2. **Create 6 files** (see CLR_BACKEND_GUIDE.md for templates)
3. **Update configure.ac** with CLR target mapping
4. **Build and test**: Run `./configure --target=clr-win32 && make`
5. **Implement operators incrementally**:
   - Start: integer operations (PLUS, MINUS, MUL)
   - Next: memory access (NAME, OREG)
   - Then: control flow (CBRANCH, RETURN)
   - Finally: complex operations (CALL, type conversions)

## Document Statistics

- **CODEBASE_ARCHITECTURE.md**: 537 lines, ~25 KB
- **ARCHITECTURE_VISUAL_GUIDE.md**: 350+ lines, ~15 KB
- **CLR_BACKEND_GUIDE.md**: 300+ lines, ~12 KB
- **DOCUMENTATION_INDEX.md**: This file

**Total documentation**: ~70 KB (1000+ lines)

## Version Information

- **PCC Version**: 1.2.0.DEVEL
- **Documentation Date**: October 26, 2025
- **Repository**: /home/user/pcc/
- **Current Branch**: claude/add-microsoft-clr-backend-011CUUsMTzQdTAb3YCcP6JYo

## Contributing Guidelines

When adding a new backend:
1. Follow the 6-file structure in existing backends
2. Keep total size around 100-120 KB
3. Implement at least 30-50 basic instruction patterns in table.c
4. Test with simple C programs (int main() { return 42; })
5. Reference similar architecture (arm for 32-bit, amd64 for 64-bit)

## Support and References

### External References
- ECMA-335: CLR specification
- ECMA-335 Partition III: IL (Intermediate Language) specification
- PCC Official Site: http://pcc.ludd.ltu.se/

### Similar Backends in PCC
- **ARM** (`arch/arm/`) - Similar scale to CLR, 32-bit
- **i386** (`arch/i386/`) - Moderate complexity, x86 32-bit
- **amd64** (`arch/amd64/`) - Most complex, x86-64

---

**Last Updated**: October 26, 2025
**Documentation Coverage**: Very Thorough (as requested)
