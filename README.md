Portable C Compiler
===================

This is a copy of the Portable C Compiler source from http://pcc.ludd.ltu.se/

The source history was imported from a CVS snapshot, `pcc-cvs-20180920.tgz` from http://pcc.ludd.ltu.se/ftp/pub/pcc/,
using the `cvs2git` tools.

Please note: *THIS IS NOT MY CODE!* It's here purely as a way to make this interesting codebase
more accessible. I'm not intending to make a fork of `pcc` or to become its maintainer.

Hence *I am unable to fix any problems* arising from this code, except possibly those relating
to the CVS import.

The source is BSD licensed. Please see `COPYING` for the exact terms.

## Recent Enhancements

This repository includes several enhancements beyond the original PCC codebase:

### Multi-Stage Bootstrap Support

PCC now supports multi-stage bootstrapping for native, cross, and Canadian cross builds:

- **Native Bootstrap**: Build PCC using itself (3-stage verification)
- **Cross-Compilation**: Build for different architectures
- **Canadian Cross**: Build on one platform, run on another, target a third

Quick start:
```bash
# Simple 3-stage bootstrap
make bootstrap

# Or use the script directly
./bootstrap.sh --stages=3 --compare-stages
```

See [BOOTSTRAP.md](BOOTSTRAP.md) for detailed documentation.

### APX (Advanced Performance Extensions) Support

- Extended register support (R16-R31)
- 32 GPRs and 32 XMM registers for x86-64
- Enabled with `-mapx` compiler flag

### Structured Exception Handling (SEH)

- Cross-platform SEH support via `libseh`
- Windows native SEH on Windows
- DWARF-based exception handling on Unix/Linux
- C++ exception interoperability

See [README_SEH.md](README_SEH.md) for details.

### Universal x86 Assembly Emitter

- Support for 9 assembly formats (GNU AS, NASM, YASM, MASM, etc.)
- PE/COFF, ELF, and Mach-O object formats
- Comprehensive DWARF debug info support

See [libx86asm/README.md](libx86asm/README.md) for details.

### Multiple ABI Support

- Itanium C++ ABI (GCC, Clang)
- Microsoft Visual C++ ABI
- Watcom ABI
- Cross-language interoperability (C++ ↔ Pascal)

### Language Frontends

PCC supports multiple programming language frontends, all targeting a unified intermediate representation:

- **C/C++**: Full C11, C17, C23 support with C++ extensions
- **Pascal**: ISO Pascal, Borland Pascal, Delphi Object Pascal, Free Pascal
- **Fortran 77**: Classic FORTRAN 77 with extensions
- **Paradox PAL/ObjectPAL**: Paradox Application Language and ObjectPAL (database scripting)

Each frontend compiler is located in its respective directory (cc/, pascal/, f77/, paradox/)

### Watcom Pragma Support

- Full Watcom C/C++ pragma compatibility
- Calling conventions (cdecl, stdcall, fastcall, watcall)
- Memory models and optimization pragmas

See [WATCOM_PRAGMAS.md](WATCOM_PRAGMAS.md) for details.

## Building

```bash
./configure
make
make install
```

## Documentation

- [BOOTSTRAP.md](BOOTSTRAP.md) - Multi-stage bootstrap guide
- [README_SEH.md](README_SEH.md) - Structured Exception Handling
- [SEH_IMPLEMENTATION.md](SEH_IMPLEMENTATION.md) - SEH implementation details
- [WATCOM_PRAGMAS.md](WATCOM_PRAGMAS.md) - Watcom pragma compatibility
- [paradox/README.md](paradox/README.md) - Paradox PAL/ObjectPAL compiler frontend


