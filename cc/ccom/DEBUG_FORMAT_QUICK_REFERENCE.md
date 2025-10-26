# Debug Format Quick Reference Card

## Format Enum Quick Lookup

```c
// Standard Formats
DBGFMT_DWARF1, DBGFMT_DWARF2, DBGFMT_DWARF3, DBGFMT_DWARF4, DBGFMT_DWARF5
DBGFMT_CV4, DBGFMT_CV5, DBGFMT_CV6, DBGFMT_CV7, DBGFMT_CV8
DBGFMT_COFF, DBGFMT_ECOFF, DBGFMT_XCOFF, DBGFMT_PECOFF
DBGFMT_STABS, DBGFMT_DBX

// Vendor Formats
DBGFMT_BORLAND_TD32, DBGFMT_BORLAND_TDS
DBGFMT_WATCOM
DBGFMT_IBM_HLL
DBGFMT_HP_SOM
DBGFMT_VMS_DST

// Classic Platforms
DBGFMT_MACOS_MPW, DBGFMT_MACOS_PEF, DBGFMT_MACOS_CODEWARRIOR
DBGFMT_ATARI_DRI, DBGFMT_ATARI_GST, DBGFMT_ATARI_PUREC
DBGFMT_AMIGA_HUNK, DBGFMT_AMIGA_SASC, DBGFMT_AMIGA_DICE
DBGFMT_ACORN_AOF, DBGFMT_ACORN_AIF, DBGFMT_ACORN_DDT

// Modern & Specialized
DBGFMT_AOUT
DBGFMT_MACHO
DBGFMT_OMF
DBGFMT_PDB
DBGFMT_CTF
DBGFMT_BTF
DBGFMT_PLAN9
DBGFMT_TADS
```

## Platform Quick Reference

| Platform | Use This Format |
|----------|----------------|
| Linux (modern) | `DBGFMT_DWARF5` |
| Linux (kernel eBPF) | `DBGFMT_BTF` |
| Windows (Visual Studio) | `DBGFMT_PDB` |
| Windows (legacy) | `DBGFMT_CV8` |
| macOS / iOS | `DBGFMT_MACHO` |
| FreeBSD | `DBGFMT_DWARF5` |
| Solaris / illumos | `DBGFMT_CTF` |
| AIX | `DBGFMT_IBM_HLL` |
| HP-UX | `DBGFMT_HP_SOM` |
| OpenVMS | `DBGFMT_VMS_DST` |
| Plan 9 | `DBGFMT_PLAN9` |
| Mac OS Classic (68k) | `DBGFMT_MACOS_MPW` |
| Mac OS Classic (PPC) | `DBGFMT_MACOS_PEF` |
| Atari ST | `DBGFMT_ATARI_DRI` |
| Amiga | `DBGFMT_AMIGA_HUNK` |
| RISC OS | `DBGFMT_ACORN_AOF` |
| MS-DOS | `DBGFMT_OMF` |
| OS/2 | `DBGFMT_IBM_HLL` |

## Implementation Files

| Format | File | Lines |
|--------|------|-------|
| DWARF | `debugsym_dwarf.c` | 500+ |
| CodeView | `debugsym_codeview.c` | 500+ |
| COFF | `debugsym_coff.c` | 200+ |
| STABS | `debugsym_stabs.c` | 200+ |
| DBX | `debugsym_dbx.c` | 150+ |
| Borland | `debugsym_borland.c` | 150+ |
| Watcom | `debugsym_watcom.c` | 150+ |
| IBM HLL | `debugsym_hll.c` | 400+ |
| HP SOM | `debugsym_hpsom.c` | 400+ |
| VMS DST | `debugsym_vms.c` | 550+ |
| Mac OS | `debugsym_macos.c` | 600+ |
| Atari | `debugsym_atari.c` | 650+ |
| Amiga | `debugsym_amiga.c` | 650+ |
| Acorn | `debugsym_acorn.c` | 650+ |
| a.out | `debugsym_aout.c` | 400+ |
| Mach-O | `debugsym_macho.c` | 500+ |
| OMF | `debugsym_omf.c` | 450+ |
| PDB | `debugsym_pdb.c` | 500+ |
| CTF | `debugsym_ctf.c` | 550+ |
| BTF | `debugsym_btf.c` | 550+ |
| Plan 9 | `debugsym_plan9.c` | 400+ |
| TADS | `debugsym_tads.c` | 500+ |

## Common API Usage

### Basic Initialization
```c
#include "debugsym.h"

// Initialize for platform
debugsym_init(DBGFMT_DWARF5);

// Begin source file
debugsym_file_begin("myfile.c");

// Record symbols
debugsym_record_variable(var_sym);
debugsym_record_function(func_sym);

// Track scope
debugsym_enter_function(func_sym);
debugsym_line(42);
debugsym_exit_function();

// Finish
debugsym_finish();
```

### Using Integration Layer
```c
#include "debugsym.h"

// Auto-detect platform and initialize
debugsym_integration_init("myfile.c", gflag);

// These are called automatically by compiler
debugsym_integration_variable(sp);
debugsym_integration_function_begin(sp);
debugsym_integration_line(lineno);
debugsym_integration_function_end();

// Cleanup
debugsym_integration_finish();
```

## Format Characteristics

| Format | Size | Complexity | Speed | Portability |
|--------|------|-----------|--------|-------------|
| DWARF 5 | Large | High | Medium | Excellent |
| PDB | Large | High | Fast | Windows-only |
| CodeView | Medium | Medium | Fast | Windows-only |
| CTF | Small | Low | Very Fast | Unix |
| BTF | Small | Low | Very Fast | Linux |
| STABS | Medium | Low | Fast | Unix/BSD |
| Mach-O | Large | High | Medium | Apple-only |
| Plan 9 | Small | Very Low | Very Fast | Plan 9 |

## Debugger Compatibility

| Debugger | Supported Formats |
|----------|------------------|
| GDB | DWARF, STABS, COFF |
| LLDB | DWARF, Mach-O |
| Visual Studio | PDB, CodeView |
| WinDbg | PDB, CodeView |
| DBX | DBX, STABS, DWARF |
| DTrace | CTF |
| bpftool | BTF |
| Acid | Plan 9 |
| TD/TD32 | Borland, TADS |

## Section Names by Format

| Format | Section Names |
|--------|--------------|
| DWARF | `.debug_info`, `.debug_abbrev`, `.debug_str`, `.debug_line` |
| CodeView | `.debug$S`, `.debug$T` |
| CTF | `.SUNW_ctf` |
| BTF | `.BTF` |
| PDB | External `.pdb` file |
| TADS | `.debug$T` |
| Mach-O | `__DWARF.__debug_info`, etc. |

## Magic Numbers / Signatures

| Format | Magic/Signature |
|--------|----------------|
| DWARF 2-5 | No magic (in ELF sections) |
| PDB 2.0 | `"Microsoft C/C++ program database 2.00\r\n\032JG\0"` |
| PDB 7.0 | `"Microsoft C/C++ MSF 7.00\r\n\032DS\0\0"` |
| CTF | `0xcff1` (magic) |
| BTF | `0xeB9f` (magic) |
| Mach-O | `0xfeedface` (32-bit), `0xfeedfacf` (64-bit) |
| a.out | `0x107` (OMAGIC), `0x108` (NMAGIC), `0x10b` (ZMAGIC) |
| Plan 9 386 | `0x8000` (8.out) |
| Plan 9 amd64 | `0x6000` (6.out) |

## Architecture Support

| Architecture | Supported Formats |
|-------------|------------------|
| x86 | DWARF, CodeView, PDB, STABS, COFF, OMF, Plan 9 (8.out) |
| x86_64 | DWARF, CodeView, PDB, Mach-O, Plan 9 (6.out) |
| ARM | DWARF, Mach-O, Acorn, Plan 9 (5.out) |
| ARM64 | DWARF, Mach-O, BTF |
| 68000 | Mac OS, Atari, Amiga, Plan 9 (2.out) |
| PowerPC | Mac OS (PEF), Plan 9 (q.out) |
| MIPS | ECOFF, Plan 9 (v.out) |
| SPARC | COFF, Plan 9 (k.out) |
| PA-RISC | HP SOM |
| Alpha | ECOFF, VMS DST |
| VAX | VMS DST |
| Itanium | HP SOM, VMS DST |

## Timeline

| Year | Format | Platform |
|------|--------|----------|
| 1977 | a.out, VMS DST | Unix v7, VMS |
| 1984 | STABS | 4.2BSD |
| 1985 | Atari, Amiga | Atari ST, Amiga |
| 1986 | Mac OS MPW | Macintosh |
| 1987 | Acorn, IBM HLL, HP SOM | Archimedes, OS/2, HP-UX |
| 1989 | Mach-O, TADS | NeXTSTEP, TASM |
| 1992 | DWARF 1, Plan 9 | Unix, Plan 9 |
| 1993 | DWARF 2, PDB | Unix, Visual C++ |
| 2001 | CTF | Solaris 8 |
| 2005 | DWARF 3 | Modern Unix/Linux |
| 2010 | DWARF 4 | Modern systems |
| 2017 | DWARF 5 | Modern systems |
| 2018 | BTF | Linux 4.18+ |

## Environment Variables

```bash
# Select format explicitly
export PCC_DEBUG_FORMAT=dwarf5  # or pdb, ctf, btf, etc.

# Enable statistics
export PCC_DEBUG_STATS=1

# Enable verbose output
export PCC_DEBUG_VERBOSE=1
```

## Compile Flags

```bash
# Generate debug symbols
pcc -g myfile.c

# Specify debug format (if supported)
pcc -g -fdebug-format=dwarf5 myfile.c

# Optimize but keep debug info
pcc -g -O2 myfile.c
```

## Testing

```bash
# Compile test suite
cd cc/ccom
make test_debugsym

# Run tests
./test_debugsym

# Test with real code
pcc -g examples/debug_test.c -o debug_test

# Verify with debugger
gdb debug_test        # For DWARF/STABS
lldb debug_test       # For DWARF/Mach-O
```

## File Sizes (Approximate)

| Format | Debug Info Size | Relative to Code |
|--------|----------------|------------------|
| DWARF 5 | Large | 5-10x |
| DWARF 4 | Large | 4-8x |
| PDB | Large | 5-10x |
| CodeView | Medium | 3-5x |
| CTF | Small | 1-2x |
| BTF | Small | 1-2x |
| STABS | Medium | 2-4x |
| Plan 9 | Very Small | 0.5-1x |

## Memory Usage (During Compilation)

| Format | Memory Overhead | Cache Efficiency |
|--------|----------------|-----------------|
| DWARF 5 | Medium | Good (with caching) |
| PDB | Medium | Good |
| CTF | Low | Excellent |
| BTF | Low | Excellent |
| STABS | Low | Good |
| Plan 9 | Very Low | Excellent |

## Compilation Speed Impact

| Format | Overhead | Typical Slowdown |
|--------|----------|-----------------|
| DWARF 5 | Medium | 5-10% |
| PDB | Medium | 5-10% |
| CTF | Low | 2-5% |
| BTF | Low | 2-5% |
| STABS | Low | 2-5% |
| Plan 9 | Very Low | < 2% |

## Common Use Cases

### Modern Linux Development
```c
debugsym_init(DBGFMT_DWARF5);
```

### Windows Development
```c
debugsym_init(DBGFMT_PDB);
```

### macOS/iOS Development
```c
debugsym_init(DBGFMT_MACHO);
```

### Linux Kernel/eBPF
```c
debugsym_init(DBGFMT_BTF);
```

### DTrace Probes
```c
debugsym_init(DBGFMT_CTF);
```

### Embedded/Resource-Constrained
```c
debugsym_init(DBGFMT_STABS);  // or Plan 9 for minimal overhead
```

### Retro 68k Development
```c
debugsym_init(DBGFMT_ATARI_DRI);    // Atari ST
debugsym_init(DBGFMT_AMIGA_HUNK);   // Amiga
debugsym_init(DBGFMT_MACOS_MPW);    // Mac 68k
```

### Retro ARM Development
```c
debugsym_init(DBGFMT_ACORN_AOF);    // RISC OS
```

## Support Matrix

| Feature | DWARF | PDB | CTF | BTF | STABS | Plan 9 |
|---------|-------|-----|-----|-----|-------|--------|
| Line numbers | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Local variables | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| Type info | ✓✓✓ | ✓✓✓ | ✓✓ | ✓✓ | ✓ | ✓ |
| Inline functions | ✓ | ✓ | - | ✓ | - | - |
| Templates/Generics | ✓ | ✓ | - | - | - | - |
| Macros | ✓ | ✓ | - | - | - | - |
| Optimized code | ✓✓ | ✓✓ | ✓ | ✓ | ✓ | ✓ |
| Multi-threading | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |

Legend: ✓✓✓ = Excellent, ✓✓ = Good, ✓ = Basic, - = Not supported

## Contact & Support

For issues, questions, or contributions:
- **Documentation**: See `DEBUGSYM_README.md` for detailed API reference
- **Catalog**: See `DEBUG_FORMAT_CATALOG.md` for comprehensive format details
- **Examples**: See `examples/debug_test.c` for usage examples
- **Tests**: Run `test_debugsym` for validation

---

**Quick Reference Version**: 1.0
**Last Updated**: 2025-10-26
**Formats Supported**: 24 families, 34+ variants
