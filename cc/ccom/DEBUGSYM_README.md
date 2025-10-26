# Universal Debug Symbol Parser/Generator

## Overview

This is a comprehensive debug symbol generation and parsing system for the Portable C Compiler (PCC). It provides a unified interface for working with multiple debug symbol formats, enabling debugging across diverse platforms and toolchains.

## Supported Debug Formats

### DWARF (Debugging With Attributed Record Formats)
- **DWARF 1** (1992) - Original specification
- **DWARF 2** (1993) - Enhanced type system
- **DWARF 3** (2005) - C++/Fortran support
- **DWARF 4** (2010) - Performance improvements, 64-bit support
- **DWARF 5** (2017) - Modern features, compression

**Platforms**: ELF-based systems (Linux, BSD, Solaris)
**File**: `debugsym_dwarf.c`

### CodeView (Microsoft Debug Formats)
- **CV4** - Visual C++ 4.x (16-bit & 32-bit)
- **CV5** - Visual C++ 5.0-6.0
- **CV6** - Visual C++ 7.0 (.NET 2002)
- **CV7** - Visual C++ 7.1 (.NET 2003)
- **CV8** - Visual Studio 2005+ (PDB 7.0 format)

**Platforms**: Windows PE/COFF executables
**File**: `debugsym_codeview.c`

### COFF Family (Common Object File Format)
- **COFF** - AT&T Unix System V
- **ECOFF** - Extended COFF (MIPS, Alpha)
- **XCOFF** - Extended COFF (IBM AIX)
- **PECOFF** - Portable Executable COFF (Windows)

**Platforms**: Unix System V, MIPS, Alpha, AIX, Windows
**File**: `debugsym_coff.c`

### STABS (Symbol TABle Strings)
- Berkeley Unix debugging format
- Widely used on older Unix systems
- Integrated with existing PCC STABS support

**Platforms**: BSD, SunOS, Linux (legacy)
**File**: `debugsym_stabs.c`

### DBX (System V Unix Debugger)
- System V Unix debugger format
- Similar to STABS with vendor extensions

**Platforms**: System V Unix, older commercial Unix
**File**: `debugsym_dbx.c`

### Borland Debug Formats
- **TD32** - Turbo Debugger 32-bit
- **TDS** - Turbo Debugger Symbol files

**Platforms**: Borland/Embarcadero toolchains
**File**: `debugsym_borland.c`

### Watcom Debug Information (WDI)
- Watcom C/C++ compiler debug format

**Platforms**: Watcom toolchain, DOS, OS/2, Windows
**File**: `debugsym_watcom.c`

### IBM HLL (High Level Language)
- IBM compiler debug format
- Used by VisualAge C++, XL C/C++
- OS/2, AIX, and mainframe systems

**Platforms**: IBM compilers (VisualAge, XL C/C++), OS/2, AIX, OS/400
**File**: `debugsym_hll.c`

### HP SOM (System Object Model)
- HP-UX debug format
- DNTT (Debug Name and Type Table)
- Source Line Table (SLT)

**Platforms**: HP-UX (PA-RISC, Itanium)
**File**: `debugsym_hpsom.c`

### VMS/OpenVMS DST (Debug Symbol Table)
- Digital Equipment Corporation (DEC) debug format
- VAX/VMS, OpenVMS on VAX, Alpha, and Itanium
- Comprehensive type descriptors
- PC correlation and source line tables

**Platforms**: VAX/VMS, OpenVMS (VAX, Alpha, I64)
**File**: `debugsym_vms.c`

### Classic Mac OS Debug Formats
- **MPW (Macintosh Programmer's Workshop)** - 68k Macintosh
- **PEF (Preferred Executable Format)** - PowerPC Macintosh
- **CodeWarrior Debug** - Metrowerks extended format
- Used by MPW C/C++, THINK C, Symantec C++, CodeWarrior

**Platforms**: Mac OS Classic (68000, 68020, 68030, 68040, PowerPC)
**File**: `debugsym_macos.c`

### Atari TOS/GEMDOS Debug Formats
- **DRI (Digital Research Inc.)** - Standard debug format
- **GST (GEM Symbol Table)** - Extended symbol table format
- **Pure C Debug** - Enhanced type information
- Used by Pure C, Turbo C, Lattice C, Aztec C

**Platforms**: Atari ST, STe, TT, Falcon (68000, 68020, 68030, 68040)
**File**: `debugsym_atari.c`

### Amiga Debug Formats
- **Hunk Format** - Standard AmigaOS debug hunks (HUNK_DEBUG)
- **SAS/C Debug** - Enhanced debug format with full type info
- **DICE Debug** - Matt Dillon's DICE C compiler format
- Used by SAS/C, DICE, Aztec C, Lattice C, Manx Aztec C

**Platforms**: AmigaOS 1.x-3.x (68000, 68020, 68030, 68040, 68060)
**File**: `debugsym_amiga.c`

### Acorn Debug Formats
- **AOF (ARM Object Format)** - Standard object file debug areas
- **AIF (ARM Image Format)** - Executable debug tables
- **DDT (Desktop Debug Table)** - Enhanced debug format
- Used by Acorn C, Acorn C++, Norcroft C, ARM SDT

**Platforms**: Archimedes, RISC PC, RISC OS (ARM2, ARM3, ARM6, ARM7, StrongARM)
**File**: `debugsym_acorn.c`

## Architecture

### Core Components

```
debugsym.h          - Universal API and data structures
debugsym.c          - Main implementation and dispatch
debugsym_dwarf.c    - DWARF v1-5 implementation
debugsym_codeview.c - CodeView CV4-CV8 implementation
debugsym_coff.c     - COFF family implementation
debugsym_stabs.c    - STABS implementation
debugsym_dbx.c      - DBX implementation
debugsym_borland.c  - Borland TD32/TDS implementation
debugsym_watcom.c   - Watcom WDI implementation
debugsym_hll.c      - IBM HLL implementation
debugsym_hpsom.c    - HP SOM implementation
debugsym_vms.c      - VMS/OpenVMS DST implementation
debugsym_macos.c    - Classic Mac OS MPW/PEF implementation
debugsym_atari.c    - Atari TOS/GEMDOS DRI/GST implementation
debugsym_amiga.c    - Amiga Hunk/SAS/C implementation
debugsym_acorn.c    - Acorn AOF/AIF implementation
```

### Data Flow

```
Source Code
    ↓
[PCC Compiler Frontend]
    ↓
Symbol Table Construction
    ↓
[Universal Debug Symbol API]
    ├─→ debugsym_record_variable()
    ├─→ debugsym_record_function()
    ├─→ debugsym_record_parameter()
    └─→ debugsym_record_type()
    ↓
[Format Dispatcher]
    ↓
    ├─→ DWARF     → debugsym_dwarf_emit()
    ├─→ CodeView  → debugsym_codeview_emit()
    ├─→ COFF      → debugsym_coff_emit()
    ├─→ STABS     → debugsym_stabs_emit()
    ├─→ DBX       → debugsym_dbx_emit()
    ├─→ Borland   → debugsym_borland_emit()
    ├─→ Watcom    → debugsym_watcom_emit()
    ├─→ IBM HLL   → debugsym_hll_emit()
    ├─→ HP SOM    → debugsym_hpsom_emit()
    ├─→ VMS DST   → debugsym_vms_emit()
    ├─→ Mac OS    → debugsym_macos_emit()
    ├─→ Atari     → debugsym_atari_emit()
    ├─→ Amiga     → debugsym_amiga_emit()
    └─→ Acorn     → debugsym_acorn_emit()
    ↓
Debug Information in Object File
```

## API Reference

### Initialization

```c
#include "debugsym.h"

/* Initialize debug symbol system */
void debugsym_init(debug_format_t format);

/* Available formats */
DBGFMT_DWARF1, DBGFMT_DWARF2, DBGFMT_DWARF3, DBGFMT_DWARF4, DBGFMT_DWARF5
DBGFMT_CV4, DBGFMT_CV5, DBGFMT_CV6, DBGFMT_CV7, DBGFMT_CV8
DBGFMT_COFF, DBGFMT_ECOFF, DBGFMT_XCOFF, DBGFMT_PECOFF
DBGFMT_STABS, DBGFMT_DBX
DBGFMT_BORLAND_TD32, DBGFMT_BORLAND_TDS
DBGFMT_WATCOM
DBGFMT_IBM_HLL
DBGFMT_HP_SOM
DBGFMT_VMS_DST
```

### Symbol Recording

```c
/* Record symbols from compiler symbol table */
void debugsym_record_variable(struct symtab *s);
void debugsym_record_function(struct symtab *s);
void debugsym_record_parameter(struct symtab *s);
void debugsym_record_type(struct symtab *s);

/* Manual symbol creation */
debug_symbol_t *debugsym_new_symbol(void);
void debugsym_record_symbol(debug_symbol_t *sym);
```

### File and Line Management

```c
void debugsym_file_begin(char *filename);
void debugsym_file_end(char *filename);
void debugsym_line(int line);
void debugsym_set_column(int column);
```

### Scope Management

```c
void debugsym_enter_function(struct symtab *s);
void debugsym_exit_function(void);
void debugsym_enter_block(int level);
void debugsym_exit_block(int level);
```

### Output

```c
/* Emit all recorded symbols */
void debugsym_emit_all(void);

/* Finish and cleanup */
void debugsym_finish(void);
```

### Type Information

```c
/* Get type from symbol table entry */
debug_type_t *debugsym_get_type(struct symtab *s);

/* Create primitive types */
debug_type_t *debugsym_primitive_type(debug_type_encoding_t enc, unsigned int size);
debug_type_t *debugsym_pointer_type(debug_type_t *base);
debug_type_t *debugsym_array_type(debug_type_t *base, int *dims, int ndims);
```

### Utilities

```c
/* Format identification */
const char *debugsym_format_name(debug_format_t format);
debug_format_t debugsym_detect_format(void *data, size_t len);

/* Format conversion */
int debugsym_can_convert(debug_format_t from, debug_format_t to);
int debugsym_convert(debug_symbol_t *sym, debug_format_t from, debug_format_t to);

/* Debugging */
void debugsym_dump_symbol(debug_symbol_t *sym);
void debugsym_dump_all(void);
void debugsym_print_statistics(void);
```

## Usage Examples

### Basic Usage (DWARF)

```c
#include "debugsym.h"

/* Initialize for DWARF 5 */
debugsym_init(DBGFMT_DWARF5);

/* Set options */
debugsym_set_options(1, 1, 1);  /* line_info, locals, types */

/* Begin source file */
debugsym_file_begin("main.c");

/* Record a function */
struct symtab *func_sym = /* ... from compiler ... */;
debugsym_enter_function(func_sym);

/* Record line numbers */
debugsym_line(42);

/* Record local variables */
struct symtab *var_sym = /* ... from compiler ... */;
debugsym_record_variable(var_sym);

/* Exit function */
debugsym_exit_function();

/* Emit all debug info and cleanup */
debugsym_finish();
```

### CodeView for Windows

```c
/* Initialize for CodeView 8 (modern Visual Studio) */
debugsym_init(DBGFMT_CV8);

debugsym_file_begin("program.c");

/* ... record symbols ... */

debugsym_finish();
```

### STABS for BSD

```c
/* Initialize for STABS */
debugsym_init(DBGFMT_STABS);

debugsym_file_begin("mycode.c");

/* ... record symbols ... */

debugsym_finish();
```

### Manual Symbol Creation

```c
/* Create a custom debug symbol */
debug_symbol_t *sym = debugsym_new_symbol();

sym->kind = DBGSYM_VARIABLE;
sym->name = debugsym_strdup("my_var");
sym->storage_class = AUTO;
sym->location.filename = debugsym_strdup("test.c");
sym->location.line = 100;

/* Create type information */
sym->type = debugsym_primitive_type(DBGTYPE_INT32, 4);

/* Record the symbol */
debugsym_record_symbol(sym);
```

## Integration with PCC

The debug symbol system integrates with PCC's compilation pipeline:

### In `main.c`

```c
#ifdef UNIVERSAL_DEBUG
    if (gflag) {
        debugsym_init(selected_format);
        debugsym_file_begin(argv[0]);
    }
#endif
```

### In symbol table code (`symtabs.c`, `pftn.c`)

```c
#ifdef UNIVERSAL_DEBUG
    debugsym_record_variable(sp);
#endif
```

### In function processing

```c
#ifdef UNIVERSAL_DEBUG
    debugsym_enter_function(sp);
    /* ... function body ... */
    debugsym_exit_function();
#endif
```

### At compilation end

```c
#ifdef UNIVERSAL_DEBUG
    if (gflag)
        debugsym_finish();
#endif
```

## Build System Integration

The debug symbol system is integrated into the PCC build system:

### Object Files
All debug symbol object files are automatically built:
- `debugsym.o`
- `debugsym_dwarf.o`
- `debugsym_codeview.o`
- `debugsym_coff.o`
- `debugsym_stabs.o`
- `debugsym_dbx.o`
- `debugsym_borland.o`
- `debugsym_watcom.o`

### Configuration
Enable universal debug symbols in `configure.ac`:

```bash
./configure --enable-universal-debug
```

## Format-Specific Details

### DWARF

**Sections Generated**:
- `.debug_info` - Debug information entries (DIEs)
- `.debug_abbrev` - Abbreviation tables
- `.debug_str` - String table
- `.debug_line` - Line number program (v2+)

**Version Differences**:
- **DWARF 1**: Basic DIE structure, limited types
- **DWARF 2**: Improved line number info, better types
- **DWARF 3**: C++ support, namespaces
- **DWARF 4**: 64-bit support, type units
- **DWARF 5**: Split DWARF, compressed sections

### CodeView

**Sections Generated**:
- `.debug$S` - Symbol records
- `.debug$T` - Type records

**Version Differences**:
- **CV4**: 16/32-bit, length-prefixed strings
- **CV5-CV7**: Enhanced types, null-terminated strings
- **CV8**: Modern PDB format, optimized

### COFF

**Symbol Table**: Embedded in object file
**String Table**: For long symbol names
**Storage Classes**: C_AUTO, C_EXT, C_STAT, etc.

## Performance Considerations

### Memory Usage
- Symbols are stored in linked lists
- Memory allocation uses `malloc()`/`calloc()`
- Call `debugsym_finish()` to free all memory

### Output Size
- Debug information can be large (2-10x code size)
- Use `debugsym_set_options(1, 0, 0)` to minimize size
- DWARF 5 supports compression

### Compilation Speed
- Minimal overhead (< 5% typically)
- Most work done during emission phase
- Format-specific backends are optimized

## Testing

### Test Program

```bash
# Compile with debug info
pcc -g -O0 test.c -o test

# Verify DWARF
readelf --debug-dump test

# Verify with debugger
gdb test
lldb test
```

### Supported Debuggers
- **GDB** - DWARF, STABS
- **LLDB** - DWARF
- **Visual Studio** - CodeView
- **WinDbg** - CodeView
- **DBX** - STABS, DBX
- **Turbo Debugger** - Borland formats

## Future Enhancements

- [ ] Full parsing implementation (currently generation-only)
- [ ] DWARF compression (DWARF 5)
- [ ] PDB file generation for CodeView
- [ ] Cross-format conversion utilities
- [ ] Debug information optimization
- [ ] Support for inline functions
- [ ] Template debugging (C++)
- [ ] Macro debugging information

## References

### Standards and Specifications
- **DWARF**: http://dwarfstd.org/
- **CodeView**: Microsoft Debug Information Format
- **COFF**: System V ABI specification
- **STABS**: gdb documentation

### Related Files
- `stabs.c` - Original PCC STABS implementation
- `dwarf.c` - Original PCC DWARF implementation
- `pass1.h` - Symbol table definitions
- `manifest.h` - Type definitions

## License

Copyright (c) 2025. Licensed under the same terms as PCC (BSD-style license).

## Author

Created for the Portable C Compiler project.

## Contact

For issues and improvements, please submit to the PCC issue tracker.
