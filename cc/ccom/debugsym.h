/*	$Id$	*/

/*
 * Copyright (c) 2025 Claude AI Assistant.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Universal Debug Symbol Parser/Generator
 *
 * Provides a unified interface for multiple debug symbol formats:
 * - CodeView (CV4, CV5, CV6, CV7, CV8 - Microsoft formats)
 * - DWARF (v1, v2, v3, v4, v5 - ELF standard)
 * - COFF/ECOFF/XCOFF/PECOFF debugging (AT&T/Unix/Windows)
 * - STABS (Berkeley/SunOS/Linux)
 * - DBX (System V Unix)
 * - Borland Debug Symbols (TD32, TDS)
 * - Watcom Debug Symbols (WDI)
 * - IBM HLL (VisualAge, XL C/C++)
 * - HP SOM (HP-UX)
 * - VMS DST (VAX/VMS, OpenVMS)
 * - Classic Mac OS (MPW/PEF for 68k/PowerPC)
 * - Atari TOS/GEMDOS (DRI/GST)
 * - Amiga (Hunk/SAS/C)
 * - Acorn RISC OS (AOF/AIF)
 * - a.out (Original Unix executable format)
 * - Mach-O (Modern macOS/iOS/Darwin)
 * - OMF (MS-DOS/OS/2 Object Module Format)
 * - PDB (Program Database - Microsoft Visual Studio)
 * - CTF (Compact C Type Format - Solaris/illumos/FreeBSD DTrace)
 * - BTF (BPF Type Format - Linux eBPF)
 * - Plan 9 (Bell Labs)
 * - TADS (Turbo Assembler Debug Symbols)
 *
 * Supports 22+ programming languages:
 * - C, C++, Objective-C, Objective-C++
 * - Fortran (77, 90, 95, 2003, 2008, 2018)
 * - Go, Rust, Zig, D, Crystal
 * - Pascal, Modula-2, Modula-3, Oberon
 * - Ada (83, 95, 2005, 2012)
 * - COBOL (74, 85, 2002)
 * - Haskell, OCaml, Prolog
 * - Dart, Kotlin, Java, C#
 */

#ifndef DEBUGSYM_H
#define DEBUGSYM_H

#include <sys/types.h>

/* Forward declaration for language support */
typedef struct language_attributes language_attributes_t;

/* Forward declarations */
struct symtab;
union dimfun;
struct attr;

/*
 * Debug symbol format enumeration
 */
typedef enum {
	DBGFMT_NONE = 0,

	/* DWARF family */
	DBGFMT_DWARF1,		/* DWARF version 1 (1992) */
	DBGFMT_DWARF2,		/* DWARF version 2 (1993) */
	DBGFMT_DWARF3,		/* DWARF version 3 (2005) */
	DBGFMT_DWARF4,		/* DWARF version 4 (2010) */
	DBGFMT_DWARF5,		/* DWARF version 5 (2017) */

	/* CodeView family (Microsoft) */
	DBGFMT_CV4,		/* CodeView 4.x (VC++ 4.x) */
	DBGFMT_CV5,		/* CodeView 5.x (VC++ 5.x-6.x) */
	DBGFMT_CV6,		/* CodeView 6.x (VC++ 7.x) */
	DBGFMT_CV7,		/* CodeView 7.x (VC++ 2002-2003) */
	DBGFMT_CV8,		/* CodeView 8.x (VC++ 2005+, PDB 7.0) */

	/* COFF family */
	DBGFMT_COFF,		/* COFF debugging (AT&T Unix System V) */
	DBGFMT_ECOFF,		/* Extended COFF (MIPS, Alpha) */
	DBGFMT_XCOFF,		/* Extended COFF (IBM AIX) */
	DBGFMT_PECOFF,		/* Portable Executable COFF (Windows) */

	/* Berkeley/Unix formats */
	DBGFMT_STABS,		/* STABS (Symbol TABle Strings) */
	DBGFMT_DBX,		/* DBX format (System V Unix) */

	/* Vendor-specific formats */
	DBGFMT_BORLAND_TD32,	/* Borland Turbo Debugger 32-bit */
	DBGFMT_BORLAND_TDS,	/* Borland symbol files (.TDS) */
	DBGFMT_WATCOM,		/* Watcom debug info (WDI) */
	DBGFMT_IBM_HLL,		/* IBM High Level Language (VisualAge, XL C/C++) */
	DBGFMT_HP_SOM,		/* HP System Object Model (HP-UX) */
	DBGFMT_VMS_DST,		/* VMS/OpenVMS Debug Symbol Table */

	/* Classic platform formats */
	DBGFMT_MACOS_MPW,	/* Classic Mac OS MPW (68k) */
	DBGFMT_MACOS_PEF,	/* Classic Mac OS PEF (PowerPC) */
	DBGFMT_ATARI_DRI,	/* Atari TOS/GEMDOS DRI format */
	DBGFMT_ATARI_GST,	/* Atari GEM Symbol Table (GST) */
	DBGFMT_AMIGA_HUNK,	/* Amiga Hunk format */
	DBGFMT_AMIGA_SASC,	/* Amiga SAS/C debug format */
	DBGFMT_ACORN_AOF,	/* Acorn ARM Object Format (AOF) */
	DBGFMT_ACORN_AIF,	/* Acorn ARM Image Format (AIF) */

	/* Additional executable formats */
	DBGFMT_AOUT,		/* a.out executable format (Unix) */
	DBGFMT_MACHO,		/* Mach-O (modern macOS/iOS/Darwin) */
	DBGFMT_OMF,		/* Object Module Format (MS-DOS/OS/2) */

	/* Modern and specialized formats */
	DBGFMT_PDB,		/* Program Database (Microsoft Visual Studio) */
	DBGFMT_CTF,		/* Compact C Type Format (Solaris/illumos/FreeBSD) */
	DBGFMT_BTF,		/* BPF Type Format (Linux kernel BPF) */
	DBGFMT_PLAN9,		/* Plan 9 from Bell Labs */
	DBGFMT_TADS,		/* Turbo Assembler Debug Symbols (Borland) */

	/* Enterprise/Mobile/Embedded formats */
	DBGFMT_NLM,		/* NetWare Loadable Module (Novell NetWare) */
	DBGFMT_SYMBIAN,		/* Symbian OS (Nokia, Sony Ericsson, Samsung) */
	DBGFMT_PALMOS,		/* Palm OS (Palm Pilot, Handspring, Sony Clié) */
	DBGFMT_VXWORKS,		/* VxWorks RTOS (Wind River) */

	DBGFMT_MAX
} debug_format_t;

/*
 * Debug symbol types
 */
typedef enum {
	DBGSYM_VARIABLE,	/* Variable (local, global, static) */
	DBGSYM_FUNCTION,	/* Function or procedure */
	DBGSYM_PARAMETER,	/* Function parameter */
	DBGSYM_TYPE,		/* Type definition */
	DBGSYM_STRUCT,		/* Structure or union */
	DBGSYM_ENUM,		/* Enumeration */
	DBGSYM_TYPEDEF,		/* Type alias */
	DBGSYM_LABEL,		/* Code label */
	DBGSYM_NAMESPACE,	/* Namespace (C++) */
	DBGSYM_CLASS,		/* Class (C++/Pascal) */
	DBGSYM_TEMPLATE,	/* Template (C++) */
	DBGSYM_CONSTANT,	/* Compile-time constant */
	DBGSYM_MODULE,		/* Module (Fortran/Pascal) */
	DBGSYM_COMMON,		/* Common block (Fortran) */
} debug_symbol_type_t;

/*
 * Debug scope information
 */
typedef enum {
	DBGSCOPE_GLOBAL,	/* Global scope */
	DBGSCOPE_FILE,		/* File/compilation unit scope */
	DBGSCOPE_FUNCTION,	/* Function scope */
	DBGSCOPE_BLOCK,		/* Block scope */
	DBGSCOPE_CLASS,		/* Class scope */
	DBGSCOPE_NAMESPACE,	/* Namespace scope */
} debug_scope_t;

/*
 * Debug location information
 */
typedef struct debug_location {
	char *filename;		/* Source file name */
	int line;		/* Line number */
	int column;		/* Column number (optional) */
	unsigned long address;	/* Code address */
} debug_location_t;

/*
 * Type encoding for primitive types
 */
typedef enum {
	DBGTYPE_VOID = 0,
	DBGTYPE_INT8,
	DBGTYPE_INT16,
	DBGTYPE_INT32,
	DBGTYPE_INT64,
	DBGTYPE_INT128,
	DBGTYPE_UINT8,
	DBGTYPE_UINT16,
	DBGTYPE_UINT32,
	DBGTYPE_UINT64,
	DBGTYPE_UINT128,
	DBGTYPE_FLOAT32,
	DBGTYPE_FLOAT64,
	DBGTYPE_FLOAT80,
	DBGTYPE_FLOAT128,
	DBGTYPE_COMPLEX_FLOAT,
	DBGTYPE_COMPLEX_DOUBLE,
	DBGTYPE_POINTER,
	DBGTYPE_ARRAY,
	DBGTYPE_STRUCT,
	DBGTYPE_UNION,
	DBGTYPE_ENUM,
	DBGTYPE_FUNCTION,
	DBGTYPE_BOOL,
	DBGTYPE_CHAR,
	DBGTYPE_WCHAR,
	DBGTYPE_STRING,

	/* Language-specific types */
	DBGTYPE_CLASS,			/* C++ class */
	DBGTYPE_INTERFACE,		/* Go interface, Rust trait, etc. */
	DBGTYPE_REFERENCE,		/* C++ reference */
	DBGTYPE_RVALUE_REFERENCE,	/* C++ rvalue reference (&&) */
	DBGTYPE_SET,			/* Pascal set */
	DBGTYPE_SUBRANGE,		/* Pascal subrange */
	DBGTYPE_MODULE,			/* Fortran module, Ada package */
	DBGTYPE_VARIANT,		/* OCaml variant, Rust enum */
	DBGTYPE_CHANNEL,		/* Go channel */
	DBGTYPE_SLICE,			/* Go slice */
	DBGTYPE_MAP,			/* Go map */
	DBGTYPE_OPTION,			/* Rust Option<T> */
	DBGTYPE_RESULT,			/* Rust Result<T, E> */
	DBGTYPE_ERROR_UNION,		/* Zig error union */
	DBGTYPE_OPTIONAL,		/* Zig optional (?T) */
} debug_type_encoding_t;

/*
 * Structure/union member information
 */
typedef struct debug_member {
	char *name;		/* Member name */
	struct debug_type *type; /* Member type */
	unsigned int offset;	/* Byte offset from start */
	unsigned int bit_size;	/* Bit field size (0 if not bitfield) */
	unsigned int bit_offset; /* Bit field offset */
} debug_member_t;

/*
 * Enumeration value information
 */
typedef struct debug_enum_value {
	char *name;		/* Enumerator name */
	long value;		/* Enumerator value */
} debug_enum_value_t;

/*
 * Type information
 */
typedef struct debug_type {
	debug_type_encoding_t encoding;
	unsigned int size;	/* Size in bytes */
	int is_const;		/* const qualified */
	int is_volatile;	/* volatile qualified */
	int is_restrict;	/* restrict qualified */
	struct debug_type *base_type; /* For pointers, arrays, functions */
	int array_dimensions;	/* For arrays */
	int *array_bounds;	/* Array dimension sizes */
	int *array_sizes;	/* Array dimension sizes (Fortran) */
	char *name;		/* Type name (for structs, typedefs) */

	/* Composite type members */
	debug_member_t *members; /* Struct/union members */
	int member_count;	/* Number of members */

	/* Enumeration values */
	debug_enum_value_t *enum_values; /* Enum values */
	int enum_value_count;	/* Number of enum values */

	/* Function type parameters */
	struct debug_type **param_types; /* Parameter types */
	int param_count;	/* Number of parameters */

	/* Type cache linkage */
	struct debug_type *next_cached;
} debug_type_t;

/*
 * Universal debug symbol record
 */
typedef struct debug_symbol {
	debug_symbol_type_t kind;
	char *name;		/* Symbol name */
	char *linkage_name;	/* Mangled/linkage name (C++, etc.) */
	debug_location_t location;
	debug_scope_t scope;
	debug_type_t *type;	/* Type information */

	/* Storage information */
	int storage_class;	/* AUTO, STATIC, EXTERN, etc. */
	long offset;		/* Stack offset or address */
	int register_num;	/* Register number (if in register) */
	int is_register;	/* Symbol in register */

	/* Function-specific */
	int is_inline;		/* Inline function */
	int is_extern;		/* External linkage */
	int is_static;		/* Static linkage */
	int is_artificial;	/* Compiler-generated */
	unsigned long low_pc;	/* Function start address */
	unsigned long high_pc;	/* Function end address */
	int frame_base;		/* Frame base register */

	/* Block scope */
	int block_level;	/* Nesting level */

	/* Language-specific extensions */
	int language_tag;	/* Language-specific tag (see debugsym_lang.h) */
	language_attributes_t *lang_attrs;	/* Language-specific attributes */

	/* Backend-specific data */
	void *format_data;	/* Format-specific extension data */

	/* Linked list */
	struct debug_symbol *next;
} debug_symbol_t;

/*
 * Debug information context
 */
typedef struct debug_context {
	debug_format_t format;		/* Selected output format */
	int source_language;		/* Source language (from debugsym_lang.h) */
	char *source_file;		/* Current source file */
	int current_line;		/* Current line number */
	int block_level;		/* Current block nesting */
	debug_symbol_t *symbols;	/* Symbol list */
	debug_symbol_t *current_func;	/* Current function being compiled */

	/* Format-specific state */
	void *format_state;

	/* Options */
	int emit_line_info;		/* Emit line number information */
	int emit_locals;		/* Emit local variable info */
	int emit_types;			/* Emit full type information */
	int optimize_debug;		/* Optimize debug info size */
	int dwarf_version;		/* DWARF version (1-5) */
	int codeview_version;		/* CodeView version (4-8) */
} debug_context_t;

/*
 * Global debug context
 */
extern debug_context_t *dbg_ctx;

/* ===================================================================
 * UNIVERSAL DEBUG SYMBOL API
 * =================================================================== */

/*
 * Initialization and cleanup
 */
void debugsym_init(debug_format_t format);
void debugsym_finish(void);
void debugsym_set_format(debug_format_t format);
void debugsym_set_options(int line_info, int locals, int types);

/*
 * File and compilation unit management
 */
void debugsym_file_begin(char *filename);
void debugsym_file_end(char *filename);
void debugsym_line(int line);
void debugsym_set_column(int column);

/*
 * Symbol recording
 */
debug_symbol_t *debugsym_new_symbol(void);
void debugsym_free_symbol(debug_symbol_t *sym);
void debugsym_record_symbol(debug_symbol_t *sym);
void debugsym_record_variable(struct symtab *s);
void debugsym_record_function(struct symtab *s);
void debugsym_record_parameter(struct symtab *s);
void debugsym_record_type(struct symtab *s);

/*
 * Scope management
 */
void debugsym_enter_scope(debug_scope_t scope);
void debugsym_exit_scope(void);
void debugsym_enter_function(struct symtab *s);
void debugsym_exit_function(void);
void debugsym_enter_block(int level);
void debugsym_exit_block(int level);

/*
 * Type information
 */
void debugsym_type_init(void);
debug_type_t *debugsym_get_type(struct symtab *s);
debug_type_t *debugsym_get_type_enhanced(struct symtab *s);
debug_type_t *debugsym_get_type_enhanced_base(struct symtab *s, TWORD t);
debug_type_t *debugsym_primitive_type(debug_type_encoding_t enc, unsigned int size);
debug_type_t *debugsym_get_primitive_type(debug_type_encoding_t enc, unsigned int size);
debug_type_t *debugsym_pointer_type(debug_type_t *base);
debug_type_t *debugsym_array_type(debug_type_t *base, int *dims, int ndims);
debug_type_t *debugsym_function_type(debug_type_t *return_type, debug_type_t **param_types, int param_count);
debug_type_t *debugsym_const_type(debug_type_t *base);
debug_type_t *debugsym_volatile_type(debug_type_t *base);
debug_type_t *debugsym_restrict_type(debug_type_t *base);
debug_type_t *debugsym_composite_type(debug_type_encoding_t enc, const char *name,
    debug_member_t *members, int member_count, unsigned int size);
debug_type_t *debugsym_enum_type(const char *name, debug_enum_value_t *values, int value_count);
unsigned int debugsym_type_size(debug_type_t *type);
unsigned int debugsym_type_align(debug_type_t *type);
void debugsym_type_stats(void);

/*
 * Emit debug information
 */
void debugsym_emit_all(void);
void debugsym_emit_symbol(debug_symbol_t *sym);
void debugsym_emit_preamble(void);
void debugsym_emit_postamble(void);

/* ===================================================================
 * FORMAT-SPECIFIC PARSERS AND GENERATORS
 * =================================================================== */

/*
 * DWARF format support (v1-5)
 */
void debugsym_dwarf_init(int version);
void debugsym_dwarf_emit(debug_symbol_t *sym);
void debugsym_dwarf_finish(void);
debug_symbol_t *debugsym_dwarf_parse(void *data, size_t len);

/*
 * CodeView format support (CV4-CV8)
 */
void debugsym_codeview_init(int version);
void debugsym_codeview_emit(debug_symbol_t *sym);
void debugsym_codeview_finish(void);
debug_symbol_t *debugsym_codeview_parse(void *data, size_t len);

/*
 * COFF family support
 */
void debugsym_coff_init(debug_format_t type);
void debugsym_coff_emit(debug_symbol_t *sym);
void debugsym_coff_finish(void);
debug_symbol_t *debugsym_coff_parse(void *data, size_t len);

/*
 * STABS format support
 */
void debugsym_stabs_init(void);
void debugsym_stabs_emit(debug_symbol_t *sym);
void debugsym_stabs_finish(void);
debug_symbol_t *debugsym_stabs_parse(void *data, size_t len);

/*
 * DBX format support
 */
void debugsym_dbx_init(void);
void debugsym_dbx_emit(debug_symbol_t *sym);
void debugsym_dbx_finish(void);
debug_symbol_t *debugsym_dbx_parse(void *data, size_t len);

/*
 * Borland debug format support
 */
void debugsym_borland_init(debug_format_t type);
void debugsym_borland_emit(debug_symbol_t *sym);
void debugsym_borland_finish(void);
debug_symbol_t *debugsym_borland_parse(void *data, size_t len);

/*
 * Watcom debug format support
 */
void debugsym_watcom_init(void);
void debugsym_watcom_emit(debug_symbol_t *sym);
void debugsym_watcom_finish(void);
debug_symbol_t *debugsym_watcom_parse(void *data, size_t len);

/*
 * IBM HLL debug format support
 */
void debugsym_hll_init(void);
void debugsym_hll_emit(debug_symbol_t *sym);
void debugsym_hll_finish(void);
debug_symbol_t *debugsym_hll_parse(void *data, size_t len);

/*
 * HP SOM debug format support
 */
void debugsym_hpsom_init(void);
void debugsym_hpsom_emit(debug_symbol_t *sym);
void debugsym_hpsom_finish(void);
debug_symbol_t *debugsym_hpsom_parse(void *data, size_t len);

/*
 * VMS/OpenVMS DST debug format support
 */
void debugsym_vms_init(void);
void debugsym_vms_emit(debug_symbol_t *sym);
void debugsym_vms_finish(void);
debug_symbol_t *debugsym_vms_parse(void *data, size_t len);

/*
 * Classic Mac OS debug format support (MPW/PEF)
 */
void debugsym_macos_init(void);
void debugsym_macos_emit(debug_symbol_t *sym);
void debugsym_macos_finish(void);
debug_symbol_t *debugsym_macos_parse(void *data, size_t len);

/*
 * Atari TOS/GEMDOS debug format support (DRI/GST)
 */
void debugsym_atari_init(void);
void debugsym_atari_emit(debug_symbol_t *sym);
void debugsym_atari_finish(void);
debug_symbol_t *debugsym_atari_parse(void *data, size_t len);

/*
 * Amiga debug format support (Hunk/SAS/C)
 */
void debugsym_amiga_init(void);
void debugsym_amiga_emit(debug_symbol_t *sym);
void debugsym_amiga_finish(void);
debug_symbol_t *debugsym_amiga_parse(void *data, size_t len);

/*
 * Acorn debug format support (AOF/AIF)
 */
void debugsym_acorn_init(void);
void debugsym_acorn_emit(debug_symbol_t *sym);
void debugsym_acorn_finish(void);
debug_symbol_t *debugsym_acorn_parse(void *data, size_t len);

/*
 * a.out debug format support (Unix)
 */
void debugsym_aout_init(void);
void debugsym_aout_emit(debug_symbol_t *sym);
void debugsym_aout_finish(void);
debug_symbol_t *debugsym_aout_parse(void *data, size_t len);

/*
 * Mach-O debug format support (macOS/iOS/Darwin)
 */
void debugsym_macho_init(void);
void debugsym_macho_emit(debug_symbol_t *sym);
void debugsym_macho_finish(void);
debug_symbol_t *debugsym_macho_parse(void *data, size_t len);

/*
 * OMF debug format support (MS-DOS/OS/2)
 */
void debugsym_omf_init(void);
void debugsym_omf_emit(debug_symbol_t *sym);
void debugsym_omf_finish(void);
debug_symbol_t *debugsym_omf_parse(void *data, size_t len);

/*
 * PDB debug format support (Microsoft Visual Studio)
 */
void debugsym_pdb_init(void);
void debugsym_pdb_emit(debug_symbol_t *sym);
void debugsym_pdb_finish(void);
debug_symbol_t *debugsym_pdb_parse(void *data, size_t len);

/*
 * CTF debug format support (Solaris/illumos/FreeBSD)
 */
void debugsym_ctf_init(void);
void debugsym_ctf_emit(debug_symbol_t *sym);
void debugsym_ctf_finish(void);
debug_symbol_t *debugsym_ctf_parse(void *data, size_t len);

/*
 * BTF debug format support (Linux kernel BPF)
 */
void debugsym_btf_init(void);
void debugsym_btf_emit(debug_symbol_t *sym);
void debugsym_btf_finish(void);
debug_symbol_t *debugsym_btf_parse(void *data, size_t len);

/*
 * Plan 9 debug format support
 */
void debugsym_plan9_init(void);
void debugsym_plan9_emit(debug_symbol_t *sym);
void debugsym_plan9_finish(void);
debug_symbol_t *debugsym_plan9_parse(void *data, size_t len);

/*
 * TADS debug format support (Borland)
 */
void debugsym_tads_init(void);
void debugsym_tads_emit(debug_symbol_t *sym);
void debugsym_tads_finish(void);
debug_symbol_t *debugsym_tads_parse(void *data, size_t len);

/*
 * NetWare NLM debug format support
 */
void debugsym_nlm_init(void);
void debugsym_nlm_emit(debug_symbol_t *sym);
void debugsym_nlm_finish(void);
int debugsym_nlm_parse(void *data, size_t len);

/*
 * Symbian OS debug format support
 */
void debugsym_symbian_init(void);
void debugsym_symbian_emit(debug_symbol_t *sym);
void debugsym_symbian_finish(void);
int debugsym_symbian_parse(void *data, size_t len);

/*
 * Palm OS debug format support
 */
void debugsym_palmos_init(void);
void debugsym_palmos_emit(debug_symbol_t *sym);
void debugsym_palmos_finish(void);
int debugsym_palmos_parse(void *data, size_t len);

/*
 * VxWorks RTOS debug format support
 */
void debugsym_vxworks_init(void);
void debugsym_vxworks_emit(debug_symbol_t *sym);
void debugsym_vxworks_finish(void);
int debugsym_vxworks_parse(void *data, size_t len);

/* ===================================================================
 * UTILITY FUNCTIONS
 * =================================================================== */

/*
 * Format identification and conversion
 */
const char *debugsym_format_name(debug_format_t format);
debug_format_t debugsym_detect_format(void *data, size_t len);
int debugsym_can_convert(debug_format_t from, debug_format_t to);
int debugsym_convert(debug_symbol_t *sym, debug_format_t from, debug_format_t to);

/*
 * Symbol dumping and debugging
 */
void debugsym_dump_symbol(debug_symbol_t *sym);
void debugsym_dump_all(void);
void debugsym_print_statistics(void);

/*
 * Memory management
 */
void *debugsym_alloc(size_t size);
void debugsym_free(void *ptr);
char *debugsym_strdup(const char *s);

/* ===================================================================
 * INTEGRATION WITH PCC COMPILER
 * =================================================================== */

/*
 * Initialize/finish integration
 */
void debugsym_integration_init(const char *filename, int enable_debug);
void debugsym_integration_finish(void);

/*
 * Source location tracking
 */
void debugsym_integration_line(int line);

/*
 * Symbol recording hooks
 */
void debugsym_integration_variable(struct symtab *sp);
void debugsym_integration_function_begin(struct symtab *sp);
void debugsym_integration_function_end(void);
void debugsym_integration_parameter(struct symtab *sp);
void debugsym_integration_type(struct symtab *sp);

/*
 * Scope tracking hooks
 */
void debugsym_integration_block_begin(int level);
void debugsym_integration_block_end(int level);

/*
 * Query functions
 */
int debugsym_integration_enabled(void);
debug_format_t debugsym_integration_get_format(void);
void debugsym_integration_set_auto_record(int enable);

#endif /* DEBUGSYM_H */
