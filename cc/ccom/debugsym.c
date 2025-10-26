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
 * Universal Debug Symbol Parser/Generator - Main Implementation
 *
 * This file provides the core infrastructure for a universal debug symbol
 * system that can parse and generate multiple debug formats.
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Global debug context */
debug_context_t *dbg_ctx = NULL;

/* Statistics */
static struct {
	int symbols_recorded;
	int symbols_emitted;
	int lines_processed;
	int functions_processed;
	int types_processed;
} stats;

/* ===================================================================
 * INITIALIZATION AND CLEANUP
 * =================================================================== */

/*
 * Initialize the debug symbol system
 */
void
debugsym_init(debug_format_t format)
{
	if (dbg_ctx != NULL) {
		/* Already initialized */
		return;
	}

	dbg_ctx = (debug_context_t *)calloc(1, sizeof(debug_context_t));
	if (dbg_ctx == NULL) {
		fprintf(stderr, "debugsym_init: out of memory\n");
		return;
	}

	dbg_ctx->format = format;
	dbg_ctx->source_file = NULL;
	dbg_ctx->current_line = 0;
	dbg_ctx->block_level = 0;
	dbg_ctx->symbols = NULL;
	dbg_ctx->current_func = NULL;
	dbg_ctx->format_state = NULL;

	/* Default options */
	dbg_ctx->emit_line_info = 1;
	dbg_ctx->emit_locals = 1;
	dbg_ctx->emit_types = 1;
	dbg_ctx->optimize_debug = 0;
	dbg_ctx->dwarf_version = 3;	/* Default to DWARF3 */
	dbg_ctx->codeview_version = 8;	/* Default to CV8 */

	/* Initialize format-specific backend */
	switch (format) {
	case DBGFMT_DWARF1:
	case DBGFMT_DWARF2:
	case DBGFMT_DWARF3:
	case DBGFMT_DWARF4:
	case DBGFMT_DWARF5:
		dbg_ctx->dwarf_version = format - DBGFMT_DWARF1 + 1;
		debugsym_dwarf_init(dbg_ctx->dwarf_version);
		break;

	case DBGFMT_CV4:
	case DBGFMT_CV5:
	case DBGFMT_CV6:
	case DBGFMT_CV7:
	case DBGFMT_CV8:
		dbg_ctx->codeview_version = format - DBGFMT_CV4 + 4;
		debugsym_codeview_init(dbg_ctx->codeview_version);
		break;

	case DBGFMT_COFF:
	case DBGFMT_ECOFF:
	case DBGFMT_XCOFF:
	case DBGFMT_PECOFF:
		debugsym_coff_init(format);
		break;

	case DBGFMT_STABS:
		debugsym_stabs_init();
		break;

	case DBGFMT_DBX:
		debugsym_dbx_init();
		break;

	case DBGFMT_BORLAND_TD32:
	case DBGFMT_BORLAND_TDS:
		debugsym_borland_init(format);
		break;

	case DBGFMT_WATCOM:
		debugsym_watcom_init();
		break;

	case DBGFMT_IBM_HLL:
		debugsym_hll_init();
		break;

	case DBGFMT_HP_SOM:
		debugsym_hpsom_init();
		break;

	case DBGFMT_VMS_DST:
		debugsym_vms_init();
		break;

	case DBGFMT_MACOS_MPW:
	case DBGFMT_MACOS_PEF:
		debugsym_macos_init();
		break;

	case DBGFMT_ATARI_DRI:
	case DBGFMT_ATARI_GST:
		debugsym_atari_init();
		break;

	case DBGFMT_AMIGA_HUNK:
	case DBGFMT_AMIGA_SASC:
		debugsym_amiga_init();
		break;

	case DBGFMT_ACORN_AOF:
	case DBGFMT_ACORN_AIF:
		debugsym_acorn_init();
		break;

	case DBGFMT_AOUT:
		debugsym_aout_init();
		break;

	case DBGFMT_MACHO:
		debugsym_macho_init();
		break;

	case DBGFMT_OMF:
		debugsym_omf_init();
		break;

	case DBGFMT_PDB:
		debugsym_pdb_init();
		break;

	case DBGFMT_CTF:
		debugsym_ctf_init();
		break;

	case DBGFMT_BTF:
		debugsym_btf_init();
		break;

	case DBGFMT_PLAN9:
		debugsym_plan9_init();
		break;

	case DBGFMT_TADS:
		debugsym_tads_init();
		break;

	default:
		break;
	}

	/* Clear statistics */
	memset(&stats, 0, sizeof(stats));
}

/*
 * Finish and cleanup the debug symbol system
 */
void
debugsym_finish(void)
{
	debug_symbol_t *sym, *next;

	if (dbg_ctx == NULL)
		return;

	/* Emit all pending symbols */
	debugsym_emit_all();

	/* Call format-specific cleanup */
	switch (dbg_ctx->format) {
	case DBGFMT_DWARF1:
	case DBGFMT_DWARF2:
	case DBGFMT_DWARF3:
	case DBGFMT_DWARF4:
	case DBGFMT_DWARF5:
		debugsym_dwarf_finish();
		break;

	case DBGFMT_CV4:
	case DBGFMT_CV5:
	case DBGFMT_CV6:
	case DBGFMT_CV7:
	case DBGFMT_CV8:
		debugsym_codeview_finish();
		break;

	case DBGFMT_COFF:
	case DBGFMT_ECOFF:
	case DBGFMT_XCOFF:
	case DBGFMT_PECOFF:
		debugsym_coff_finish();
		break;

	case DBGFMT_STABS:
		debugsym_stabs_finish();
		break;

	case DBGFMT_DBX:
		debugsym_dbx_finish();
		break;

	case DBGFMT_BORLAND_TD32:
	case DBGFMT_BORLAND_TDS:
		debugsym_borland_finish();
		break;

	case DBGFMT_WATCOM:
		debugsym_watcom_finish();
		break;

	case DBGFMT_IBM_HLL:
		debugsym_hll_finish();
		break;

	case DBGFMT_HP_SOM:
		debugsym_hpsom_finish();
		break;

	case DBGFMT_VMS_DST:
		debugsym_vms_finish();
		break;

	case DBGFMT_MACOS_MPW:
	case DBGFMT_MACOS_PEF:
		debugsym_macos_finish();
		break;

	case DBGFMT_ATARI_DRI:
	case DBGFMT_ATARI_GST:
		debugsym_atari_finish();
		break;

	case DBGFMT_AMIGA_HUNK:
	case DBGFMT_AMIGA_SASC:
		debugsym_amiga_finish();
		break;

	case DBGFMT_ACORN_AOF:
	case DBGFMT_ACORN_AIF:
		debugsym_acorn_finish();
		break;

	case DBGFMT_AOUT:
		debugsym_aout_finish();
		break;

	case DBGFMT_MACHO:
		debugsym_macho_finish();
		break;

	case DBGFMT_OMF:
		debugsym_omf_finish();
		break;

	case DBGFMT_PDB:
		debugsym_pdb_finish();
		break;

	case DBGFMT_CTF:
		debugsym_ctf_finish();
		break;

	case DBGFMT_BTF:
		debugsym_btf_finish();
		break;

	case DBGFMT_PLAN9:
		debugsym_plan9_finish();
		break;

	case DBGFMT_TADS:
		debugsym_tads_finish();
		break;

	default:
		break;
	}

	/* Free all symbols */
	for (sym = dbg_ctx->symbols; sym != NULL; sym = next) {
		next = sym->next;
		debugsym_free_symbol(sym);
	}

	/* Free source file name */
	if (dbg_ctx->source_file)
		free(dbg_ctx->source_file);

	/* Free context */
	free(dbg_ctx);
	dbg_ctx = NULL;
}

/*
 * Change the output format
 */
void
debugsym_set_format(debug_format_t format)
{
	if (dbg_ctx == NULL)
		return;

	dbg_ctx->format = format;

	/* Update version numbers */
	if (format >= DBGFMT_DWARF1 && format <= DBGFMT_DWARF5)
		dbg_ctx->dwarf_version = format - DBGFMT_DWARF1 + 1;
	else if (format >= DBGFMT_CV4 && format <= DBGFMT_CV8)
		dbg_ctx->codeview_version = format - DBGFMT_CV4 + 4;
}

/*
 * Set debug options
 */
void
debugsym_set_options(int line_info, int locals, int types)
{
	if (dbg_ctx == NULL)
		return;

	dbg_ctx->emit_line_info = line_info;
	dbg_ctx->emit_locals = locals;
	dbg_ctx->emit_types = types;
}

/* ===================================================================
 * FILE AND COMPILATION UNIT MANAGEMENT
 * =================================================================== */

/*
 * Begin a new source file
 */
void
debugsym_file_begin(char *filename)
{
	if (dbg_ctx == NULL)
		return;

	if (dbg_ctx->source_file)
		free(dbg_ctx->source_file);

	dbg_ctx->source_file = debugsym_strdup(filename);
	dbg_ctx->current_line = 0;
	dbg_ctx->block_level = 0;
}

/*
 * End a source file
 */
void
debugsym_file_end(char *filename)
{
	if (dbg_ctx == NULL)
		return;

	/* Emit any pending symbols for this file */
	debugsym_emit_all();
}

/*
 * Set current line number
 */
void
debugsym_line(int line)
{
	if (dbg_ctx == NULL)
		return;

	dbg_ctx->current_line = line;
	stats.lines_processed++;
}

/*
 * Set current column number
 */
void
debugsym_set_column(int column)
{
	/* Currently unused but available for future use */
}

/* ===================================================================
 * SYMBOL MANAGEMENT
 * =================================================================== */

/*
 * Create a new debug symbol
 */
debug_symbol_t *
debugsym_new_symbol(void)
{
	debug_symbol_t *sym;

	sym = (debug_symbol_t *)calloc(1, sizeof(debug_symbol_t));
	if (sym == NULL) {
		fprintf(stderr, "debugsym_new_symbol: out of memory\n");
		return NULL;
	}

	return sym;
}

/*
 * Free a debug symbol
 */
void
debugsym_free_symbol(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	if (sym->name)
		free(sym->name);
	if (sym->linkage_name)
		free(sym->linkage_name);
	if (sym->location.filename)
		free(sym->location.filename);
	if (sym->type) {
		if (sym->type->name)
			free(sym->type->name);
		if (sym->type->array_bounds)
			free(sym->type->array_bounds);
		free(sym->type);
	}
	if (sym->format_data)
		free(sym->format_data);

	free(sym);
}

/*
 * Record a symbol in the symbol list
 */
void
debugsym_record_symbol(debug_symbol_t *sym)
{
	if (dbg_ctx == NULL || sym == NULL)
		return;

	/* Add to symbol list */
	sym->next = dbg_ctx->symbols;
	dbg_ctx->symbols = sym;

	stats.symbols_recorded++;
}

/*
 * Record a variable from the compiler's symbol table
 */
void
debugsym_record_variable(struct symtab *s)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL || s == NULL)
		return;

	if (!dbg_ctx->emit_locals && s->sclass == AUTO)
		return;

	sym = debugsym_new_symbol();
	if (sym == NULL)
		return;

	sym->kind = DBGSYM_VARIABLE;
	sym->name = debugsym_strdup(s->sname);
	sym->storage_class = s->sclass;
	sym->type = debugsym_get_type(s);
	sym->location.filename = debugsym_strdup(dbg_ctx->source_file);
	sym->location.line = dbg_ctx->current_line;
	sym->scope = (s->sclass == AUTO) ? DBGSCOPE_FUNCTION : DBGSCOPE_FILE;
	sym->block_level = dbg_ctx->block_level;

	debugsym_record_symbol(sym);
}

/*
 * Record a function from the compiler's symbol table
 */
void
debugsym_record_function(struct symtab *s)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL || s == NULL)
		return;

	sym = debugsym_new_symbol();
	if (sym == NULL)
		return;

	sym->kind = DBGSYM_FUNCTION;
	sym->name = debugsym_strdup(s->sname);
	sym->storage_class = s->sclass;
	sym->type = debugsym_get_type(s);
	sym->location.filename = debugsym_strdup(dbg_ctx->source_file);
	sym->location.line = dbg_ctx->current_line;
	sym->scope = DBGSCOPE_FILE;
	sym->is_inline = (s->sflags & SINLINE) ? 1 : 0;
	sym->is_extern = (s->sclass == EXTERN || s->sclass == EXTDEF) ? 1 : 0;
	sym->is_static = (s->sclass == STATIC) ? 1 : 0;

	dbg_ctx->current_func = sym;
	debugsym_record_symbol(sym);

	stats.functions_processed++;
}

/*
 * Record a parameter from the compiler's symbol table
 */
void
debugsym_record_parameter(struct symtab *s)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL || s == NULL)
		return;

	sym = debugsym_new_symbol();
	if (sym == NULL)
		return;

	sym->kind = DBGSYM_PARAMETER;
	sym->name = debugsym_strdup(s->sname);
	sym->storage_class = s->sclass;
	sym->type = debugsym_get_type(s);
	sym->location.filename = debugsym_strdup(dbg_ctx->source_file);
	sym->location.line = dbg_ctx->current_line;
	sym->scope = DBGSCOPE_FUNCTION;

	debugsym_record_symbol(sym);
}

/*
 * Record a type from the compiler's symbol table
 */
void
debugsym_record_type(struct symtab *s)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL || s == NULL)
		return;

	if (!dbg_ctx->emit_types)
		return;

	sym = debugsym_new_symbol();
	if (sym == NULL)
		return;

	/* Determine the specific type kind */
	if (s->sclass == STNAME)
		sym->kind = DBGSYM_STRUCT;
	else if (s->sclass == UNAME)
		sym->kind = DBGSYM_STRUCT;  /* Union */
	else if (s->sclass == ENAME)
		sym->kind = DBGSYM_ENUM;
	else if (s->sclass == TYPEDEF)
		sym->kind = DBGSYM_TYPEDEF;
	else
		sym->kind = DBGSYM_TYPE;

	sym->name = debugsym_strdup(s->sname);
	sym->type = debugsym_get_type(s);
	sym->location.filename = debugsym_strdup(dbg_ctx->source_file);
	sym->location.line = dbg_ctx->current_line;
	sym->scope = DBGSCOPE_FILE;

	debugsym_record_symbol(sym);
	stats.types_processed++;
}

/* ===================================================================
 * SCOPE MANAGEMENT
 * =================================================================== */

/*
 * Enter a new scope
 */
void
debugsym_enter_scope(debug_scope_t scope)
{
	if (dbg_ctx == NULL)
		return;

	/* Scope management can be extended as needed */
}

/*
 * Exit current scope
 */
void
debugsym_exit_scope(void)
{
	if (dbg_ctx == NULL)
		return;

	/* Scope management can be extended as needed */
}

/*
 * Enter a function
 */
void
debugsym_enter_function(struct symtab *s)
{
	if (dbg_ctx == NULL)
		return;

	debugsym_record_function(s);
	dbg_ctx->block_level = 0;
}

/*
 * Exit a function
 */
void
debugsym_exit_function(void)
{
	if (dbg_ctx == NULL)
		return;

	dbg_ctx->current_func = NULL;
	dbg_ctx->block_level = 0;
}

/*
 * Enter a block
 */
void
debugsym_enter_block(int level)
{
	if (dbg_ctx == NULL)
		return;

	dbg_ctx->block_level = level;
}

/*
 * Exit a block
 */
void
debugsym_exit_block(int level)
{
	if (dbg_ctx == NULL)
		return;

	if (dbg_ctx->block_level > 0)
		dbg_ctx->block_level--;
}

/* ===================================================================
 * TYPE INFORMATION
 * =================================================================== */

/*
 * Get type information from a symbol table entry
 */
debug_type_t *
debugsym_get_type(struct symtab *s)
{
	debug_type_t *type;
	TWORD t;

	if (s == NULL)
		return NULL;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	t = s->stype;

	/* Determine type encoding */
	if (ISPTR(t)) {
		type->encoding = DBGTYPE_POINTER;
		type->size = SZPOINT(t) / SZCHAR;
	} else if (ISARY(t)) {
		type->encoding = DBGTYPE_ARRAY;
	} else {
		switch (BTYPE(t)) {
		case VOID:
			type->encoding = DBGTYPE_VOID;
			type->size = 0;
			break;
		case BOOL:
			type->encoding = DBGTYPE_BOOL;
			type->size = SZBOOL / SZCHAR;
			break;
		case CHAR:
			type->encoding = DBGTYPE_CHAR;
			type->size = SZCHAR / SZCHAR;
			break;
		case SHORT:
			type->encoding = DBGTYPE_INT16;
			type->size = SZSHORT / SZCHAR;
			break;
		case INT:
			type->encoding = DBGTYPE_INT32;
			type->size = SZINT / SZCHAR;
			break;
		case LONG:
			type->encoding = DBGTYPE_INT32;
			type->size = SZLONG / SZCHAR;
			break;
		case LONGLONG:
			type->encoding = DBGTYPE_INT64;
			type->size = SZLONGLONG / SZCHAR;
			break;
		case FLOAT:
			type->encoding = DBGTYPE_FLOAT32;
			type->size = SZFLOAT / SZCHAR;
			break;
		case DOUBLE:
			type->encoding = DBGTYPE_FLOAT64;
			type->size = SZDOUBLE / SZCHAR;
			break;
		case LDOUBLE:
			type->encoding = DBGTYPE_FLOAT80;
			type->size = SZLDOUBLE / SZCHAR;
			break;
		case STRTY:
			type->encoding = DBGTYPE_STRUCT;
			break;
		case UNIONTY:
			type->encoding = DBGTYPE_UNION;
			break;
		case ENUMTY:
			type->encoding = DBGTYPE_ENUM;
			break;
		default:
			type->encoding = DBGTYPE_VOID;
			type->size = 0;
			break;
		}
	}

	/* Handle qualifiers */
	type->is_const = (s->squal & 1) ? 1 : 0;
	type->is_volatile = (s->squal & 2) ? 1 : 0;

	return type;
}

/*
 * Create a primitive type
 */
debug_type_t *
debugsym_primitive_type(debug_type_encoding_t enc, unsigned int size)
{
	debug_type_t *type;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = enc;
	type->size = size;

	return type;
}

/*
 * Create a pointer type
 */
debug_type_t *
debugsym_pointer_type(debug_type_t *base)
{
	debug_type_t *type;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = DBGTYPE_POINTER;
	type->base_type = base;
	type->size = sizeof(void *);

	return type;
}

/*
 * Create an array type
 */
debug_type_t *
debugsym_array_type(debug_type_t *base, int *dims, int ndims)
{
	debug_type_t *type;
	int i;

	type = (debug_type_t *)calloc(1, sizeof(debug_type_t));
	if (type == NULL)
		return NULL;

	type->encoding = DBGTYPE_ARRAY;
	type->base_type = base;
	type->array_dimensions = ndims;

	if (ndims > 0) {
		type->array_bounds = (int *)calloc(ndims, sizeof(int));
		if (type->array_bounds != NULL) {
			for (i = 0; i < ndims; i++)
				type->array_bounds[i] = dims[i];
		}
	}

	return type;
}

/* ===================================================================
 * EMIT DEBUG INFORMATION
 * =================================================================== */

/*
 * Emit all recorded symbols
 */
void
debugsym_emit_all(void)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL)
		return;

	debugsym_emit_preamble();

	for (sym = dbg_ctx->symbols; sym != NULL; sym = sym->next) {
		debugsym_emit_symbol(sym);
	}

	debugsym_emit_postamble();
}

/*
 * Emit a single symbol
 */
void
debugsym_emit_symbol(debug_symbol_t *sym)
{
	if (dbg_ctx == NULL || sym == NULL)
		return;

	/* Dispatch to format-specific emitter */
	switch (dbg_ctx->format) {
	case DBGFMT_DWARF1:
	case DBGFMT_DWARF2:
	case DBGFMT_DWARF3:
	case DBGFMT_DWARF4:
	case DBGFMT_DWARF5:
		debugsym_dwarf_emit(sym);
		break;

	case DBGFMT_CV4:
	case DBGFMT_CV5:
	case DBGFMT_CV6:
	case DBGFMT_CV7:
	case DBGFMT_CV8:
		debugsym_codeview_emit(sym);
		break;

	case DBGFMT_COFF:
	case DBGFMT_ECOFF:
	case DBGFMT_XCOFF:
	case DBGFMT_PECOFF:
		debugsym_coff_emit(sym);
		break;

	case DBGFMT_STABS:
		debugsym_stabs_emit(sym);
		break;

	case DBGFMT_DBX:
		debugsym_dbx_emit(sym);
		break;

	case DBGFMT_BORLAND_TD32:
	case DBGFMT_BORLAND_TDS:
		debugsym_borland_emit(sym);
		break;

	case DBGFMT_WATCOM:
		debugsym_watcom_emit(sym);
		break;

	case DBGFMT_IBM_HLL:
		debugsym_hll_emit(sym);
		break;

	case DBGFMT_HP_SOM:
		debugsym_hpsom_emit(sym);
		break;

	case DBGFMT_VMS_DST:
		debugsym_vms_emit(sym);
		break;

	case DBGFMT_MACOS_MPW:
	case DBGFMT_MACOS_PEF:
		debugsym_macos_emit(sym);
		break;

	case DBGFMT_ATARI_DRI:
	case DBGFMT_ATARI_GST:
		debugsym_atari_emit(sym);
		break;

	case DBGFMT_AMIGA_HUNK:
	case DBGFMT_AMIGA_SASC:
		debugsym_amiga_emit(sym);
		break;

	case DBGFMT_ACORN_AOF:
	case DBGFMT_ACORN_AIF:
		debugsym_acorn_emit(sym);
		break;

	case DBGFMT_AOUT:
		debugsym_aout_emit(sym);
		break;

	case DBGFMT_MACHO:
		debugsym_macho_emit(sym);
		break;

	case DBGFMT_OMF:
		debugsym_omf_emit(sym);
		break;

	case DBGFMT_PDB:
		debugsym_pdb_emit(sym);
		break;

	case DBGFMT_CTF:
		debugsym_ctf_emit(sym);
		break;

	case DBGFMT_BTF:
		debugsym_btf_emit(sym);
		break;

	case DBGFMT_PLAN9:
		debugsym_plan9_emit(sym);
		break;

	case DBGFMT_TADS:
		debugsym_tads_emit(sym);
		break;

	default:
		break;
	}

	stats.symbols_emitted++;
}

/*
 * Emit preamble/header for debug section
 */
void
debugsym_emit_preamble(void)
{
	/* Format-specific preambles are handled in init functions */
}

/*
 * Emit postamble/footer for debug section
 */
void
debugsym_emit_postamble(void)
{
	/* Format-specific postambles are handled in finish functions */
}

/* ===================================================================
 * UTILITY FUNCTIONS
 * =================================================================== */

/*
 * Get format name string
 */
const char *
debugsym_format_name(debug_format_t format)
{
	static const char *names[] = {
		"None",
		"DWARF1", "DWARF2", "DWARF3", "DWARF4", "DWARF5",
		"CodeView4", "CodeView5", "CodeView6", "CodeView7", "CodeView8",
		"COFF", "ECOFF", "XCOFF", "PECOFF",
		"STABS", "DBX",
		"Borland TD32", "Borland TDS", "Watcom",
		"IBM HLL", "HP SOM", "VMS DST",
		"Mac OS MPW", "Mac OS PEF",
		"Atari DRI", "Atari GST",
		"Amiga Hunk", "Amiga SAS/C",
		"Acorn AOF", "Acorn AIF",
		"a.out", "Mach-O", "OMF",
		"PDB", "CTF", "BTF", "Plan 9", "TADS"
	};

	if (format < 0 || format >= DBGFMT_MAX)
		return "Unknown";

	return names[format];
}

/*
 * Detect debug format from data
 */
debug_format_t
debugsym_detect_format(void *data, size_t len)
{
	unsigned char *p = (unsigned char *)data;

	if (len < 4)
		return DBGFMT_NONE;

	/* Check for DWARF magic */
	if (p[0] == 0x7f && p[1] == 'E' && p[2] == 'L' && p[3] == 'F')
		return DBGFMT_DWARF3;  /* Assume DWARF3 for ELF */

	/* Check for PE/COFF magic */
	if (p[0] == 'M' && p[1] == 'Z')
		return DBGFMT_PECOFF;

	/* Check for CodeView signature */
	if (len >= 8 && p[0] == 'N' && p[1] == 'B')
		return DBGFMT_CV8;

	/* Check for STABS (harder to detect, look for .stab strings) */
	/* This is heuristic-based */

	return DBGFMT_NONE;
}

/*
 * Check if conversion between formats is possible
 */
int
debugsym_can_convert(debug_format_t from, debug_format_t to)
{
	/* Most conversions are possible with some information loss */
	if (from == DBGFMT_NONE || to == DBGFMT_NONE)
		return 0;

	/* Same format is always OK */
	if (from == to)
		return 1;

	/* DWARF versions can convert between themselves */
	if (from >= DBGFMT_DWARF1 && from <= DBGFMT_DWARF5 &&
	    to >= DBGFMT_DWARF1 && to <= DBGFMT_DWARF5)
		return 1;

	/* CodeView versions can convert between themselves */
	if (from >= DBGFMT_CV4 && from <= DBGFMT_CV8 &&
	    to >= DBGFMT_CV4 && to <= DBGFMT_CV8)
		return 1;

	/* STABS and DBX are similar */
	if ((from == DBGFMT_STABS && to == DBGFMT_DBX) ||
	    (from == DBGFMT_DBX && to == DBGFMT_STABS))
		return 1;

	/* Generic conversions with potential information loss */
	return 1;
}

/*
 * Convert a symbol between formats
 */
int
debugsym_convert(debug_symbol_t *sym, debug_format_t from, debug_format_t to)
{
	if (sym == NULL)
		return 0;

	if (!debugsym_can_convert(from, to))
		return 0;

	/* Conversion logic would go here */
	/* For now, just a placeholder */

	return 1;
}

/*
 * Dump a symbol for debugging
 */
void
debugsym_dump_symbol(debug_symbol_t *sym)
{
	if (sym == NULL)
		return;

	fprintf(stderr, "Symbol: %s\n", sym->name ? sym->name : "(null)");
	fprintf(stderr, "  Kind: %d\n", sym->kind);
	fprintf(stderr, "  Location: %s:%d\n",
	    sym->location.filename ? sym->location.filename : "(null)",
	    sym->location.line);
	fprintf(stderr, "  Storage class: %d\n", sym->storage_class);
	fprintf(stderr, "  Scope: %d\n", sym->scope);
}

/*
 * Dump all symbols for debugging
 */
void
debugsym_dump_all(void)
{
	debug_symbol_t *sym;

	if (dbg_ctx == NULL)
		return;

	fprintf(stderr, "=== Debug Symbol Dump ===\n");
	fprintf(stderr, "Format: %s\n", debugsym_format_name(dbg_ctx->format));
	fprintf(stderr, "Source: %s\n", dbg_ctx->source_file ? dbg_ctx->source_file : "(null)");
	fprintf(stderr, "\n");

	for (sym = dbg_ctx->symbols; sym != NULL; sym = sym->next) {
		debugsym_dump_symbol(sym);
		fprintf(stderr, "\n");
	}
}

/*
 * Print statistics
 */
void
debugsym_print_statistics(void)
{
	if (dbg_ctx == NULL)
		return;

	fprintf(stderr, "=== Debug Symbol Statistics ===\n");
	fprintf(stderr, "Format: %s\n", debugsym_format_name(dbg_ctx->format));
	fprintf(stderr, "Symbols recorded: %d\n", stats.symbols_recorded);
	fprintf(stderr, "Symbols emitted: %d\n", stats.symbols_emitted);
	fprintf(stderr, "Lines processed: %d\n", stats.lines_processed);
	fprintf(stderr, "Functions processed: %d\n", stats.functions_processed);
	fprintf(stderr, "Types processed: %d\n", stats.types_processed);
}

/*
 * Allocate memory
 */
void *
debugsym_alloc(size_t size)
{
	return calloc(1, size);
}

/*
 * Free memory
 */
void
debugsym_free(void *ptr)
{
	if (ptr)
		free(ptr);
}

/*
 * Duplicate a string
 */
char *
debugsym_strdup(const char *s)
{
	char *p;
	size_t len;

	if (s == NULL)
		return NULL;

	len = strlen(s) + 1;
	p = (char *)malloc(len);
	if (p == NULL)
		return NULL;

	memcpy(p, s, len);
	return p;
}
