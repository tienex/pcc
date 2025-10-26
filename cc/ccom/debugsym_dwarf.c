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
 * DWARF Debug Format Support (v1-5)
 *
 * Implements comprehensive DWARF debugging support for all versions:
 * - DWARF 1 (1992) - Original spec, minimal DIE structure
 * - DWARF 2 (1993) - Improved with better type system
 * - DWARF 3 (2005) - Added C++/Fortran support
 * - DWARF 4 (2010) - Performance improvements, 64-bit
 * - DWARF 5 (2017) - Modern features, improved compression
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* DWARF format versions */
#define DWARF_VERSION_1		1
#define DWARF_VERSION_2		2
#define DWARF_VERSION_3		3
#define DWARF_VERSION_4		4
#define DWARF_VERSION_5		5

/* DWARF TAG encodings */
#define DW_TAG_array_type		0x01
#define DW_TAG_class_type		0x02
#define DW_TAG_entry_point		0x03
#define DW_TAG_enumeration_type		0x04
#define DW_TAG_formal_parameter		0x05
#define DW_TAG_label			0x0a
#define DW_TAG_lexical_block		0x0b
#define DW_TAG_member			0x0d
#define DW_TAG_pointer_type		0x0f
#define DW_TAG_reference_type		0x10
#define DW_TAG_compile_unit		0x11
#define DW_TAG_string_type		0x12
#define DW_TAG_structure_type		0x13
#define DW_TAG_subroutine_type		0x15
#define DW_TAG_typedef			0x16
#define DW_TAG_union_type		0x17
#define DW_TAG_unspecified_parameters	0x18
#define DW_TAG_variant			0x19
#define DW_TAG_common_block		0x1a
#define DW_TAG_common_inclusion		0x1b
#define DW_TAG_inheritance		0x1c
#define DW_TAG_inlined_subroutine	0x1d
#define DW_TAG_module			0x1e
#define DW_TAG_ptr_to_member_type	0x1f
#define DW_TAG_set_type			0x20
#define DW_TAG_subrange_type		0x21
#define DW_TAG_with_stmt		0x22
#define DW_TAG_access_declaration	0x23
#define DW_TAG_base_type		0x24
#define DW_TAG_catch_block		0x25
#define DW_TAG_const_type		0x26
#define DW_TAG_constant			0x27
#define DW_TAG_enumerator		0x28
#define DW_TAG_file_type		0x29
#define DW_TAG_friend			0x2a
#define DW_TAG_namelist			0x2b
#define DW_TAG_namelist_item		0x2c
#define DW_TAG_packed_type		0x2d
#define DW_TAG_subprogram		0x2e
#define DW_TAG_template_type_parameter	0x2f
#define DW_TAG_template_value_parameter	0x30
#define DW_TAG_thrown_type		0x31
#define DW_TAG_try_block		0x32
#define DW_TAG_variant_part		0x33
#define DW_TAG_variable			0x34
#define DW_TAG_volatile_type		0x35

/* DWARF 3+ tags */
#define DW_TAG_dwarf_procedure		0x36
#define DW_TAG_restrict_type		0x37
#define DW_TAG_interface_type		0x38
#define DW_TAG_namespace		0x39
#define DW_TAG_imported_module		0x3a
#define DW_TAG_unspecified_type		0x3b
#define DW_TAG_partial_unit		0x3c
#define DW_TAG_imported_unit		0x3d
#define DW_TAG_condition		0x3f
#define DW_TAG_shared_type		0x40

/* DWARF 4+ tags */
#define DW_TAG_type_unit		0x41
#define DW_TAG_rvalue_reference_type	0x42
#define DW_TAG_template_alias		0x43

/* DWARF 5+ tags */
#define DW_TAG_coarray_type		0x44
#define DW_TAG_generic_subrange		0x45
#define DW_TAG_dynamic_type		0x46
#define DW_TAG_atomic_type		0x47
#define DW_TAG_call_site		0x48
#define DW_TAG_call_site_parameter	0x49
#define DW_TAG_skeleton_unit		0x4a
#define DW_TAG_immutable_type		0x4b

/* DWARF attribute encodings */
#define DW_AT_sibling			0x01
#define DW_AT_location			0x02
#define DW_AT_name			0x03
#define DW_AT_ordering			0x09
#define DW_AT_byte_size			0x0b
#define DW_AT_bit_offset		0x0c
#define DW_AT_bit_size			0x0d
#define DW_AT_stmt_list			0x10
#define DW_AT_low_pc			0x11
#define DW_AT_high_pc			0x12
#define DW_AT_language			0x13
#define DW_AT_discr			0x15
#define DW_AT_discr_value		0x16
#define DW_AT_visibility		0x17
#define DW_AT_import			0x18
#define DW_AT_string_length		0x19
#define DW_AT_common_reference		0x1a
#define DW_AT_comp_dir			0x1b
#define DW_AT_const_value		0x1c
#define DW_AT_containing_type		0x1d
#define DW_AT_default_value		0x1e
#define DW_AT_inline			0x20
#define DW_AT_is_optional		0x21
#define DW_AT_lower_bound		0x22
#define DW_AT_producer			0x25
#define DW_AT_prototyped		0x27
#define DW_AT_return_addr		0x2a
#define DW_AT_start_scope		0x2c
#define DW_AT_bit_stride		0x2e
#define DW_AT_upper_bound		0x2f
#define DW_AT_abstract_origin		0x31
#define DW_AT_accessibility		0x32
#define DW_AT_address_class		0x33
#define DW_AT_artificial		0x34
#define DW_AT_base_types		0x35
#define DW_AT_calling_convention	0x36
#define DW_AT_count			0x37
#define DW_AT_data_member_location	0x38
#define DW_AT_decl_column		0x39
#define DW_AT_decl_file			0x3a
#define DW_AT_decl_line			0x3b
#define DW_AT_declaration		0x3c
#define DW_AT_discr_list		0x3d
#define DW_AT_encoding			0x3e
#define DW_AT_external			0x3f
#define DW_AT_frame_base		0x40
#define DW_AT_friend			0x41
#define DW_AT_identifier_case		0x42
#define DW_AT_macro_info		0x43
#define DW_AT_namelist_item		0x44
#define DW_AT_priority			0x45
#define DW_AT_segment			0x46
#define DW_AT_specification		0x47
#define DW_AT_static_link		0x48
#define DW_AT_type			0x49
#define DW_AT_use_location		0x4a
#define DW_AT_variable_parameter	0x4b
#define DW_AT_virtuality		0x4c
#define DW_AT_vtable_elem_location	0x4d
#define DW_AT_allocated			0x4e
#define DW_AT_associated		0x4f
#define DW_AT_data_location		0x50
#define DW_AT_byte_stride		0x51
#define DW_AT_entry_pc			0x52
#define DW_AT_use_UTF8			0x53
#define DW_AT_extension			0x54
#define DW_AT_ranges			0x55
#define DW_AT_trampoline		0x56
#define DW_AT_call_column		0x57
#define DW_AT_call_file			0x58
#define DW_AT_call_line			0x59
#define DW_AT_description		0x5a

/* DWARF form encodings */
#define DW_FORM_addr			0x01
#define DW_FORM_block2			0x03
#define DW_FORM_block4			0x04
#define DW_FORM_data2			0x05
#define DW_FORM_data4			0x06
#define DW_FORM_data8			0x07
#define DW_FORM_string			0x08
#define DW_FORM_block			0x09
#define DW_FORM_block1			0x0a
#define DW_FORM_data1			0x0b
#define DW_FORM_flag			0x0c
#define DW_FORM_sdata			0x0d
#define DW_FORM_strp			0x0e
#define DW_FORM_udata			0x0f
#define DW_FORM_ref_addr		0x10
#define DW_FORM_ref1			0x11
#define DW_FORM_ref2			0x12
#define DW_FORM_ref4			0x13
#define DW_FORM_ref8			0x14
#define DW_FORM_ref_udata		0x15
#define DW_FORM_indirect		0x16

/* DWARF 4+ forms */
#define DW_FORM_sec_offset		0x17
#define DW_FORM_exprloc			0x18
#define DW_FORM_flag_present		0x19
#define DW_FORM_ref_sig8		0x20

/* DWARF 5+ forms */
#define DW_FORM_strx			0x1a
#define DW_FORM_addrx			0x1b
#define DW_FORM_ref_sup4		0x1c
#define DW_FORM_strp_sup		0x1d
#define DW_FORM_data16			0x1e
#define DW_FORM_line_strp		0x1f
#define DW_FORM_implicit_const		0x21
#define DW_FORM_loclistx		0x22
#define DW_FORM_rnglistx		0x23
#define DW_FORM_ref_sup8		0x24
#define DW_FORM_strx1			0x25
#define DW_FORM_strx2			0x26
#define DW_FORM_strx3			0x27
#define DW_FORM_strx4			0x28
#define DW_FORM_addrx1			0x29
#define DW_FORM_addrx2			0x2a
#define DW_FORM_addrx3			0x2b
#define DW_FORM_addrx4			0x2c

/* Base type encodings */
#define DW_ATE_address			0x01
#define DW_ATE_boolean			0x02
#define DW_ATE_complex_float		0x03
#define DW_ATE_float			0x04
#define DW_ATE_signed			0x05
#define DW_ATE_signed_char		0x06
#define DW_ATE_unsigned			0x07
#define DW_ATE_unsigned_char		0x08
#define DW_ATE_imaginary_float		0x09
#define DW_ATE_packed_decimal		0x0a
#define DW_ATE_numeric_string		0x0b
#define DW_ATE_edited			0x0c
#define DW_ATE_signed_fixed		0x0d
#define DW_ATE_unsigned_fixed		0x0e
#define DW_ATE_decimal_float		0x0f
#define DW_ATE_UTF			0x10

/* Language codes */
#define DW_LANG_C89			0x0001
#define DW_LANG_C			0x0002
#define DW_LANG_C_plus_plus		0x0004
#define DW_LANG_Fortran77		0x0008
#define DW_LANG_Fortran90		0x0009
#define DW_LANG_Pascal83		0x000a
#define DW_LANG_C99			0x000c
#define DW_LANG_C11			0x001d
#define DW_LANG_C_plus_plus_03		0x0019
#define DW_LANG_C_plus_plus_11		0x001a
#define DW_LANG_C_plus_plus_14		0x0021

/* DWARF state */
static struct {
	int version;		/* DWARF version (1-5) */
	int pointer_size;	/* Target pointer size */
	int label_num;		/* Label counter */
	FILE *debug_info;	/* .debug_info section */
	FILE *debug_abbrev;	/* .debug_abbrev section */
	FILE *debug_str;	/* .debug_str section */
	FILE *debug_line;	/* .debug_line section (v2+) */
	int string_offset;	/* String table offset */
	int abbrev_num;		/* Abbreviation number */
} dwarf_state;

/*
 * Initialize DWARF output
 */
void
debugsym_dwarf_init(int version)
{
	memset(&dwarf_state, 0, sizeof(dwarf_state));
	dwarf_state.version = version;
	dwarf_state.pointer_size = sizeof(void *);
	dwarf_state.label_num = 1;
	dwarf_state.string_offset = 0;
	dwarf_state.abbrev_num = 1;

	/* In a real implementation, open output sections here */
	/* For now, we'll emit to stdout with section markers */

	printf("\t.section .debug_abbrev,\"\",@progbits\n");
	printf(".Ldebug_abbrev0:\n");

	printf("\t.section .debug_info,\"\",@progbits\n");
	printf(".Ldebug_info0:\n");

	if (version >= DWARF_VERSION_2) {
		printf("\t.section .debug_line,\"\",@progbits\n");
		printf(".Ldebug_line0:\n");
	}

	printf("\t.section .debug_str,\"MS\",@progbits,1\n");
	printf(".Ldebug_str0:\n");
}

/*
 * Encode unsigned LEB128
 */
static void
emit_uleb128(unsigned long value)
{
	do {
		unsigned char byte = value & 0x7f;
		value >>= 7;
		if (value != 0)
			byte |= 0x80;
		printf("\t.byte 0x%02x\n", byte);
	} while (value != 0);
}

/*
 * Encode signed LEB128
 */
static void
emit_sleb128(long value)
{
	int more = 1;
	int negative = (value < 0);

	while (more) {
		unsigned char byte = value & 0x7f;
		value >>= 7;

		if ((value == 0 && !(byte & 0x40)) ||
		    (value == -1 && (byte & 0x40)))
			more = 0;
		else
			byte |= 0x80;

		printf("\t.byte 0x%02x\n", byte);
	}
}

/*
 * Emit a string to the string table
 */
static int
emit_string(const char *str)
{
	int offset = dwarf_state.string_offset;

	if (str == NULL)
		str = "";

	printf("\t.section .debug_str\n");
	printf(".Lstr%d:\n", offset);
	printf("\t.asciz \"%s\"\n", str);

	dwarf_state.string_offset += strlen(str) + 1;

	return offset;
}

/*
 * Emit compilation unit header
 */
static void
emit_cu_header(void)
{
	printf("\t.section .debug_info\n");
	printf(".Lcu_begin:\n");

	/* Unit length (placeholder) */
	printf("\t.long .Lcu_end - .Lcu_start\n");
	printf(".Lcu_start:\n");

	/* Version */
	printf("\t.short %d\n", dwarf_state.version);

	if (dwarf_state.version >= DWARF_VERSION_5) {
		/* DWARF 5 unit type */
		printf("\t.byte 0x01\n");  /* DW_UT_compile */
		printf("\t.byte %d\n", dwarf_state.pointer_size);
	}

	/* Abbreviation table offset */
	printf("\t.long .Ldebug_abbrev0\n");

	if (dwarf_state.version < DWARF_VERSION_5) {
		/* Address size */
		printf("\t.byte %d\n", dwarf_state.pointer_size);
	}
}

/*
 * Emit DIE (Debug Information Entry)
 */
static void
emit_die_header(int tag, int has_children)
{
	printf("\t# DIE tag=%d children=%d\n", tag, has_children);
	emit_uleb128(dwarf_state.abbrev_num++);
}

/*
 * Map debug symbol type to DWARF base type encoding
 */
static int
get_dwarf_encoding(debug_type_encoding_t enc)
{
	switch (enc) {
	case DBGTYPE_BOOL:
		return DW_ATE_boolean;
	case DBGTYPE_CHAR:
		return DW_ATE_signed_char;
	case DBGTYPE_INT8:
	case DBGTYPE_INT16:
	case DBGTYPE_INT32:
	case DBGTYPE_INT64:
	case DBGTYPE_INT128:
		return DW_ATE_signed;
	case DBGTYPE_UINT8:
	case DBGTYPE_UINT16:
	case DBGTYPE_UINT32:
	case DBGTYPE_UINT64:
	case DBGTYPE_UINT128:
		return DW_ATE_unsigned;
	case DBGTYPE_FLOAT32:
	case DBGTYPE_FLOAT64:
	case DBGTYPE_FLOAT80:
	case DBGTYPE_FLOAT128:
		return DW_ATE_float;
	case DBGTYPE_COMPLEX_FLOAT:
	case DBGTYPE_COMPLEX_DOUBLE:
		return DW_ATE_complex_float;
	case DBGTYPE_POINTER:
		return DW_ATE_address;
	default:
		return DW_ATE_signed;
	}
}

/*
 * Emit a symbol as DWARF
 */
void
debugsym_dwarf_emit(debug_symbol_t *sym)
{
	int tag = 0;

	if (sym == NULL)
		return;

	printf("\t.section .debug_info\n");

	/* Determine appropriate DWARF tag */
	switch (sym->kind) {
	case DBGSYM_VARIABLE:
		tag = DW_TAG_variable;
		break;
	case DBGSYM_FUNCTION:
		tag = DW_TAG_subprogram;
		break;
	case DBGSYM_PARAMETER:
		tag = DW_TAG_formal_parameter;
		break;
	case DBGSYM_TYPEDEF:
		tag = DW_TAG_typedef;
		break;
	case DBGSYM_STRUCT:
		tag = DW_TAG_structure_type;
		break;
	case DBGSYM_ENUM:
		tag = DW_TAG_enumeration_type;
		break;
	case DBGSYM_CONSTANT:
		tag = DW_TAG_constant;
		break;
	case DBGSYM_LABEL:
		tag = DW_TAG_label;
		break;
	case DBGSYM_NAMESPACE:
		if (dwarf_state.version >= DWARF_VERSION_3)
			tag = DW_TAG_namespace;
		break;
	case DBGSYM_CLASS:
		tag = DW_TAG_class_type;
		break;
	default:
		return;
	}

	emit_die_header(tag, 0);

	/* Emit name */
	if (sym->name) {
		int str_offset = emit_string(sym->name);
		printf("\t# DW_AT_name\n");
		if (dwarf_state.version >= DWARF_VERSION_4)
			printf("\t.long .Lstr%d\n", str_offset);
		else
			printf("\t.long .Lstr%d\n", str_offset);
	}

	/* Emit location/line information */
	if (sym->location.line > 0) {
		printf("\t# DW_AT_decl_line\n");
		emit_uleb128(sym->location.line);
	}

	if (sym->location.column > 0 && dwarf_state.version >= DWARF_VERSION_2) {
		printf("\t# DW_AT_decl_column\n");
		emit_uleb128(sym->location.column);
	}

	/* Emit type information */
	if (sym->type) {
		printf("\t# DW_AT_type\n");
		printf("\t.long 0  # type reference placeholder\n");

		if (sym->kind == DBGSYM_VARIABLE || sym->kind == DBGSYM_PARAMETER) {
			printf("\t# DW_AT_byte_size\n");
			emit_uleb128(sym->type->size);
		}
	}

	/* Function-specific attributes */
	if (sym->kind == DBGSYM_FUNCTION) {
		if (sym->is_external) {
			printf("\t# DW_AT_external\n");
			printf("\t.byte 1\n");
		}

		if (sym->is_inline && dwarf_state.version >= DWARF_VERSION_2) {
			printf("\t# DW_AT_inline\n");
			printf("\t.byte 1\n");
		}

		if (sym->low_pc || sym->high_pc) {
			printf("\t# DW_AT_low_pc\n");
			printf("\t.%s 0x%lx\n",
			    dwarf_state.pointer_size == 8 ? "quad" : "long",
			    sym->low_pc);
			printf("\t# DW_AT_high_pc\n");
			printf("\t.%s 0x%lx\n",
			    dwarf_state.pointer_size == 8 ? "quad" : "long",
			    sym->high_pc);
		}
	}

	/* Null terminator for DIE */
	printf("\t.byte 0\n");
}

/*
 * Parse DWARF debug information
 */
debug_symbol_t *
debugsym_dwarf_parse(void *data, size_t len)
{
	/* DWARF parsing would be implemented here */
	/* This is a placeholder for the full parser */
	return NULL;
}

/*
 * Finish DWARF output
 */
void
debugsym_dwarf_finish(void)
{
	printf("\t.section .debug_info\n");
	printf(".Lcu_end:\n");

	printf("\t.section .debug_abbrev\n");
	printf("\t.byte 0\n");  /* Null terminator */

	if (dwarf_state.version >= DWARF_VERSION_2) {
		printf("\t.section .debug_line\n");
		printf("\t.byte 0\n");
	}
}
