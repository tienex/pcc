/*	$Id$	*/

/*
 * Copyright (c) 2025
 * All rights reserved.
 *
 * Debug Symbol Emission Wrapper - Implementation
 *
 * This provides architecture-independent debug symbol emission using
 * libx86asm for x86 targets and printf() for other architectures.
 */

#include "debug_emit.h"
#include <string.h>

#if DEBUG_USE_X86ASM
/* Access the global asm context from architecture-specific code */
extern x86asm_ctx_t *asm_ctx;

x86asm_ctx_t *
debug_get_asm_ctx(void)
{
	return asm_ctx;
}
#endif

/*
 * Emit a debug section directive
 */
void
debug_emit_section(const char *name, const char *flags, const char *type)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		/* Use libx86asm for section switching */
		x86asm_segment(asm_ctx, SEG_TEXT, name);  /* Custom section */
	} else
#endif
	{
		/* Fall back to printf for non-x86 or uninitialized context */
		if (flags && flags[0] && type && type[0])
			printf("\t.section\t%s,\"%s\",%s\n", name, flags, type);
		else if (type && type[0])
			printf("\t.section\t%s,\"\"%s\n", name, type);
		else
			printf("\t.section\t%s\n", name);
	}
}

/*
 * Emit a label
 */
void
debug_emit_label(const char *name)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_label(asm_ctx, name, 0);  /* Non-global label */
	} else
#endif
	{
		printf("%s:\n", name);
	}
}

/*
 * Emit a single byte
 */
void
debug_emit_byte(uint8_t value)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_BYTE, &value, 1);
	} else
#endif
	{
		printf("\t.byte\t0x%02x\n", value);
	}
}

/*
 * Emit multiple bytes
 */
void
debug_emit_bytes(const uint8_t *data, size_t count)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_BYTE, data, count);
	} else
#endif
	{
		size_t i;
		printf("\t.byte\t");
		for (i = 0; i < count; i++) {
			if (i > 0) printf(", ");
			printf("0x%02x", data[i]);
		}
		printf("\n");
	}
}

/*
 * Emit a 16-bit word
 */
void
debug_emit_word(uint16_t value)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_WORD, &value, 1);
	} else
#endif
	{
		printf("\t.short\t0x%04x\n", value);
	}
}

/*
 * Emit multiple words
 */
void
debug_emit_words(const uint16_t *data, size_t count)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_WORD, data, count);
	} else
#endif
	{
		size_t i;
		printf("\t.short\t");
		for (i = 0; i < count; i++) {
			if (i > 0) printf(", ");
			printf("0x%04x", data[i]);
		}
		printf("\n");
	}
}

/*
 * Emit a 32-bit long/dword
 */
void
debug_emit_long(uint32_t value)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_DWORD, &value, 1);
	} else
#endif
	{
		printf("\t.long\t0x%08x\n", (unsigned int)value);
	}
}

/*
 * Emit multiple longs
 */
void
debug_emit_longs(const uint32_t *data, size_t count)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_DWORD, data, count);
	} else
#endif
	{
		size_t i;
		printf("\t.long\t");
		for (i = 0; i < count; i++) {
			if (i > 0) printf(", ");
			printf("0x%08x", (unsigned int)data[i]);
		}
		printf("\n");
	}
}

/*
 * Emit a 64-bit quad
 */
void
debug_emit_quad(uint64_t value)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_QWORD, &value, 1);
	} else
#endif
	{
		printf("\t.quad\t0x%016llx\n", (unsigned long long)value);
	}
}

/*
 * Emit multiple quads
 */
void
debug_emit_quads(const uint64_t *data, size_t count)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_QWORD, data, count);
	} else
#endif
	{
		size_t i;
		printf("\t.quad\t");
		for (i = 0; i < count; i++) {
			if (i > 0) printf(", ");
			printf("0x%016llx", (unsigned long long)data[i]);
		}
		printf("\n");
	}
}

/*
 * Emit a null-terminated string
 */
void
debug_emit_asciz(const char *str)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		/* Use DATA_ASCIZ which includes the null terminator */
		x86asm_data(asm_ctx, DATA_ASCIZ, str, strlen(str));
	} else
#endif
	{
		printf("\t.asciz\t\"");
		/* Escape special characters */
		while (*str) {
			if (*str == '"' || *str == '\\')
				printf("\\%c", *str);
			else if (*str >= 32 && *str < 127)
				printf("%c", *str);
			else
				printf("\\%03o", (unsigned char)*str);
			str++;
		}
		printf("\"\n");
	}
}

/*
 * Emit a string without null terminator
 */
void
debug_emit_string(const char *str, size_t len)
{
#if DEBUG_USE_X86ASM
	if (asm_ctx) {
		x86asm_data(asm_ctx, DATA_STRING, str, len);
	} else
#endif
	{
		size_t i;
		printf("\t.ascii\t\"");
		for (i = 0; i < len; i++) {
			if (str[i] == '"' || str[i] == '\\')
				printf("\\%c", str[i]);
			else if (str[i] >= 32 && str[i] < 127)
				printf("%c", str[i]);
			else
				printf("\\%03o", (unsigned char)str[i]);
		}
		printf("\"\n");
	}
}

/*
 * Emit a label reference (address)
 */
void
debug_emit_ref(const char *label, int size)
{
	/* For now, use printf even on x86 as this requires symbol reference support */
	/* TODO: Add x86asm_data_ref() API to libx86asm for symbol references */
	switch (size) {
	case 1:
		printf("\t.byte\t%s\n", label);
		break;
	case 2:
		printf("\t.short\t%s\n", label);
		break;
	case 4:
		printf("\t.long\t%s\n", label);
		break;
	case 8:
		printf("\t.quad\t%s\n", label);
		break;
	default:
		printf("\t.long\t%s\n", label);
		break;
	}
}

/*
 * Emit an offset expression (end - start)
 */
void
debug_emit_offset(const char *end_label, const char *start_label, int size)
{
	/* For now, use printf even on x86 as this requires expression support */
	/* TODO: Add x86asm_data_expr() API to libx86asm for expressions */
	switch (size) {
	case 1:
		printf("\t.byte\t%s - %s\n", end_label, start_label);
		break;
	case 2:
		printf("\t.short\t%s - %s\n", end_label, start_label);
		break;
	case 4:
		printf("\t.long\t%s - %s\n", end_label, start_label);
		break;
	case 8:
		printf("\t.quad\t%s - %s\n", end_label, start_label);
		break;
	default:
		printf("\t.long\t%s - %s\n", end_label, start_label);
		break;
	}
}
