/*
 * BTF (BPF Type Format) Debug Symbol Support
 *
 * Supports BTF (BPF Type Format) for Linux kernel BPF programs:
 * - BTF version 1 (Linux 4.18+)
 * - Type information for BPF programs and maps
 * - Function signatures for BPF helpers
 * - Line info and function info
 * - CO-RE (Compile Once - Run Everywhere) support
 *
 * Used by: Linux kernel BPF, libbpf, bpftool, BCC, bpftrace
 * Platforms: Linux (x86, x86_64, ARM, ARM64, RISC-V, s390, PowerPC, MIPS)
 *
 * BTF is essential for modern eBPF programming and kernel tracing.
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * BTF Header
 */
#define BTF_MAGIC		0xeB9f		/* BTF magic number */
#define BTF_VERSION		1		/* BTF version */

/*
 * BTF Type Kinds
 */
#define BTF_KIND_UNKN		0	/* Unknown */
#define BTF_KIND_INT		1	/* Integer */
#define BTF_KIND_PTR		2	/* Pointer */
#define BTF_KIND_ARRAY		3	/* Array */
#define BTF_KIND_STRUCT		4	/* Struct */
#define BTF_KIND_UNION		5	/* Union */
#define BTF_KIND_ENUM		6	/* Enumeration */
#define BTF_KIND_FWD		7	/* Forward declaration */
#define BTF_KIND_TYPEDEF	8	/* Typedef */
#define BTF_KIND_VOLATILE	9	/* Volatile */
#define BTF_KIND_CONST		10	/* Const */
#define BTF_KIND_RESTRICT	11	/* Restrict */
#define BTF_KIND_FUNC		12	/* Function */
#define BTF_KIND_FUNC_PROTO	13	/* Function prototype */
#define BTF_KIND_VAR		14	/* Variable */
#define BTF_KIND_DATASEC	15	/* Data section */
#define BTF_KIND_FLOAT		16	/* Float */
#define BTF_KIND_DECL_TAG	17	/* Decl tag */
#define BTF_KIND_TYPE_TAG	18	/* Type tag */
#define BTF_KIND_ENUM64		19	/* 64-bit enum */

/*
 * BTF Integer Encoding
 */
#define BTF_INT_SIGNED		(1 << 0)	/* Signed integer */
#define BTF_INT_CHAR		(1 << 1)	/* Character */
#define BTF_INT_BOOL		(1 << 2)	/* Boolean */

/*
 * BTF Function Linkage
 */
#define BTF_FUNC_STATIC		0	/* Static */
#define BTF_FUNC_GLOBAL		1	/* Global */
#define BTF_FUNC_EXTERN		2	/* Extern */

/*
 * BTF Variable Linkage
 */
#define BTF_VAR_STATIC		0	/* Static */
#define BTF_VAR_GLOBAL_ALLOC	1	/* Global allocated */
#define BTF_VAR_GLOBAL_EXTERN	2	/* Global extern */

/* Global state */
static unsigned int type_id = 1;	/* Type IDs start at 1 */
static unsigned int string_offset = 0;
static unsigned int type_count = 0;

/* Forward declarations */
static void emit_btf_header(void);
static void emit_btf_type(unsigned int name_off, unsigned int info,
			   unsigned int size_or_type);
static void emit_btf_int_data(unsigned int encoding, unsigned int offset,
			       unsigned int bits);
static void emit_btf_array(unsigned int type, unsigned int index_type,
			    unsigned int nelems);
static void emit_btf_var(unsigned int linkage);
static void emit_btf_func_proto_param(unsigned int name_off, unsigned int type);
static unsigned int add_btf_string(const char *str);
static unsigned int get_btf_type_id(debug_type_t *type);
static unsigned int make_btf_info(unsigned int kind, unsigned int vlen,
				   unsigned int kind_flag);

/*
 * Initialize BTF debug symbol generation
 */
void
debugsym_btf_init(void)
{
	/* Emit BTF section */
	printf("\t.section .BTF,\"a\",@progbits\n");
	printf("\t.align 4\n");
	printf("L_btf_start:\n");

	/* BTF Header (24 bytes) */
	printf("L_btf_header:\n");
	printf("\t.word 0x%04x\n", (unsigned short)BTF_MAGIC);	/* Magic */
	printf("\t.byte 0x%02x\n", (unsigned char)BTF_VERSION);	/* Version */
	printf("\t.byte 0x00\n");				/* Flags */
	printf("\t.long 0x00000018\n");				/* hdr_len (24) */
	printf("\t.long 0\n");					/* type_off */
	printf("\t.long 0\n");					/* type_len */
	printf("\t.long 0\n");					/* str_off */
	printf("\t.long 0\n");					/* str_len */

	/* Type section */
	printf("L_btf_types:\n");

	/* String table starts with empty string */
	printf("L_btf_strings:\n");
	printf("\t.byte 0\n");

	type_id = 1;
	string_offset = 1;
	type_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_btf_emit(debug_symbol_t *sym)
{
	unsigned int name_off;
	unsigned int tid;
	unsigned int info;
	unsigned int linkage;

	if (!sym)
		return;

	/* Add symbol name */
	name_off = add_btf_string(sym->name);

	/* Get type ID */
	tid = get_btf_type_id(sym->type);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* BTF_KIND_FUNC */
		if (sym->is_extern) {
			linkage = BTF_FUNC_EXTERN;
		} else if (sym->is_static) {
			linkage = BTF_FUNC_STATIC;
		} else {
			linkage = BTF_FUNC_GLOBAL;
		}

		info = make_btf_info(BTF_KIND_FUNC, linkage, 0);
		emit_btf_type(name_off, info, tid);
		type_id++;
		type_count++;
		break;

	case DBGSYM_VARIABLE:
		/* BTF_KIND_VAR */
		if (sym->is_extern) {
			linkage = BTF_VAR_GLOBAL_EXTERN;
		} else if (sym->is_static) {
			linkage = BTF_VAR_STATIC;
		} else {
			linkage = BTF_VAR_GLOBAL_ALLOC;
		}

		info = make_btf_info(BTF_KIND_VAR, 0, 0);
		emit_btf_type(name_off, info, tid);
		emit_btf_var(linkage);
		type_id++;
		type_count++;
		break;

	default:
		break;
	}
}

/*
 * Finish and close BTF debug info
 */
void
debugsym_btf_finish(void)
{
	unsigned int type_off;
	unsigned int type_len;
	unsigned int str_off;
	unsigned int str_len;

	printf("L_btf_end:\n");

	/* Calculate section sizes */
	type_off = 24;	/* After header */
	str_off = type_off + (type_count * 12); /* Simplified */
	type_len = str_off - type_off;
	str_len = string_offset;

	/* Patch BTF header */
	printf("\t.pushsection .BTF\n");
	printf("\t.org L_btf_header + 8\n");
	printf("\t.long 0x%08x\n", type_off);
	printf("\t.long 0x%08x\n", type_len);
	printf("\t.long 0x%08x\n", str_off);
	printf("\t.long 0x%08x\n", str_len);
	printf("\t.popsection\n");

	/* Return to text section */
	printf("\t.text\n");
}

/*
 * Parse BTF debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_btf_parse(void *data, size_t len)
{
	/* BTF parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit BTF header
 */
static void
emit_btf_header(void)
{
	printf("\t.word 0x%04x\n", (unsigned short)BTF_MAGIC);
	printf("\t.byte 0x%02x\n", (unsigned char)BTF_VERSION);
	printf("\t.byte 0x00\n");
	printf("\t.long 0x00000018\n");	/* hdr_len */
}

/*
 * Helper: Emit BTF type entry (12 bytes)
 */
static void
emit_btf_type(unsigned int name_off, unsigned int info,
	       unsigned int size_or_type)
{
	printf("\t.long 0x%08x\n", name_off);		/* Name offset */
	printf("\t.long 0x%08x\n", info);		/* Info */
	printf("\t.long 0x%08x\n", size_or_type);	/* Size or type */
}

/*
 * Helper: Emit BTF integer encoding (4 bytes)
 */
static void
emit_btf_int_data(unsigned int encoding, unsigned int offset,
		   unsigned int bits)
{
	unsigned int data;

	/* BTF integer encoding:
	 * Bits 0-15: offset (for bitfields)
	 * Bits 16-23: bits
	 * Bits 24-31: encoding
	 */
	data = (encoding << 24) | ((bits & 0xff) << 16) | (offset & 0xffff);

	printf("\t.long 0x%08x\n", data);
}

/*
 * Helper: Emit BTF array (12 bytes)
 */
static void
emit_btf_array(unsigned int type, unsigned int index_type,
		unsigned int nelems)
{
	printf("\t.long 0x%08x\n", type);		/* Element type */
	printf("\t.long 0x%08x\n", index_type);		/* Index type */
	printf("\t.long 0x%08x\n", nelems);		/* Number of elements */
}

/*
 * Helper: Emit BTF variable linkage (4 bytes)
 */
static void
emit_btf_var(unsigned int linkage)
{
	printf("\t.long 0x%08x\n", linkage);
}

/*
 * Helper: Emit BTF function proto parameter (8 bytes)
 */
static void
emit_btf_func_proto_param(unsigned int name_off, unsigned int type)
{
	printf("\t.long 0x%08x\n", name_off);
	printf("\t.long 0x%08x\n", type);
}

/*
 * Helper: Add string to BTF string table
 */
static unsigned int
add_btf_string(const char *str)
{
	unsigned int offset;
	int len;

	if (!str || !*str)
		return 0;

	offset = string_offset;
	len = strlen(str);

	/* Emit string */
	printf("\t.ascii \"");
	printf("%s", str);
	printf("\\0\"\n");

	string_offset += len + 1;

	return offset;
}

/*
 * Helper: Get BTF type ID for debug type
 */
static unsigned int
get_btf_type_id(debug_type_t *type)
{
	unsigned int tid;
	unsigned int name_off;
	unsigned int info;
	unsigned int encoding;

	if (!type) {
		/* Void type (type ID 0) */
		return 0;
	}

	tid = type_id;
	type_id++;
	type_count++;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		return 0;	/* Void is always type ID 0 */

	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		encoding = BTF_INT_SIGNED | BTF_INT_CHAR;
		name_off = add_btf_string("char");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 1);
		emit_btf_int_data(encoding, 0, 8);
		break;

	case DBGTYPE_UCHAR:
		encoding = BTF_INT_CHAR;
		name_off = add_btf_string("unsigned char");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 1);
		emit_btf_int_data(encoding, 0, 8);
		break;

	case DBGTYPE_INT16:
		encoding = BTF_INT_SIGNED;
		name_off = add_btf_string("short");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 2);
		emit_btf_int_data(encoding, 0, 16);
		break;

	case DBGTYPE_UINT16:
		encoding = 0;
		name_off = add_btf_string("unsigned short");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 2);
		emit_btf_int_data(encoding, 0, 16);
		break;

	case DBGTYPE_INT32:
		encoding = BTF_INT_SIGNED;
		name_off = add_btf_string("int");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 4);
		emit_btf_int_data(encoding, 0, 32);
		break;

	case DBGTYPE_UINT32:
		encoding = 0;
		name_off = add_btf_string("unsigned int");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 4);
		emit_btf_int_data(encoding, 0, 32);
		break;

	case DBGTYPE_INT64:
		encoding = BTF_INT_SIGNED;
		name_off = add_btf_string("long long");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 8);
		emit_btf_int_data(encoding, 0, 64);
		break;

	case DBGTYPE_BOOL:
		encoding = BTF_INT_BOOL;
		name_off = add_btf_string("_Bool");
		info = make_btf_info(BTF_KIND_INT, 0, 0);
		emit_btf_type(name_off, info, 1);
		emit_btf_int_data(encoding, 0, 8);
		break;

	case DBGTYPE_FLOAT32:
		name_off = add_btf_string("float");
		info = make_btf_info(BTF_KIND_FLOAT, 0, 0);
		emit_btf_type(name_off, info, 4);
		break;

	case DBGTYPE_FLOAT64:
		name_off = add_btf_string("double");
		info = make_btf_info(BTF_KIND_FLOAT, 0, 0);
		emit_btf_type(name_off, info, 8);
		break;

	case DBGTYPE_PTR:
		if (type->base_type) {
			unsigned int base_tid = get_btf_type_id(type->base_type);
			info = make_btf_info(BTF_KIND_PTR, 0, 0);
			emit_btf_type(0, info, base_tid);
		} else {
			info = make_btf_info(BTF_KIND_PTR, 0, 0);
			emit_btf_type(0, info, 0);	/* void* */
		}
		break;

	case DBGTYPE_ARRAY:
		if (type->name) {
			name_off = add_btf_string(type->name);
		} else {
			name_off = 0;
		}
		info = make_btf_info(BTF_KIND_ARRAY, 0, 0);
		emit_btf_type(name_off, info, 0);
		/* Array data would follow */
		break;

	case DBGTYPE_STRUCT:
		if (type->name) {
			name_off = add_btf_string(type->name);
		} else {
			name_off = 0;
		}
		info = make_btf_info(BTF_KIND_STRUCT, type->member_count, 0);
		emit_btf_type(name_off, info, type->size);
		break;

	case DBGTYPE_UNION:
		if (type->name) {
			name_off = add_btf_string(type->name);
		} else {
			name_off = 0;
		}
		info = make_btf_info(BTF_KIND_UNION, type->member_count, 0);
		emit_btf_type(name_off, info, type->size);
		break;

	case DBGTYPE_ENUM:
		if (type->name) {
			name_off = add_btf_string(type->name);
		} else {
			name_off = 0;
		}
		info = make_btf_info(BTF_KIND_ENUM, type->enum_value_count, 0);
		emit_btf_type(name_off, info, 4);
		break;

	case DBGTYPE_FUNCTION:
		info = make_btf_info(BTF_KIND_FUNC_PROTO, type->param_count, 0);
		emit_btf_type(0, info, 0);
		break;

	default:
		info = make_btf_info(BTF_KIND_UNKN, 0, 0);
		emit_btf_type(0, info, 0);
		break;
	}

	return tid;
}

/*
 * Helper: Make BTF info field
 */
static unsigned int
make_btf_info(unsigned int kind, unsigned int vlen, unsigned int kind_flag)
{
	/* BTF info encoding:
	 * Bits 0-15: vlen (variable length data)
	 * Bits 16-23: unused
	 * Bits 24-28: kind
	 * Bit 29-30: unused
	 * Bit 31: kind_flag
	 */
	return (kind_flag << 31) | (kind << 24) | (vlen & 0xffff);
}
