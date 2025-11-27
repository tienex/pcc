/*
 * CTF (Compact C Type Format) Debug Symbol Support
 *
 * Supports CTF (Compact C Type Format):
 * - CTF version 2 (Solaris 10+)
 * - CTF version 3 (illumos, FreeBSD 11.2+, Linux)
 * - Type information in .SUNW_ctf section (Solaris) or .ctf section
 * - Function and data object information
 * - Parent/child CTF containers
 *
 * Used by: Solaris, OpenSolaris, illumos, FreeBSD, DTrace
 * Platforms: SPARC, x86, x86_64 (Solaris/illumos/FreeBSD)
 *
 * CTF is designed to be compact and efficient for kernel debugging
 * and DTrace probes.
 */

#include "pass1.h"
#include "debugsym.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * CTF File Header (ctf_header_t)
 */
#define CTF_MAGIC		0xcff1	/* CTF magic number */
#define CTF_VERSION_2		2	/* Solaris 10 */
#define CTF_VERSION_3		3	/* illumos/FreeBSD/Linux */

/*
 * CTF Header Flags
 */
#define CTF_F_COMPRESS		0x0001	/* Data is compressed */

/*
 * CTF Type Kinds (encoded in type info)
 */
#define CTF_K_UNKNOWN		0	/* Unknown type */
#define CTF_K_INTEGER		1	/* Integer */
#define CTF_K_FLOAT		2	/* Floating point */
#define CTF_K_POINTER		3	/* Pointer */
#define CTF_K_ARRAY		4	/* Array */
#define CTF_K_FUNCTION		5	/* Function */
#define CTF_K_STRUCT		6	/* Structure */
#define CTF_K_UNION		7	/* Union */
#define CTF_K_ENUM		8	/* Enumeration */
#define CTF_K_FORWARD		9	/* Forward declaration */
#define CTF_K_TYPEDEF		10	/* Typedef */
#define CTF_K_VOLATILE		11	/* Volatile */
#define CTF_K_CONST		12	/* Const */
#define CTF_K_RESTRICT		13	/* Restrict */
#define CTF_K_SLICE		14	/* Bitfield slice */

/*
 * CTF Integer Encoding
 */
#define CTF_INT_SIGNED		0x01	/* Signed integer */
#define CTF_INT_CHAR		0x02	/* Character */
#define CTF_INT_BOOL		0x04	/* Boolean */
#define CTF_INT_VARARGS		0x08	/* Varargs */

/*
 * CTF Function Encoding
 */
#define CTF_FUNC_VARARGS	0x01	/* Function has varargs */

/* Global state */
static int ctf_version = CTF_VERSION_3;	/* Default to version 3 */
static unsigned int type_count = 0;
static unsigned int string_offset = 0;
static unsigned int func_count = 0;
static unsigned int object_count = 0;

/* Forward declarations */
static void emit_ctf_header(void);
static void emit_ctf_type(unsigned int kind, unsigned int name_off,
			   unsigned int size_or_type, unsigned int vlen);
static void emit_ctf_integer(unsigned int encoding, unsigned int offset,
			      unsigned int bits);
static void emit_ctf_array(unsigned int elem_type, unsigned int index_type,
			    unsigned int nelems);
static unsigned int add_ctf_string(const char *str);
static unsigned int get_ctf_type_id(debug_type_t *type);

/*
 * Initialize CTF debug symbol generation
 */
void
debugsym_ctf_init(void)
{
	const char *version = getenv("PCC_CTF_VERSION");

	/* Determine CTF version */
	if (version) {
		if (strcmp(version, "2") == 0) {
			ctf_version = CTF_VERSION_2;
		} else if (strcmp(version, "3") == 0) {
			ctf_version = CTF_VERSION_3;
		}
	}

	/* Emit CTF section */
	printf("\t.section .SUNW_ctf,\"\",@progbits\n");
	printf("\t.align 4\n");
	printf("L_ctf_start:\n");

	/* CTF Header will be emitted in finish() */
	printf("L_ctf_header:\n");
	printf("\t.space 36\n");	/* Reserve space for header */

	/* Type section */
	printf("L_ctf_types:\n");

	/* String table starts with empty string */
	printf("L_ctf_strings:\n");
	printf("\t.byte 0\n");

	type_count = 0;
	string_offset = 1;
	func_count = 0;
	object_count = 0;
}

/*
 * Emit a debug symbol
 */
void
debugsym_ctf_emit(debug_symbol_t *sym)
{
	unsigned int type_id;
	unsigned int name_off;

	if (!sym)
		return;

	/* Add symbol name to string table */
	name_off = add_ctf_string(sym->name);

	/* Get CTF type ID */
	type_id = get_ctf_type_id(sym->type);

	switch (sym->kind) {
	case DBGSYM_FUNCTION:
		/* CTF function info */
		printf("\t.word 0x%04x\n", (unsigned short)name_off);
		printf("\t.word 0x%04x\n", (unsigned short)type_id);
		func_count++;
		break;

	case DBGSYM_VARIABLE:
		if (sym->is_external) {
			/* CTF data object */
			printf("\t.word 0x%04x\n", (unsigned short)name_off);
			printf("\t.word 0x%04x\n", (unsigned short)type_id);
			object_count++;
		}
		break;

	case DBGSYM_TYPE:
		/* Type already added via get_ctf_type_id */
		break;

	default:
		break;
	}
}

/*
 * Finish and close CTF debug info
 */
void
debugsym_ctf_finish(void)
{
	unsigned int header_offset;
	unsigned int types_offset;
	unsigned int strings_offset;
	unsigned int total_size;

	printf("L_ctf_end:\n");

	/* Calculate offsets and sizes */
	header_offset = 0;
	types_offset = 36;	/* After header */
	strings_offset = types_offset + (type_count * 8); /* Simplified */

	total_size = strings_offset + string_offset;

	/* Emit CTF header at reserved location */
	printf("\t.pushsection .SUNW_ctf\n");
	printf("\t.org L_ctf_header\n");

	/* CTF Header (36 bytes) */
	printf("\t.word 0x%04x\n", (unsigned short)CTF_MAGIC);	/* Magic */
	printf("\t.byte 0x%02x\n", (unsigned char)ctf_version);	/* Version */
	printf("\t.byte 0x00\n");				/* Flags */
	printf("\t.long 0x00000000\n");				/* Parent label */
	printf("\t.long 0x00000000\n");				/* Parent name */
	printf("\t.long 0x00000000\n");				/* Label section */
	printf("\t.long 0x%08x\n", func_count);			/* Object section */
	printf("\t.long 0x%08x\n", func_count + object_count);	/* Function section */
	printf("\t.long 0x%08x\n", types_offset);		/* Type section */
	printf("\t.long 0x%08x\n", strings_offset);		/* String section */
	printf("\t.long 0x%08x\n", string_offset);		/* String length */

	printf("\t.popsection\n");

	/* Return to text section */
	printf("\t.text\n");
}

/*
 * Parse CTF debug symbols (placeholder)
 */
debug_symbol_t *
debugsym_ctf_parse(void *data, size_t len)
{
	/* CTF parsing not yet implemented */
	return NULL;
}

/*
 * Helper: Emit CTF header
 */
static void
emit_ctf_header(void)
{
	printf("\t.word 0x%04x\n", (unsigned short)CTF_MAGIC);
	printf("\t.byte 0x%02x\n", (unsigned char)ctf_version);
	printf("\t.byte 0x00\n");	/* Flags */
}

/*
 * Helper: Emit CTF type entry
 */
static void
emit_ctf_type(unsigned int kind, unsigned int name_off,
	       unsigned int size_or_type, unsigned int vlen)
{
	unsigned int info;

	/* CTF type info encoding:
	 * Bits 0-15: vlen (variable length data)
	 * Bits 16-20: kind
	 * Bits 21-30: reserved
	 * Bit 31: isroot
	 */
	info = (kind << 11) | (vlen & 0x7ff);

	printf("\t.long 0x%08x\n", name_off);		/* Name offset */
	printf("\t.long 0x%08x\n", info);		/* Info */
	printf("\t.long 0x%08x\n", size_or_type);	/* Size or type */

	type_count++;
}

/*
 * Helper: Emit CTF integer encoding
 */
static void
emit_ctf_integer(unsigned int encoding, unsigned int offset,
		  unsigned int bits)
{
	unsigned int data;

	/* CTF integer encoding:
	 * Bits 0-15: offset (for bitfields)
	 * Bits 16-23: bits
	 * Bits 24-31: encoding
	 */
	data = (encoding << 24) | ((bits & 0xff) << 16) | (offset & 0xffff);

	printf("\t.long 0x%08x\n", data);
}

/*
 * Helper: Emit CTF array info
 */
static void
emit_ctf_array(unsigned int elem_type, unsigned int index_type,
		unsigned int nelems)
{
	printf("\t.long 0x%08x\n", elem_type);		/* Element type */
	printf("\t.long 0x%08x\n", index_type);		/* Index type */
	printf("\t.long 0x%08x\n", nelems);		/* Number of elements */
}

/*
 * Helper: Add string to CTF string table
 */
static unsigned int
add_ctf_string(const char *str)
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
 * Helper: Get CTF type ID for debug type
 */
static unsigned int
get_ctf_type_id(debug_type_t *type)
{
	unsigned int type_id;
	unsigned int name_off;
	unsigned int encoding;

	if (!type) {
		/* Void type */
		type_id = type_count + 1;
		emit_ctf_type(CTF_K_UNKNOWN, 0, 0, 0);
		return type_id;
	}

	type_id = type_count + 1;

	switch (type->encoding) {
	case DBGTYPE_VOID:
		emit_ctf_type(CTF_K_UNKNOWN, 0, 0, 0);
		break;

	case DBGTYPE_CHAR:
	case DBGTYPE_SCHAR:
		encoding = CTF_INT_SIGNED | CTF_INT_CHAR;
		name_off = add_ctf_string("char");
		emit_ctf_type(CTF_K_INTEGER, name_off, 1, 0);
		emit_ctf_integer(encoding, 0, 8);
		break;

	case DBGTYPE_UCHAR:
		encoding = CTF_INT_CHAR;
		name_off = add_ctf_string("unsigned char");
		emit_ctf_type(CTF_K_INTEGER, name_off, 1, 0);
		emit_ctf_integer(encoding, 0, 8);
		break;

	case DBGTYPE_INT16:
		encoding = CTF_INT_SIGNED;
		name_off = add_ctf_string("short");
		emit_ctf_type(CTF_K_INTEGER, name_off, 2, 0);
		emit_ctf_integer(encoding, 0, 16);
		break;

	case DBGTYPE_UINT16:
		encoding = 0;
		name_off = add_ctf_string("unsigned short");
		emit_ctf_type(CTF_K_INTEGER, name_off, 2, 0);
		emit_ctf_integer(encoding, 0, 16);
		break;

	case DBGTYPE_INT32:
		encoding = CTF_INT_SIGNED;
		name_off = add_ctf_string("int");
		emit_ctf_type(CTF_K_INTEGER, name_off, 4, 0);
		emit_ctf_integer(encoding, 0, 32);
		break;

	case DBGTYPE_UINT32:
		encoding = 0;
		name_off = add_ctf_string("unsigned int");
		emit_ctf_type(CTF_K_INTEGER, name_off, 4, 0);
		emit_ctf_integer(encoding, 0, 32);
		break;

	case DBGTYPE_INT64:
		encoding = CTF_INT_SIGNED;
		name_off = add_ctf_string("long long");
		emit_ctf_type(CTF_K_INTEGER, name_off, 8, 0);
		emit_ctf_integer(encoding, 0, 64);
		break;

	case DBGTYPE_FLOAT32:
		name_off = add_ctf_string("float");
		emit_ctf_type(CTF_K_FLOAT, name_off, 4, 0);
		emit_ctf_integer(0, 0, 32);
		break;

	case DBGTYPE_FLOAT64:
		name_off = add_ctf_string("double");
		emit_ctf_type(CTF_K_FLOAT, name_off, 8, 0);
		emit_ctf_integer(0, 0, 64);
		break;

	case DBGTYPE_PTR:
		if (type->base_type) {
			unsigned int base_id = get_ctf_type_id(type->base_type);
			emit_ctf_type(CTF_K_POINTER, 0, base_id, 0);
		} else {
			emit_ctf_type(CTF_K_POINTER, 0, 0, 0);
		}
		break;

	case DBGTYPE_ARRAY:
		if (type->name) {
			name_off = add_ctf_string(type->name);
		} else {
			name_off = 0;
		}
		emit_ctf_type(CTF_K_ARRAY, name_off, 0, 0);
		/* Array info would follow */
		break;

	case DBGTYPE_STRUCT:
		if (type->name) {
			name_off = add_ctf_string(type->name);
		} else {
			name_off = 0;
		}
		emit_ctf_type(CTF_K_STRUCT, name_off, type->size,
			       type->member_count);
		break;

	case DBGTYPE_UNION:
		if (type->name) {
			name_off = add_ctf_string(type->name);
		} else {
			name_off = 0;
		}
		emit_ctf_type(CTF_K_UNION, name_off, type->size,
			       type->member_count);
		break;

	case DBGTYPE_ENUM:
		if (type->name) {
			name_off = add_ctf_string(type->name);
		} else {
			name_off = 0;
		}
		emit_ctf_type(CTF_K_ENUM, name_off, 4,
			       type->enum_value_count);
		break;

	case DBGTYPE_FUNCTION:
		emit_ctf_type(CTF_K_FUNCTION, 0, 0, type->param_count);
		break;

	default:
		emit_ctf_type(CTF_K_UNKNOWN, 0, 0, 0);
		break;
	}

	return type_id;
}
