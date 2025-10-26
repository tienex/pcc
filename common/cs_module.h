/*
 * C# Module Serialization Library
 * Endian-Neutral and Architecture-Neutral Module Storage
 *
 * This library provides a generic framework for serializing compiled C# modules
 * in a platform-independent format for later instantiation and linking.
 */

#ifndef _CS_MODULE_H_
#define _CS_MODULE_H_

#include <stdint.h>
#include <stdio.h>

/* Module Format Version */
#define CS_MODULE_VERSION 1
#define CS_MODULE_MAGIC   0x4353504D  /* "CSPM" - C# Portable Module */

/* Endianness Detection and Conversion */
enum cs_endian {
	CS_ENDIAN_LITTLE,
	CS_ENDIAN_BIG,
	CS_ENDIAN_UNKNOWN
};

/* Architecture Type */
enum cs_arch {
	CS_ARCH_NEUTRAL,      /* Architecture-independent IL */
	CS_ARCH_X86,
	CS_ARCH_X86_64,
	CS_ARCH_ARM,
	CS_ARCH_ARM64,
	CS_ARCH_MIPS,
	CS_ARCH_PPC,
	CS_ARCH_RISCV,
	CS_ARCH_WASM,
};

/* Module Section Types */
enum cs_section_type {
	CS_SECTION_METADATA,   /* Module metadata */
	CS_SECTION_TYPES,      /* Type definitions */
	CS_SECTION_METHODS,    /* Method definitions */
	CS_SECTION_FIELDS,     /* Field definitions */
	CS_SECTION_STRINGS,    /* String pool */
	CS_SECTION_BLOBS,      /* Binary data blobs */
	CS_SECTION_GUID,       /* GUID pool */
	CS_SECTION_CODE,       /* Compiled code (IL or native) */
	CS_SECTION_RESOURCES,  /* Embedded resources */
	CS_SECTION_DEBUG,      /* Debug information */
	CS_SECTION_CUSTOM,     /* Custom attributes */
};

/* Module Header (always stored in little-endian) */
struct cs_module_header {
	uint32_t magic;           /* Magic number: CS_MODULE_MAGIC */
	uint32_t version;         /* Format version */
	uint32_t endian;          /* Endianness of data */
	uint32_t arch;            /* Target architecture */
	uint32_t section_count;   /* Number of sections */
	uint32_t flags;           /* Module flags */
	uint64_t timestamp;       /* Creation timestamp */
	uint8_t  guid[16];        /* Module GUID */
};

/* Section Header */
struct cs_section_header {
	uint32_t type;            /* Section type */
	uint32_t flags;           /* Section flags */
	uint64_t offset;          /* Offset in file */
	uint64_t size;            /* Size in bytes */
	uint32_t entry_count;     /* Number of entries */
	uint32_t reserved;        /* Reserved for future use */
};

/* Module Metadata */
struct cs_module_metadata {
	char *name;               /* Module name */
	char *version;            /* Module version */
	char *culture;            /* Culture/locale */
	uint32_t flags;           /* Module flags */
	uint32_t ref_count;       /* Number of references */
	char **references;        /* Referenced modules */
};

/* Type Descriptor (endian-neutral) */
struct cs_type_desc {
	uint32_t token;           /* Type token */
	uint32_t flags;           /* Type flags */
	uint32_t name_offset;     /* Offset into string pool */
	uint32_t namespace_offset;/* Offset into string pool */
	uint32_t parent_token;    /* Parent type token */
	uint32_t field_count;     /* Number of fields */
	uint32_t method_count;    /* Number of methods */
	uint32_t size;            /* Size in bytes (0 for reference types) */
	uint32_t alignment;       /* Alignment requirement */
};

/* Method Descriptor (endian-neutral) */
struct cs_method_desc {
	uint32_t token;           /* Method token */
	uint32_t flags;           /* Method flags */
	uint32_t name_offset;     /* Offset into string pool */
	uint32_t signature_offset;/* Signature blob offset */
	uint32_t code_offset;     /* Code offset */
	uint32_t code_size;       /* Code size in bytes */
	uint32_t locals_count;    /* Number of local variables */
	uint32_t max_stack;       /* Maximum stack depth */
};

/* Field Descriptor (endian-neutral) */
struct cs_field_desc {
	uint32_t token;           /* Field token */
	uint32_t flags;           /* Field flags */
	uint32_t name_offset;     /* Offset into string pool */
	uint32_t type_token;      /* Field type token */
	uint32_t offset;          /* Field offset in parent type */
	uint32_t default_offset;  /* Default value blob offset */
};

/* Module Writer */
struct cs_module_writer {
	FILE *fp;
	struct cs_module_header header;
	struct cs_section_header *sections;
	int section_count;
	int section_capacity;
	enum cs_endian target_endian;
	enum cs_arch target_arch;

	/* Section buffers */
	void **section_data;
	size_t *section_sizes;
	size_t *section_capacities;
};

/* Module Reader */
struct cs_module_reader {
	FILE *fp;
	struct cs_module_header header;
	struct cs_section_header *sections;
	int section_count;
	enum cs_endian file_endian;
	enum cs_arch file_arch;

	/* Section data cache */
	void **section_data;
	int needs_swap;       /* Need to swap byte order */
};

/* Endian Conversion Functions */
enum cs_endian cs_detect_endian(void);
uint16_t cs_swap16(uint16_t val);
uint32_t cs_swap32(uint32_t val);
uint64_t cs_swap64(uint64_t val);
void cs_swap_bytes(void *data, size_t size);

/* Module Writer API */
struct cs_module_writer *cs_module_writer_create(const char *filename,
                                                   enum cs_endian endian,
                                                   enum cs_arch arch);
int cs_module_writer_add_section(struct cs_module_writer *writer,
                                  enum cs_section_type type,
                                  const void *data,
                                  size_t size);
int cs_module_writer_finalize(struct cs_module_writer *writer);
void cs_module_writer_destroy(struct cs_module_writer *writer);

/* Module Reader API */
struct cs_module_reader *cs_module_reader_open(const char *filename);
const void *cs_module_reader_get_section(struct cs_module_reader *reader,
                                          enum cs_section_type type,
                                          size_t *size);
int cs_module_reader_verify(struct cs_module_reader *reader);
void cs_module_reader_destroy(struct cs_module_reader *reader);

/* Metadata API */
struct cs_module_metadata *cs_metadata_create(const char *name,
                                               const char *version);
int cs_metadata_add_reference(struct cs_module_metadata *meta,
                               const char *ref);
void cs_metadata_destroy(struct cs_module_metadata *meta);

/* Type API */
struct cs_type_desc *cs_type_create(const char *name,
                                     const char *namespace,
                                     uint32_t flags);
void cs_type_set_parent(struct cs_type_desc *type, uint32_t parent_token);
void cs_type_set_size(struct cs_type_desc *type, uint32_t size,
                      uint32_t alignment);

/* Method API */
struct cs_method_desc *cs_method_create(const char *name, uint32_t flags);
void cs_method_set_code(struct cs_method_desc *method,
                        const void *code, size_t code_size);
void cs_method_set_signature(struct cs_method_desc *method,
                              const void *sig, size_t sig_size);

/* Field API */
struct cs_field_desc *cs_field_create(const char *name, uint32_t type_token,
                                       uint32_t flags);
void cs_field_set_offset(struct cs_field_desc *field, uint32_t offset);
void cs_field_set_default(struct cs_field_desc *field,
                          const void *value, size_t value_size);

/* String Pool API */
typedef struct cs_string_pool cs_string_pool_t;
cs_string_pool_t *cs_string_pool_create(void);
uint32_t cs_string_pool_add(cs_string_pool_t *pool, const char *str);
const char *cs_string_pool_get(cs_string_pool_t *pool, uint32_t offset);
void cs_string_pool_destroy(cs_string_pool_t *pool);

/* Blob Pool API */
typedef struct cs_blob_pool cs_blob_pool_t;
cs_blob_pool_t *cs_blob_pool_create(void);
uint32_t cs_blob_pool_add(cs_blob_pool_t *pool, const void *data, size_t size);
const void *cs_blob_pool_get(cs_blob_pool_t *pool, uint32_t offset,
                              size_t *size);
void cs_blob_pool_destroy(cs_blob_pool_t *pool);

/* Module Verification */
int cs_module_verify_integrity(struct cs_module_reader *reader);
int cs_module_verify_signature(struct cs_module_reader *reader,
                                const void *public_key);

/* Module Linking */
struct cs_module_linker;
struct cs_module_linker *cs_linker_create(void);
int cs_linker_add_module(struct cs_module_linker *linker,
                         struct cs_module_reader *reader);
int cs_linker_resolve_references(struct cs_module_linker *linker);
int cs_linker_generate_output(struct cs_module_linker *linker,
                               const char *output_file);
void cs_linker_destroy(struct cs_module_linker *linker);

/* Utility Functions */
void cs_module_print_info(struct cs_module_reader *reader);
int cs_module_is_compatible(struct cs_module_reader *reader);
const char *cs_arch_to_string(enum cs_arch arch);
const char *cs_endian_to_string(enum cs_endian endian);

#endif /* _CS_MODULE_H_ */
