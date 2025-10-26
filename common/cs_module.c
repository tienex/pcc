/*
 * C# Module Serialization Library Implementation
 * Endian-Neutral and Architecture-Neutral Module Storage
 */

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "cs_module.h"

/* ========== Endian Detection and Conversion ========== */

enum cs_endian cs_detect_endian(void) {
	union {
		uint32_t i;
		uint8_t c[4];
	} test = { 0x01020304 };

	if (test.c[0] == 0x01)
		return CS_ENDIAN_BIG;
	else if (test.c[0] == 0x04)
		return CS_ENDIAN_LITTLE;
	else
		return CS_ENDIAN_UNKNOWN;
}

uint16_t cs_swap16(uint16_t val) {
	return ((val & 0x00FF) << 8) |
	       ((val & 0xFF00) >> 8);
}

uint32_t cs_swap32(uint32_t val) {
	return ((val & 0x000000FF) << 24) |
	       ((val & 0x0000FF00) << 8) |
	       ((val & 0x00FF0000) >> 8) |
	       ((val & 0xFF000000) >> 24);
}

uint64_t cs_swap64(uint64_t val) {
	return ((val & 0x00000000000000FFULL) << 56) |
	       ((val & 0x000000000000FF00ULL) << 40) |
	       ((val & 0x0000000000FF0000ULL) << 24) |
	       ((val & 0x00000000FF000000ULL) << 8) |
	       ((val & 0x000000FF00000000ULL) >> 8) |
	       ((val & 0x0000FF0000000000ULL) >> 24) |
	       ((val & 0x00FF000000000000ULL) >> 40) |
	       ((val & 0xFF00000000000000ULL) >> 56);
}

void cs_swap_bytes(void *data, size_t size) {
	uint8_t *bytes = (uint8_t *)data;
	size_t i;
	for (i = 0; i < size / 2; i++) {
		uint8_t tmp = bytes[i];
		bytes[i] = bytes[size - 1 - i];
		bytes[size - 1 - i] = tmp;
	}
}

/* Helper: Convert values based on endianness */
static void cs_convert_endian(void *data, size_t size, int needs_swap) {
	if (!needs_swap)
		return;

	switch (size) {
	case 2:
		*(uint16_t *)data = cs_swap16(*(uint16_t *)data);
		break;
	case 4:
		*(uint32_t *)data = cs_swap32(*(uint32_t *)data);
		break;
	case 8:
		*(uint64_t *)data = cs_swap64(*(uint64_t *)data);
		break;
	default:
		cs_swap_bytes(data, size);
		break;
	}
}

/* ========== Module Writer Implementation ========== */

struct cs_module_writer *cs_module_writer_create(const char *filename,
                                                   enum cs_endian endian,
                                                   enum cs_arch arch) {
	struct cs_module_writer *writer;

	writer = calloc(1, sizeof(struct cs_module_writer));
	if (!writer)
		return NULL;

	writer->fp = fopen(filename, "wb");
	if (!writer->fp) {
		free(writer);
		return NULL;
	}

	/* Initialize header */
	writer->header.magic = CS_MODULE_MAGIC;
	writer->header.version = CS_MODULE_VERSION;
	writer->header.endian = endian;
	writer->header.arch = arch;
	writer->header.section_count = 0;
	writer->header.flags = 0;
	writer->header.timestamp = (uint64_t)time(NULL);

	/* Generate GUID (simple version - use proper UUID in production) */
	for (int i = 0; i < 16; i++) {
		writer->header.guid[i] = (uint8_t)(rand() & 0xFF);
	}

	writer->target_endian = endian;
	writer->target_arch = arch;
	writer->section_capacity = 16;
	writer->sections = calloc(writer->section_capacity,
	                          sizeof(struct cs_section_header));
	writer->section_data = calloc(writer->section_capacity, sizeof(void *));
	writer->section_sizes = calloc(writer->section_capacity, sizeof(size_t));
	writer->section_capacities = calloc(writer->section_capacity,
	                                    sizeof(size_t));

	/* Reserve space for header (write at end) */
	fseek(writer->fp, sizeof(struct cs_module_header), SEEK_SET);

	return writer;
}

int cs_module_writer_add_section(struct cs_module_writer *writer,
                                  enum cs_section_type type,
                                  const void *data,
                                  size_t size) {
	if (!writer || !data || size == 0)
		return -1;

	/* Expand section arrays if needed */
	if (writer->section_count >= writer->section_capacity) {
		writer->section_capacity *= 2;
		writer->sections = realloc(writer->sections,
		                           writer->section_capacity *
		                           sizeof(struct cs_section_header));
		writer->section_data = realloc(writer->section_data,
		                               writer->section_capacity *
		                               sizeof(void *));
		writer->section_sizes = realloc(writer->section_sizes,
		                                writer->section_capacity *
		                                sizeof(size_t));
		writer->section_capacities = realloc(writer->section_capacities,
		                                     writer->section_capacity *
		                                     sizeof(size_t));
	}

	int idx = writer->section_count;
	struct cs_section_header *sec = &writer->sections[idx];

	/* Fill section header */
	sec->type = type;
	sec->flags = 0;
	sec->offset = ftell(writer->fp);
	sec->size = size;
	sec->entry_count = 0;  /* Set by caller if needed */
	sec->reserved = 0;

	/* Write section data */
	fwrite(data, 1, size, writer->fp);

	/* Store section info */
	writer->section_data[idx] = malloc(size);
	memcpy(writer->section_data[idx], data, size);
	writer->section_sizes[idx] = size;

	writer->section_count++;
	writer->header.section_count++;

	return idx;
}

int cs_module_writer_finalize(struct cs_module_writer *writer) {
	if (!writer || !writer->fp)
		return -1;

	long section_table_offset = ftell(writer->fp);

	/* Write section table */
	enum cs_endian host_endian = cs_detect_endian();
	int needs_swap = (host_endian != writer->target_endian);

	for (int i = 0; i < writer->section_count; i++) {
		struct cs_section_header sec = writer->sections[i];

		/* Convert to target endianness */
		if (needs_swap) {
			sec.type = cs_swap32(sec.type);
			sec.flags = cs_swap32(sec.flags);
			sec.offset = cs_swap64(sec.offset);
			sec.size = cs_swap64(sec.size);
			sec.entry_count = cs_swap32(sec.entry_count);
			sec.reserved = cs_swap32(sec.reserved);
		}

		fwrite(&sec, sizeof(struct cs_section_header), 1, writer->fp);
	}

	/* Write header at beginning */
	fseek(writer->fp, 0, SEEK_SET);

	struct cs_module_header hdr = writer->header;
	if (needs_swap) {
		hdr.magic = cs_swap32(hdr.magic);
		hdr.version = cs_swap32(hdr.version);
		hdr.endian = cs_swap32(hdr.endian);
		hdr.arch = cs_swap32(hdr.arch);
		hdr.section_count = cs_swap32(hdr.section_count);
		hdr.flags = cs_swap32(hdr.flags);
		hdr.timestamp = cs_swap64(hdr.timestamp);
	}

	fwrite(&hdr, sizeof(struct cs_module_header), 1, writer->fp);

	fclose(writer->fp);
	writer->fp = NULL;

	return 0;
}

void cs_module_writer_destroy(struct cs_module_writer *writer) {
	if (!writer)
		return;

	if (writer->fp)
		fclose(writer->fp);

	free(writer->sections);

	for (int i = 0; i < writer->section_count; i++) {
		free(writer->section_data[i]);
	}
	free(writer->section_data);
	free(writer->section_sizes);
	free(writer->section_capacities);

	free(writer);
}

/* ========== Module Reader Implementation ========== */

struct cs_module_reader *cs_module_reader_open(const char *filename) {
	struct cs_module_reader *reader;

	reader = calloc(1, sizeof(struct cs_module_reader));
	if (!reader)
		return NULL;

	reader->fp = fopen(filename, "rb");
	if (!reader->fp) {
		free(reader);
		return NULL;
	}

	/* Read header */
	fread(&reader->header, sizeof(struct cs_module_header), 1, reader->fp);

	/* Detect endianness */
	enum cs_endian host_endian = cs_detect_endian();

	/* Check magic number (try both endiannesses) */
	if (reader->header.magic == CS_MODULE_MAGIC) {
		reader->file_endian = host_endian;
		reader->needs_swap = 0;
	} else if (cs_swap32(reader->header.magic) == CS_MODULE_MAGIC) {
		reader->file_endian = (host_endian == CS_ENDIAN_LITTLE) ?
		                      CS_ENDIAN_BIG : CS_ENDIAN_LITTLE;
		reader->needs_swap = 1;

		/* Swap header fields */
		reader->header.magic = cs_swap32(reader->header.magic);
		reader->header.version = cs_swap32(reader->header.version);
		reader->header.endian = cs_swap32(reader->header.endian);
		reader->header.arch = cs_swap32(reader->header.arch);
		reader->header.section_count = cs_swap32(reader->header.section_count);
		reader->header.flags = cs_swap32(reader->header.flags);
		reader->header.timestamp = cs_swap64(reader->header.timestamp);
	} else {
		fclose(reader->fp);
		free(reader);
		return NULL;  /* Invalid file format */
	}

	reader->file_arch = reader->header.arch;
	reader->section_count = reader->header.section_count;

	/* Allocate section arrays */
	reader->sections = calloc(reader->section_count,
	                          sizeof(struct cs_section_header));
	reader->section_data = calloc(reader->section_count, sizeof(void *));

	/* Read section table (at end of file after all section data) */
	fseek(reader->fp, 0, SEEK_END);
	long file_size = ftell(reader->fp);
	long section_table_offset = file_size -
	                            (reader->section_count *
	                             sizeof(struct cs_section_header));
	fseek(reader->fp, section_table_offset, SEEK_SET);

	for (int i = 0; i < reader->section_count; i++) {
		fread(&reader->sections[i], sizeof(struct cs_section_header),
		      1, reader->fp);

		if (reader->needs_swap) {
			reader->sections[i].type =
			    cs_swap32(reader->sections[i].type);
			reader->sections[i].flags =
			    cs_swap32(reader->sections[i].flags);
			reader->sections[i].offset =
			    cs_swap64(reader->sections[i].offset);
			reader->sections[i].size =
			    cs_swap64(reader->sections[i].size);
			reader->sections[i].entry_count =
			    cs_swap32(reader->sections[i].entry_count);
			reader->sections[i].reserved =
			    cs_swap32(reader->sections[i].reserved);
		}
	}

	return reader;
}

const void *cs_module_reader_get_section(struct cs_module_reader *reader,
                                          enum cs_section_type type,
                                          size_t *size) {
	if (!reader)
		return NULL;

	/* Find section by type */
	for (int i = 0; i < reader->section_count; i++) {
		if (reader->sections[i].type == type) {
			/* Return cached data if available */
			if (reader->section_data[i]) {
				if (size)
					*size = reader->sections[i].size;
				return reader->section_data[i];
			}

			/* Load section data */
			fseek(reader->fp, reader->sections[i].offset, SEEK_SET);
			void *data = malloc(reader->sections[i].size);
			fread(data, 1, reader->sections[i].size, reader->fp);

			reader->section_data[i] = data;

			if (size)
				*size = reader->sections[i].size;
			return data;
		}
	}

	return NULL;
}

int cs_module_reader_verify(struct cs_module_reader *reader) {
	if (!reader)
		return 0;

	/* Verify magic number */
	if (reader->header.magic != CS_MODULE_MAGIC)
		return 0;

	/* Verify version */
	if (reader->header.version != CS_MODULE_VERSION)
		return 0;

	/* Additional verification can be added here */

	return 1;
}

void cs_module_reader_destroy(struct cs_module_reader *reader) {
	if (!reader)
		return;

	if (reader->fp)
		fclose(reader->fp);

	free(reader->sections);

	for (int i = 0; i < reader->section_count; i++) {
		free(reader->section_data[i]);
	}
	free(reader->section_data);

	free(reader);
}

/* ========== String Pool Implementation ========== */

struct cs_string_pool {
	char *buffer;
	size_t size;
	size_t capacity;
};

cs_string_pool_t *cs_string_pool_create(void) {
	cs_string_pool_t *pool = calloc(1, sizeof(cs_string_pool_t));
	pool->capacity = 4096;
	pool->buffer = malloc(pool->capacity);
	pool->size = 0;
	return pool;
}

uint32_t cs_string_pool_add(cs_string_pool_t *pool, const char *str) {
	size_t len = strlen(str) + 1;

	/* Expand if needed */
	while (pool->size + len > pool->capacity) {
		pool->capacity *= 2;
		pool->buffer = realloc(pool->buffer, pool->capacity);
	}

	uint32_t offset = pool->size;
	memcpy(pool->buffer + offset, str, len);
	pool->size += len;

	return offset;
}

const char *cs_string_pool_get(cs_string_pool_t *pool, uint32_t offset) {
	if (offset >= pool->size)
		return NULL;
	return pool->buffer + offset;
}

void cs_string_pool_destroy(cs_string_pool_t *pool) {
	if (pool) {
		free(pool->buffer);
		free(pool);
	}
}

/* ========== Blob Pool Implementation ========== */

struct cs_blob_pool {
	uint8_t *buffer;
	size_t size;
	size_t capacity;
};

cs_blob_pool_t *cs_blob_pool_create(void) {
	cs_blob_pool_t *pool = calloc(1, sizeof(cs_blob_pool_t));
	pool->capacity = 8192;
	pool->buffer = malloc(pool->capacity);
	pool->size = 0;
	return pool;
}

uint32_t cs_blob_pool_add(cs_blob_pool_t *pool, const void *data, size_t size) {
	/* Expand if needed */
	while (pool->size + size + 4 > pool->capacity) {
		pool->capacity *= 2;
		pool->buffer = realloc(pool->buffer, pool->capacity);
	}

	uint32_t offset = pool->size;

	/* Store size prefix */
	uint32_t size32 = (uint32_t)size;
	memcpy(pool->buffer + pool->size, &size32, 4);
	pool->size += 4;

	/* Store data */
	memcpy(pool->buffer + pool->size, data, size);
	pool->size += size;

	return offset;
}

const void *cs_blob_pool_get(cs_blob_pool_t *pool, uint32_t offset,
                              size_t *size) {
	if (offset >= pool->size)
		return NULL;

	/* Read size prefix */
	uint32_t size32;
	memcpy(&size32, pool->buffer + offset, 4);

	if (size)
		*size = size32;

	return pool->buffer + offset + 4;
}

void cs_blob_pool_destroy(cs_blob_pool_t *pool) {
	if (pool) {
		free(pool->buffer);
		free(pool);
	}
}

/* ========== Utility Functions ========== */

const char *cs_arch_to_string(enum cs_arch arch) {
	switch (arch) {
	case CS_ARCH_NEUTRAL: return "Neutral";
	case CS_ARCH_X86: return "x86";
	case CS_ARCH_X86_64: return "x86-64";
	case CS_ARCH_ARM: return "ARM";
	case CS_ARCH_ARM64: return "ARM64";
	case CS_ARCH_MIPS: return "MIPS";
	case CS_ARCH_PPC: return "PowerPC";
	case CS_ARCH_RISCV: return "RISC-V";
	case CS_ARCH_WASM: return "WebAssembly";
	default: return "Unknown";
	}
}

const char *cs_endian_to_string(enum cs_endian endian) {
	switch (endian) {
	case CS_ENDIAN_LITTLE: return "Little-Endian";
	case CS_ENDIAN_BIG: return "Big-Endian";
	default: return "Unknown";
	}
}

void cs_module_print_info(struct cs_module_reader *reader) {
	if (!reader)
		return;

	printf("C# Module Information:\n");
	printf("  Version: %u\n", reader->header.version);
	printf("  Endianness: %s\n", cs_endian_to_string(reader->file_endian));
	printf("  Architecture: %s\n", cs_arch_to_string(reader->file_arch));
	printf("  Sections: %u\n", reader->section_count);
	printf("  Timestamp: %lu\n", (unsigned long)reader->header.timestamp);
}

int cs_module_is_compatible(struct cs_module_reader *reader) {
	if (!reader)
		return 0;

	/* Check if module can run on current architecture */
	if (reader->file_arch == CS_ARCH_NEUTRAL)
		return 1;  /* Architecture-neutral modules work everywhere */

	/* Add architecture-specific compatibility checks here */

	return 1;  /* For now, assume compatible */
}
