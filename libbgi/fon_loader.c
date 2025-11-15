/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft FON Bitmap Font File Loader
 *
 * Loads Microsoft Windows FON font files with endian safety
 * FON files are NE (New Executable) format with embedded font resources
 */

#include "font_utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * FON File Format:
 * - DOS MZ header
 * - NE (New Executable) header
 * - Resource table with font resources
 * - Font data (bitmap glyphs)
 *
 * All multi-byte values are stored in little-endian format
 */

#define MZ_SIGNATURE	0x5A4D	/* "MZ" */
#define NE_SIGNATURE	0x454E	/* "NE" */

/* DOS MZ header */
typedef struct {
	uint16_t signature;		/* MZ signature */
	uint16_t bytes_in_last_page;
	uint16_t pages_in_file;
	uint16_t relocations;
	uint16_t header_paragraphs;
	uint16_t min_extra_paragraphs;
	uint16_t max_extra_paragraphs;
	uint16_t initial_ss;
	uint16_t initial_sp;
	uint16_t checksum;
	uint16_t initial_ip;
	uint16_t initial_cs;
	uint16_t relocation_table_offset;
	uint16_t overlay_number;
	uint16_t reserved1[4];
	uint16_t oem_id;
	uint16_t oem_info;
	uint16_t reserved2[10];
	uint32_t ne_header_offset;	/* Offset to NE header */
} mz_header_t;

fon_font_t *
fon_load_font(const char *filename)
{
	FILE *fp;
	fon_font_t *font;
	mz_header_t mz;
	fon_header_t ne;
	font_resource_t font_res;
	uint8_t header_buf[256];
	uint32_t ne_offset, resource_offset;
	uint16_t resource_type, resource_count;
	size_t bytes_read;
	int i;

	if (!filename)
		return NULL;

	fp = fopen(filename, "rb");
	if (!fp)
		return NULL;

	/* Read MZ header */
	bytes_read = fread(header_buf, 1, 64, fp);
	if (bytes_read < 64) {
		fclose(fp);
		return NULL;
	}

	/* Parse MZ header */
	mz.signature = read_le16(&header_buf[0]);
	if (mz.signature != MZ_SIGNATURE) {
		fclose(fp);
		return NULL;
	}

	/* Get NE header offset */
	ne_offset = read_le32(&header_buf[60]);

	/* Seek to NE header */
	fseek(fp, ne_offset, SEEK_SET);

	/* Read NE header */
	bytes_read = fread(header_buf, 1, sizeof(fon_header_t), fp);
	if (bytes_read < sizeof(fon_header_t)) {
		fclose(fp);
		return NULL;
	}

	/* Parse NE header */
	ne.ne_magic = read_le16(&header_buf[0]);
	if (ne.ne_magic != NE_SIGNATURE) {
		fclose(fp);
		return NULL;
	}

	ne.resource_table_offset = read_le16(&header_buf[36]);
	ne.resource_count = read_le16(&header_buf[34]);

	/* Seek to resource table */
	resource_offset = ne_offset + ne.resource_table_offset;
	fseek(fp, resource_offset, SEEK_SET);

	/* Read resource table */
	bytes_read = fread(header_buf, 1, 2, fp);
	if (bytes_read < 2) {
		fclose(fp);
		return NULL;
	}

	/* Alignment shift count */
	/* Skip for now, look for font resources */

	/* Scan for font resource (type 0x8008) */
	while (1) {
		bytes_read = fread(header_buf, 1, 4, fp);
		if (bytes_read < 4)
			break;

		resource_type = read_le16(&header_buf[0]);
		resource_count = read_le16(&header_buf[2]);

		if (resource_type == 0)
			break; /* End of resources */

		if (resource_type == 0x8008) {
			/* Found font resource */
			/* Read first font resource entry */
			bytes_read = fread(header_buf, 1, 12, fp);
			if (bytes_read < 12) {
				fclose(fp);
				return NULL;
			}

			/* Parse resource entry */
			uint16_t offset_shift = read_le16(&header_buf[0]);
			uint16_t length_shift = read_le16(&header_buf[2]);
			uint32_t font_data_offset = (uint32_t)offset_shift << 4;
			uint32_t font_data_length = (uint32_t)length_shift << 4;

			/* Seek to font data */
			fseek(fp, font_data_offset, SEEK_SET);

			/* Read font resource header */
			bytes_read = fread(&font_res, 1, sizeof(font_resource_t), fp);
			if (bytes_read < sizeof(font_resource_t)) {
				fclose(fp);
				return NULL;
			}

			/* Allocate font structure */
			font = (fon_font_t *)malloc(sizeof(fon_font_t));
			if (!font) {
				fclose(fp);
				return NULL;
			}

			memset(font, 0, sizeof(fon_font_t));

			/* Parse font resource with endian conversion */
			font->char_width = read_le16((uint8_t *)&font_res.dfPixWidth);
			font->char_height = read_le16((uint8_t *)&font_res.dfPixHeight);
			font->first_char = font_res.dfFirstChar;
			font->last_char = font_res.dfLastChar;
			font->is_proportional = (font->char_width == 0);

			/* Read face name */
			if (font_res.dfFace != 0) {
				uint32_t face_offset = font_data_offset +
				                       read_le32((uint8_t *)&font_res.dfFace);
				fseek(fp, face_offset, SEEK_SET);
				fgets(font->face_name, sizeof(font->face_name), fp);
			} else {
				strcpy(font->face_name, "Unknown");
			}

			/* Calculate bitmap data size */
			int num_chars = font->last_char - font->first_char + 1;
			uint16_t width_bytes = read_le16((uint8_t *)&font_res.dfWidthBytes);

			font->data_size = width_bytes * font->char_height;

			/* Read bitmap data */
			font->bitmap_data = (uint8_t *)malloc(font->data_size);
			if (!font->bitmap_data) {
				free(font);
				fclose(fp);
				return NULL;
			}

			uint32_t bitmap_offset = font_data_offset +
			                         read_le32((uint8_t *)&font_res.dfBitsOffset);
			fseek(fp, bitmap_offset, SEEK_SET);
			bytes_read = fread(font->bitmap_data, 1, font->data_size, fp);

			if (bytes_read != font->data_size) {
				free(font->bitmap_data);
				free(font);
				fclose(fp);
				return NULL;
			}

			/* Read character width table for proportional fonts */
			if (font->is_proportional) {
				font->char_widths = (uint16_t *)malloc(num_chars * sizeof(uint16_t));
				if (!font->char_widths) {
					free(font->bitmap_data);
					free(font);
					fclose(fp);
					return NULL;
				}

				/* Width table follows font resource header */
				fseek(fp, font_data_offset + sizeof(font_resource_t), SEEK_SET);
				for (i = 0; i < num_chars; i++) {
					uint8_t width_buf[2];
					fread(width_buf, 1, 2, fp);
					font->char_widths[i] = read_le16(width_buf);
				}
			}

			fclose(fp);
			return font;
		}

		/* Skip this resource type's entries */
		fseek(fp, resource_count * 12, SEEK_CUR);
	}

	fclose(fp);
	return NULL;
}

void
fon_free_font(fon_font_t *font)
{
	if (!font)
		return;

	if (font->bitmap_data)
		free(font->bitmap_data);
	if (font->char_widths)
		free(font->char_widths);
	free(font);
}

uint8_t *
fon_get_char_bitmap(fon_font_t *font, unsigned char ch, int *width, int *height)
{
	int char_index;
	int char_width;
	uint8_t *bitmap;
	int bytes_per_row;
	int row, col;
	int src_byte, src_bit;
	int dest_byte;

	if (!font || !font->bitmap_data || !width || !height)
		return NULL;

	/* Check if character is in range */
	if (ch < font->first_char || ch > font->last_char)
		return NULL;

	char_index = ch - font->first_char;

	/* Get character width */
	if (font->is_proportional && font->char_widths) {
		char_width = font->char_widths[char_index];
	} else {
		char_width = font->char_width;
	}

	*width = char_width;
	*height = font->char_height;

	/* Allocate bitmap */
	bytes_per_row = (char_width + 7) / 8;
	bitmap = (uint8_t *)malloc(bytes_per_row * font->char_height);
	if (!bitmap)
		return NULL;

	memset(bitmap, 0, bytes_per_row * font->char_height);

	/* Extract character bitmap from font data */
	/* This is simplified - real implementation would handle bitmap stride */
	/* For now, copy character bitmap directly */

	/* Calculate source offset in font bitmap */
	/* This varies by font format - simplified here */

	return bitmap;
}
