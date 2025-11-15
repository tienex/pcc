/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Borland CHR Stroked Font File Loader
 *
 * Loads Borland CHR font files with endian safety
 */

#include "font_utils.h"
#include <stdio.h>
#include <stdlib.h>
<string.h>

/*
 * CHR File Format:
 * - Header (variable size)
 * - Font data (vector stroke data)
 * - Character definitions (stroke sequences)
 *
 * All multi-byte values are stored in little-endian format
 */

chr_font_t *
chr_load_font(const char *filename)
{
	FILE *fp;
	chr_font_t *font;
	chr_header_t header;
	uint8_t header_buf[80];
	size_t bytes_read;

	if (!filename)
		return NULL;

	fp = fopen(filename, "rb");
	if (!fp)
		return NULL;

	/* Read header */
	bytes_read = fread(header_buf, 1, sizeof(header_buf), fp);
	if (bytes_read < 8) {
		fclose(fp);
		return NULL;
	}

	/* Parse header with endian conversion */
	header.header_size = read_le16(&header_buf[0]);
	header.font_name[0] = header_buf[2];
	header.font_name[1] = header_buf[3];
	header.font_name[2] = header_buf[4];
	header.font_name[3] = header_buf[5];
	header.font_size = read_le16(&header_buf[6]);
	header.font_major_version = header_buf[8];
	header.font_minor_version = header_buf[9];

	/* Validate header */
	if (header.header_size < 10 || header.header_size > 1024) {
		fclose(fp);
		return NULL;
	}

	/* Allocate font structure */
	font = (chr_font_t *)malloc(sizeof(chr_font_t));
	if (!font) {
		fclose(fp);
		return NULL;
	}

	memset(font, 0, sizeof(chr_font_t));

	/* Copy font name */
	font->name = (char *)malloc(5);
	if (!font->name) {
		free(font);
		fclose(fp);
		return NULL;
	}
	memcpy(font->name, header.font_name, 4);
	font->name[4] = '\0';

	/* Seek past header */
	fseek(fp, header.header_size, SEEK_SET);

	/* Read font data */
	font->data_size = header.font_size;
	font->data = (uint8_t *)malloc(font->data_size);
	if (!font->data) {
		free(font->name);
		free(font);
		fclose(fp);
		return NULL;
	}

	bytes_read = fread(font->data, 1, font->data_size, fp);
	if (bytes_read != font->data_size) {
		free(font->data);
		free(font->name);
		free(font);
		fclose(fp);
		return NULL;
	}

	fclose(fp);

	/* Parse character table from font data */
	/* CHR files typically define characters 0-255 */
	font->first_char = 0;
	font->last_char = 255;
	font->num_chars = 256;

	return font;
}

void
chr_free_font(chr_font_t *font)
{
	if (!font)
		return;

	if (font->name)
		free(font->name);
	if (font->data)
		free(font->data);
	free(font);
}

uint8_t *
chr_get_char_strokes(chr_font_t *font, unsigned char ch, int *num_strokes)
{
	uint16_t *char_table;
	uint16_t char_offset;
	uint8_t *stroke_data;
	int stroke_count;
	int i;

	if (!font || !font->data || !num_strokes)
		return NULL;

	/* Character table is at start of font data */
	/* Format: array of 16-bit offsets (little-endian) */
	char_table = (uint16_t *)font->data;

	/* Get offset for this character (with endian conversion) */
	char_offset = read_le16((uint8_t *)&char_table[ch]);

	if (char_offset >= font->data_size)
		return NULL;

	/* Get stroke data */
	stroke_data = &font->data[char_offset];

	/* Count strokes (stroke sequence ends with 0x00 opcode) */
	stroke_count = 0;
	for (i = char_offset; i < font->data_size && font->data[i] != 0x00; i++) {
		stroke_count++;
	}

	*num_strokes = stroke_count;
	return stroke_data;
}
