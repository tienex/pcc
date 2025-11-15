/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Font Utilities - Endian-safe font file loading
 *
 * Supports:
 * - Borland CHR (stroked font) files
 * - Microsoft FON (bitmap font) files
 * - Automatic endian detection and conversion
 */

#ifndef FONT_UTILS_H
#define FONT_UTILS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Endian detection and conversion */
#define IS_BIG_ENDIAN()		(*(uint16_t *)"\0\xff" < 0x100)

static inline uint16_t swap_uint16(uint16_t val)
{
	return (val << 8) | (val >> 8);
}

static inline uint32_t swap_uint32(uint32_t val)
{
	return ((val << 24) & 0xFF000000) |
	       ((val <<  8) & 0x00FF0000) |
	       ((val >>  8) & 0x0000FF00) |
	       ((val >> 24) & 0x000000FF);
}

/* Read little-endian 16-bit value */
static inline uint16_t read_le16(const uint8_t *p)
{
	uint16_t val = p[0] | (p[1] << 8);
	return val;
}

/* Read little-endian 32-bit value */
static inline uint32_t read_le32(const uint8_t *p)
{
	uint32_t val = p[0] | (p[1] << 8) | (p[2] << 16) | (p[3] << 24);
	return val;
}

/* Read big-endian 16-bit value */
static inline uint16_t read_be16(const uint8_t *p)
{
	uint16_t val = (p[0] << 8) | p[1];
	return val;
}

/* Read big-endian 32-bit value */
static inline uint32_t read_be32(const uint8_t *p)
{
	uint32_t val = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
	return val;
}

/*
 * Borland CHR Font File Format
 */

/* CHR file header */
typedef struct chr_header {
	uint16_t header_size;		/* Header size in bytes */
	char font_name[4];		/* Font name ("Trip", "Litt", etc.) */
	uint16_t font_size;		/* Font data size */
	uint8_t font_major_version;	/* Major version */
	uint8_t font_minor_version;	/* Minor version */
} chr_header_t;

/* CHR font descriptor */
typedef struct chr_font {
	char *name;			/* Font name */
	uint8_t *data;			/* Font stroke data */
	uint32_t data_size;		/* Data size */
	uint16_t num_chars;		/* Number of characters */
	uint8_t first_char;		/* First character code */
	uint8_t last_char;		/* Last character code */
} chr_font_t;

/*
 * Microsoft FON Font File Format
 */

/* FON file header (NE executable) */
typedef struct fon_header {
	uint16_t ne_magic;		/* 0x454E "NE" */
	uint8_t linker_version;
	uint8_t linker_revision;
	uint16_t entry_table_offset;
	uint16_t entry_table_size;
	uint32_t file_crc;
	uint16_t program_flags;
	uint16_t application_flags;
	uint16_t auto_data_segment;
	uint16_t initial_heap_size;
	uint16_t initial_stack_size;
	uint32_t entry_point;
	uint32_t initial_stack_pointer;
	uint16_t segment_count;
	uint16_t module_ref_count;
	uint16_t nonresident_name_table_size;
	uint16_t segment_table_offset;
	uint16_t resource_table_offset;
	uint16_t resident_name_table_offset;
	uint16_t module_ref_table_offset;
	uint16_t imported_names_table_offset;
	uint32_t nonresident_name_table_offset;
	uint16_t movable_entry_count;
	uint16_t segment_alignment;
	uint16_t resource_count;
	uint8_t target_os;
	uint8_t additional_flags;
} fon_header_t;

/* Font resource header */
typedef struct font_resource {
	uint16_t dfVersion;		/* Font version */
	uint32_t dfSize;		/* Font file size */
	char dfCopyright[60];		/* Copyright string */
	uint16_t dfType;		/* Font type */
	uint16_t dfPoints;		/* Point size */
	uint16_t dfVertRes;		/* Vertical resolution */
	uint16_t dfHorizRes;		/* Horizontal resolution */
	uint16_t dfAscent;		/* Ascent */
	uint16_t dfInternalLeading;	/* Internal leading */
	uint16_t dfExternalLeading;	/* External leading */
	uint8_t dfItalic;		/* Italic flag */
	uint8_t dfUnderline;		/* Underline flag */
	uint8_t dfStrikeOut;		/* Strike-out flag */
	uint16_t dfWeight;		/* Weight (400=normal, 700=bold) */
	uint8_t dfCharSet;		/* Character set */
	uint16_t dfPixWidth;		/* Pixel width (0=proportional) */
	uint16_t dfPixHeight;		/* Pixel height */
	uint8_t dfPitchAndFamily;	/* Pitch and family */
	uint16_t dfAvgWidth;		/* Average character width */
	uint16_t dfMaxWidth;		/* Maximum character width */
	uint8_t dfFirstChar;		/* First character */
	uint8_t dfLastChar;		/* Last character */
	uint8_t dfDefaultChar;		/* Default character */
	uint8_t dfBreakChar;		/* Break character */
	uint16_t dfWidthBytes;		/* Bytes per row */
	uint32_t dfDevice;		/* Device name offset */
	uint32_t dfFace;		/* Font face name offset */
	uint32_t dfBitsPointer;		/* Pointer to bitmap bits */
	uint32_t dfBitsOffset;		/* Offset to bitmap bits */
} font_resource_t;

/* FON font descriptor */
typedef struct fon_font {
	char face_name[64];		/* Font face name */
	uint8_t *bitmap_data;		/* Bitmap data */
	uint16_t *char_widths;		/* Character widths */
	uint32_t data_size;		/* Data size */
	uint16_t char_width;		/* Character width (fixed) */
	uint16_t char_height;		/* Character height */
	uint8_t first_char;		/* First character code */
	uint8_t last_char;		/* Last character code */
	uint8_t is_proportional;	/* Proportional spacing flag */
} fon_font_t;

/*
 * Font loading functions
 */

/* Load Borland CHR font file */
chr_font_t *chr_load_font(const char *filename);
void chr_free_font(chr_font_t *font);

/* Load Microsoft FON font file */
fon_font_t *fon_load_font(const char *filename);
void fon_free_font(fon_font_t *font);

/* Get character bitmap from FON font */
uint8_t *fon_get_char_bitmap(fon_font_t *font, unsigned char ch,
                             int *width, int *height);

/* Get character strokes from CHR font */
uint8_t *chr_get_char_strokes(chr_font_t *font, unsigned char ch,
                              int *num_strokes);

#ifdef __cplusplus
}
#endif

#endif /* FONT_UTILS_H */
