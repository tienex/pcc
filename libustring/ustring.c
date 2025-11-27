/*
 * Copyright (c) 2025 PCC Project
 * Universal Unicode String Library
 * Supports UTF-8, UTF-16, UTF-32, UTF-7, UTF-9, UTF-18, EBCDIC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include "ustring.h"

/* EBCDIC to ASCII conversion table */
static const unsigned char ebcdic_to_ascii[256] = {
	0x00,0x01,0x02,0x03,0x9C,0x09,0x86,0x7F,0x97,0x8D,0x8E,0x0B,0x0C,0x0D,0x0E,0x0F,
	0x10,0x11,0x12,0x13,0x9D,0x85,0x08,0x87,0x18,0x19,0x92,0x8F,0x1C,0x1D,0x1E,0x1F,
	0x80,0x81,0x82,0x83,0x84,0x0A,0x17,0x1B,0x88,0x89,0x8A,0x8B,0x8C,0x05,0x06,0x07,
	0x90,0x91,0x16,0x93,0x94,0x95,0x96,0x04,0x98,0x99,0x9A,0x9B,0x14,0x15,0x9E,0x1A,
	0x20,0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0x5B,0x2E,0x3C,0x28,0x2B,0x21,
	0x26,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,0xB0,0xB1,0x5D,0x24,0x2A,0x29,0x3B,0x5E,
	0x2D,0x2F,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0x7C,0x2C,0x25,0x5F,0x3E,0x3F,
	0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,0xC0,0xC1,0xC2,0x60,0x3A,0x23,0x40,0x27,0x3D,0x22,
	0xC3,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,
	0xCA,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,0x70,0x71,0x72,0xCB,0xCC,0xCD,0xCE,0xCF,0xD0,
	0xD1,0x7E,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,
	0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,
	0x7B,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,
	0x7D,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,0x50,0x51,0x52,0xEE,0xEF,0xF0,0xF1,0xF2,0xF3,
	0x5C,0x9F,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,
	0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF
};

/* ASCII to EBCDIC conversion table */
static const unsigned char ascii_to_ebcdic[256] = {
	0x00,0x01,0x02,0x03,0x37,0x2D,0x2E,0x2F,0x16,0x05,0x25,0x0B,0x0C,0x0D,0x0E,0x0F,
	0x10,0x11,0x12,0x13,0x3C,0x3D,0x32,0x26,0x18,0x19,0x3F,0x27,0x1C,0x1D,0x1E,0x1F,
	0x40,0x4F,0x7F,0x7B,0x5B,0x6C,0x50,0x7D,0x4D,0x5D,0x5C,0x4E,0x6B,0x60,0x4B,0x61,
	0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0x7A,0x5E,0x4C,0x7E,0x6E,0x6F,
	0x7C,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,
	0xD7,0xD8,0xD9,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0x4A,0xE0,0x5A,0x5F,0x6D,
	0x79,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x91,0x92,0x93,0x94,0x95,0x96,
	0x97,0x98,0x99,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xC0,0x6A,0xD0,0xA1,0x07,
	0x20,0x21,0x22,0x23,0x24,0x15,0x06,0x17,0x28,0x29,0x2A,0x2B,0x2C,0x09,0x0A,0x1B,
	0x30,0x31,0x1A,0x33,0x34,0x35,0x36,0x08,0x38,0x39,0x3A,0x3B,0x04,0x14,0x3E,0xE1,
	0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x51,0x52,0x53,0x54,0x55,0x56,0x57,
	0x58,0x59,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x70,0x71,0x72,0x73,0x74,0x75,
	0x76,0x77,0x78,0x80,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,0x90,0x9A,0x9B,0x9C,0x9D,0x9E,
	0x9F,0xA0,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,
	0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,0xDA,0xDB,
	0xDC,0xDD,0xDE,0xDF,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF
};

/*
 * UTF-8 String Operations
 */
ustr8_t *ustr8_create(const char *utf8_str) {
	if (!utf8_str) {
		return NULL;
	}

	ustr8_t *str = (ustr8_t *)malloc(sizeof(ustr8_t));
	if (!str) {
		return NULL;
	}

	str->byte_length = strlen(utf8_str);
	str->capacity = str->byte_length + 1;
	str->data = (char *)malloc(str->capacity);

	if (!str->data) {
		free(str);
		return NULL;
	}

	memcpy(str->data, utf8_str, str->byte_length + 1);

	/* Count characters */
	str->char_count = 0;
	const unsigned char *p = (const unsigned char *)utf8_str;
	while (*p) {
		if ((*p & 0xC0) != 0x80) {
			str->char_count++;
		}
		p++;
	}

	return str;
}

void ustr8_free(ustr8_t *str) {
	if (str) {
		if (str->data) {
			free(str->data);
		}
		free(str);
	}
}

size_t ustr8_length(const ustr8_t *str) {
	return str ? str->char_count : 0;
}

/*
 * UTF-16 Operations
 */
ustr16_t *ustr16_create(const uint16_t *utf16_str, size_t len) {
	if (!utf16_str) {
		return NULL;
	}

	ustr16_t *str = (ustr16_t *)malloc(sizeof(ustr16_t));
	if (!str) {
		return NULL;
	}

	str->unit_count = len;
	str->capacity = len + 1;
	str->data = (uint16_t *)malloc(str->capacity * sizeof(uint16_t));

	if (!str->data) {
		free(str);
		return NULL;
	}

	memcpy(str->data, utf16_str, len * sizeof(uint16_t));
	str->data[len] = 0;

	/* Count characters (handle surrogates) */
	str->char_count = 0;
	for (size_t i = 0; i < len; i++) {
		if (utf16_str[i] < 0xD800 || utf16_str[i] > 0xDFFF) {
			str->char_count++;
		} else if (utf16_str[i] >= 0xD800 && utf16_str[i] <= 0xDBFF) {
			/* High surrogate - count as one character */
			str->char_count++;
			i++; /* Skip low surrogate */
		}
	}

	return str;
}

void ustr16_free(ustr16_t *str) {
	if (str) {
		if (str->data) {
			free(str->data);
		}
		free(str);
	}
}

/*
 * UTF-32 Operations
 */
ustr32_t *ustr32_create(const uint32_t *utf32_str, size_t len) {
	if (!utf32_str) {
		return NULL;
	}

	ustr32_t *str = (ustr32_t *)malloc(sizeof(ustr32_t));
	if (!str) {
		return NULL;
	}

	str->length = len;
	str->capacity = len + 1;
	str->data = (uint32_t *)malloc(str->capacity * sizeof(uint32_t));

	if (!str->data) {
		free(str);
		return NULL;
	}

	memcpy(str->data, utf32_str, len * sizeof(uint32_t));
	str->data[len] = 0;

	return str;
}

void ustr32_free(ustr32_t *str) {
	if (str) {
		if (str->data) {
			free(str->data);
		}
		free(str);
	}
}

/*
 * Encoding Conversion - UTF-8 to UTF-16
 */
ustr16_t *ustr16_from_utf8(const char *utf8_str) {
	if (!utf8_str) {
		return NULL;
	}

	size_t len = strlen(utf8_str);
	uint16_t *utf16 = (uint16_t *)malloc((len + 1) * sizeof(uint16_t) * 2);
	if (!utf16) {
		return NULL;
	}

	size_t out_pos = 0;
	const unsigned char *p = (const unsigned char *)utf8_str;

	while (*p) {
		uint32_t codepoint = 0;
		int bytes = 0;

		if (*p < 0x80) {
			codepoint = *p;
			bytes = 1;
		} else if ((*p & 0xE0) == 0xC0) {
			codepoint = (*p & 0x1F) << 6;
			codepoint |= (p[1] & 0x3F);
			bytes = 2;
		} else if ((*p & 0xF0) == 0xE0) {
			codepoint = (*p & 0x0F) << 12;
			codepoint |= (p[1] & 0x3F) << 6;
			codepoint |= (p[2] & 0x3F);
			bytes = 3;
		} else if ((*p & 0xF8) == 0xF0) {
			codepoint = (*p & 0x07) << 18;
			codepoint |= (p[1] & 0x3F) << 12;
			codepoint |= (p[2] & 0x3F) << 6;
			codepoint |= (p[3] & 0x3F);
			bytes = 4;
		} else {
			p++;
			continue;
		}

		if (codepoint < 0x10000) {
			utf16[out_pos++] = (uint16_t)codepoint;
		} else {
			/* Surrogate pair */
			codepoint -= 0x10000;
			utf16[out_pos++] = 0xD800 + (codepoint >> 10);
			utf16[out_pos++] = 0xDC00 + (codepoint & 0x3FF);
		}

		p += bytes;
	}

	utf16[out_pos] = 0;

	return ustr16_create(utf16, out_pos);
}

/*
 * UTF-8 to UTF-32
 */
ustr32_t *ustr32_from_utf8(const char *utf8_str) {
	if (!utf8_str) {
		return NULL;
	}

	size_t len = strlen(utf8_str);
	uint32_t *utf32 = (uint32_t *)malloc((len + 1) * sizeof(uint32_t));
	if (!utf32) {
		return NULL;
	}

	size_t out_pos = 0;
	const unsigned char *p = (const unsigned char *)utf8_str;

	while (*p) {
		uint32_t codepoint = 0;
		int bytes = 0;

		if (*p < 0x80) {
			codepoint = *p;
			bytes = 1;
		} else if ((*p & 0xE0) == 0xC0) {
			codepoint = (*p & 0x1F) << 6;
			codepoint |= (p[1] & 0x3F);
			bytes = 2;
		} else if ((*p & 0xF0) == 0xE0) {
			codepoint = (*p & 0x0F) << 12;
			codepoint |= (p[1] & 0x3F) << 6;
			codepoint |= (p[2] & 0x3F);
			bytes = 3;
		} else if ((*p & 0xF8) == 0xF0) {
			codepoint = (*p & 0x07) << 18;
			codepoint |= (p[1] & 0x3F) << 12;
			codepoint |= (p[2] & 0x3F) << 6;
			codepoint |= (p[3] & 0x3F);
			bytes = 4;
		} else {
			p++;
			continue;
		}

		utf32[out_pos++] = codepoint;
		p += bytes;
	}

	utf32[out_pos] = 0;

	return ustr32_create(utf32, out_pos);
}

/*
 * EBCDIC Conversion
 */
char *ustr_from_ebcdic(const unsigned char *ebcdic_str, size_t len) {
	if (!ebcdic_str) {
		return NULL;
	}

	char *ascii = (char *)malloc(len + 1);
	if (!ascii) {
		return NULL;
	}

	for (size_t i = 0; i < len; i++) {
		ascii[i] = ebcdic_to_ascii[ebcdic_str[i]];
	}
	ascii[len] = '\0';

	return ascii;
}

unsigned char *ustr_to_ebcdic(const char *ascii_str, size_t *out_len) {
	if (!ascii_str) {
		return NULL;
	}

	size_t len = strlen(ascii_str);
	unsigned char *ebcdic = (unsigned char *)malloc(len + 1);
	if (!ebcdic) {
		return NULL;
	}

	for (size_t i = 0; i < len; i++) {
		ebcdic[i] = ascii_to_ebcdic[(unsigned char)ascii_str[i]];
	}
	ebcdic[len] = '\0';

	if (out_len) {
		*out_len = len;
	}

	return ebcdic;
}

/*
 * Validation
 */
int ustr_validate_utf8(const char *str, size_t len) {
	const unsigned char *p = (const unsigned char *)str;
	const unsigned char *end = p + len;

	while (p < end) {
		if (*p < 0x80) {
			p++;
		} else if ((*p & 0xE0) == 0xC0) {
			if (p + 1 >= end || (p[1] & 0xC0) != 0x80) return 0;
			p += 2;
		} else if ((*p & 0xF0) == 0xE0) {
			if (p + 2 >= end || (p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80) return 0;
			p += 3;
		} else if ((*p & 0xF8) == 0xF0) {
			if (p + 3 >= end || (p[1] & 0xC0) != 0x80 || (p[2] & 0xC0) != 0x80 || (p[3] & 0xC0) != 0x80) return 0;
			p += 4;
		} else {
			return 0;
		}
	}

	return 1;
}

/*
 * Character Operations
 */
uchar_t ustr8_char_at(const ustr8_t *str, size_t index) {
	if (!str || index >= str->char_count) {
		return 0;
	}

	const unsigned char *p = (const unsigned char *)str->data;
	size_t char_idx = 0;

	while (*p && char_idx < index) {
		if ((*p & 0xC0) != 0x80) {
			char_idx++;
		}
		p++;
	}

	/* Decode UTF-8 character */
	uint32_t codepoint = 0;
	if (*p < 0x80) {
		codepoint = *p;
	} else if ((*p & 0xE0) == 0xC0) {
		codepoint = (*p & 0x1F) << 6;
		codepoint |= (p[1] & 0x3F);
	} else if ((*p & 0xF0) == 0xE0) {
		codepoint = (*p & 0x0F) << 12;
		codepoint |= (p[1] & 0x3F) << 6;
		codepoint |= (p[2] & 0x3F);
	} else if ((*p & 0xF8) == 0xF0) {
		codepoint = (*p & 0x07) << 18;
		codepoint |= (p[1] & 0x3F) << 12;
		codepoint |= (p[2] & 0x3F) << 6;
		codepoint |= (p[3] & 0x3F);
	}

	return codepoint;
}

/*
 * Case Conversion (simplified - full Unicode would use ICU)
 */
ustr8_t *ustr8_to_upper(const ustr8_t *str) {
	if (!str) {
		return NULL;
	}

	ustr8_t *result = ustr8_create(str->data);
	if (!result) {
		return NULL;
	}

	/* Simple ASCII uppercase */
	for (size_t i = 0; i < result->byte_length; i++) {
		if (result->data[i] >= 'a' && result->data[i] <= 'z') {
			result->data[i] -= 32;
		}
	}

	return result;
}

ustr8_t *ustr8_to_lower(const ustr8_t *str) {
	if (!str) {
		return NULL;
	}

	ustr8_t *result = ustr8_create(str->data);
	if (!result) {
		return NULL;
	}

	/* Simple ASCII lowercase */
	for (size_t i = 0; i < result->byte_length; i++) {
		if (result->data[i] >= 'A' && result->data[i] <= 'Z') {
			result->data[i] += 32;
		}
	}

	return result;
}

/*
 * Comparison
 */
int ustr8_compare(const ustr8_t *s1, const ustr8_t *s2) {
	if (!s1 || !s2) {
		return 0;
	}
	return strcmp(s1->data, s2->data);
}

/*
 * Concatenation
 */
ustr8_t *ustr8_concat(const ustr8_t *s1, const ustr8_t *s2) {
	if (!s1 || !s2) {
		return NULL;
	}

	size_t total_len = s1->byte_length + s2->byte_length;
	char *combined = (char *)malloc(total_len + 1);
	if (!combined) {
		return NULL;
	}

	memcpy(combined, s1->data, s1->byte_length);
	memcpy(combined + s1->byte_length, s2->data, s2->byte_length);
	combined[total_len] = '\0';

	ustr8_t *result = ustr8_create(combined);
	free(combined);

	return result;
}

/*
 * UTF-7 Support (RFC 2152)
 * Modified Base64 encoding for email-safe Unicode
 */

static const char utf7_base64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int utf7_is_direct(uint16_t ch) {
	/* Direct characters: printable ASCII except +, \, ~ and whitespace varies */
	if (ch >= 0x20 && ch <= 0x7E) {
		if (ch == '+' || ch == '\\' || ch == '~') return 0;
		return 1;
	}
	if (ch == '\t' || ch == '\r' || ch == '\n') return 1;
	return 0;
}

char *ustr_to_utf7(const uint32_t *utf32_str, size_t len) {
	if (!utf32_str) return NULL;

	/* Worst case: every char needs encoding, ~5 bytes per char */
	size_t buf_size = len * 6 + 1;
	char *result = (char *)malloc(buf_size);
	if (!result) return NULL;

	size_t out_pos = 0;
	size_t i = 0;

	while (i < len) {
		uint32_t ch = utf32_str[i];

		if (ch < 0x10000 && utf7_is_direct((uint16_t)ch)) {
			/* Direct character */
			result[out_pos++] = (char)ch;
			i++;
		} else {
			/* Start modified Base64 sequence */
			result[out_pos++] = '+';

			/* Encode run of non-direct characters */
			uint32_t bits = 0;
			int bit_count = 0;

			while (i < len && !(utf32_str[i] < 0x10000 && utf7_is_direct((uint16_t)utf32_str[i]))) {
				uint32_t codepoint = utf32_str[i++];

				/* Convert to UTF-16 */
				if (codepoint < 0x10000) {
					bits = (bits << 16) | codepoint;
					bit_count += 16;
				} else {
					/* Surrogate pair */
					codepoint -= 0x10000;
					uint16_t high = 0xD800 + (codepoint >> 10);
					uint16_t low = 0xDC00 + (codepoint & 0x3FF);
					bits = (bits << 16) | high;
					bit_count += 16;
					while (bit_count >= 6) {
						result[out_pos++] = utf7_base64[(bits >> (bit_count - 6)) & 0x3F];
						bit_count -= 6;
					}
					bits = (bits << 16) | low;
					bit_count += 16;
				}

				/* Output base64 chars */
				while (bit_count >= 6) {
					result[out_pos++] = utf7_base64[(bits >> (bit_count - 6)) & 0x3F];
					bit_count -= 6;
				}
			}

			/* Flush remaining bits */
			if (bit_count > 0) {
				result[out_pos++] = utf7_base64[(bits << (6 - bit_count)) & 0x3F];
			}

			result[out_pos++] = '-';
		}
	}

	result[out_pos] = '\0';
	return result;
}

uint32_t *ustr_from_utf7(const char *utf7_str, size_t *out_len) {
	if (!utf7_str) return NULL;

	size_t len = strlen(utf7_str);
	uint32_t *result = (uint32_t *)malloc((len + 1) * sizeof(uint32_t));
	if (!result) return NULL;

	size_t out_pos = 0;
	size_t i = 0;

	while (i < len) {
		if (utf7_str[i] == '+') {
			i++;
			if (i < len && utf7_str[i] == '-') {
				/* +- represents + */
				result[out_pos++] = '+';
				i++;
				continue;
			}

			/* Decode modified base64 */
			uint32_t bits = 0;
			int bit_count = 0;

			while (i < len && utf7_str[i] != '-') {
				char ch = utf7_str[i++];
				int val = 0;

				if (ch >= 'A' && ch <= 'Z') val = ch - 'A';
				else if (ch >= 'a' && ch <= 'z') val = ch - 'a' + 26;
				else if (ch >= '0' && ch <= '9') val = ch - '0' + 52;
				else if (ch == '+') val = 62;
				else if (ch == '/') val = 63;
				else continue;

				bits = (bits << 6) | val;
				bit_count += 6;

				if (bit_count >= 16) {
					uint16_t utf16_unit = (bits >> (bit_count - 16)) & 0xFFFF;
					bit_count -= 16;

					/* Check for surrogate pair */
					if (utf16_unit >= 0xD800 && utf16_unit <= 0xDBFF) {
						/* High surrogate - need low surrogate */
						uint16_t high = utf16_unit;
						/* Get next 16 bits */
						while (bit_count < 16 && i < len && utf7_str[i] != '-') {
							char ch2 = utf7_str[i++];
							int val2 = 0;
							if (ch2 >= 'A' && ch2 <= 'Z') val2 = ch2 - 'A';
							else if (ch2 >= 'a' && ch2 <= 'z') val2 = ch2 - 'a' + 26;
							else if (ch2 >= '0' && ch2 <= '9') val2 = ch2 - '0' + 52;
							else if (ch2 == '+') val2 = 62;
							else if (ch2 == '/') val2 = 63;
							else continue;
							bits = (bits << 6) | val2;
							bit_count += 6;
						}
						uint16_t low = (bits >> (bit_count - 16)) & 0xFFFF;
						bit_count -= 16;

						/* Combine surrogates */
						uint32_t codepoint = 0x10000 + (((high & 0x3FF) << 10) | (low & 0x3FF));
						result[out_pos++] = codepoint;
					} else {
						result[out_pos++] = utf16_unit;
					}
				}
			}

			if (i < len && utf7_str[i] == '-') i++;
		} else {
			result[out_pos++] = (unsigned char)utf7_str[i++];
		}
	}

	result[out_pos] = 0;
	if (out_len) *out_len = out_pos;
	return result;
}

/*
 * UTF-9 Support (Historical)
 * 9-bit encoding, uses values 0x00-0x1FF
 */

/* UTF-9 encodes using byte sequences with a continuation pattern */
char *ustr_to_utf9(const uint32_t *utf32_str, size_t len) {
	if (!utf32_str) return NULL;

	/* Worst case: ~6 bytes per character */
	size_t buf_size = len * 6 + 1;
	char *result = (char *)malloc(buf_size);
	if (!result) return NULL;

	size_t out_pos = 0;

	for (size_t i = 0; i < len; i++) {
		uint32_t ch = utf32_str[i];

		if (ch < 0x100) {
			result[out_pos++] = (char)ch;
		} else if (ch < 0x200) {
			result[out_pos++] = 0xFF;
			result[out_pos++] = (char)(ch & 0xFF);
		} else if (ch < 0x10000) {
			result[out_pos++] = 0xFF;
			result[out_pos++] = (char)((ch >> 8) | 0x80);
			result[out_pos++] = (char)(ch & 0xFF);
		} else {
			/* Extended range */
			result[out_pos++] = 0xFF;
			result[out_pos++] = (char)(((ch >> 16) & 0x1F) | 0xC0);
			result[out_pos++] = (char)((ch >> 8) & 0xFF);
			result[out_pos++] = (char)(ch & 0xFF);
		}
	}

	result[out_pos] = '\0';
	return result;
}

uint32_t *ustr_from_utf9(const unsigned char *utf9_str, size_t len, size_t *out_len) {
	if (!utf9_str) return NULL;

	uint32_t *result = (uint32_t *)malloc((len + 1) * sizeof(uint32_t));
	if (!result) return NULL;

	size_t out_pos = 0;
	size_t i = 0;

	while (i < len) {
		if (utf9_str[i] == 0xFF && i + 1 < len) {
			i++;
			unsigned char next = utf9_str[i++];

			if ((next & 0xC0) == 0xC0) {
				/* 4-byte sequence */
				if (i + 1 < len) {
					uint32_t ch = ((next & 0x1F) << 16) | (utf9_str[i] << 8) | utf9_str[i + 1];
					i += 2;
					result[out_pos++] = ch;
				}
			} else if ((next & 0x80) == 0x80) {
				/* 3-byte sequence */
				if (i < len) {
					uint32_t ch = ((next & 0x7F) << 8) | utf9_str[i++];
					result[out_pos++] = ch;
				}
			} else {
				/* 2-byte sequence */
				result[out_pos++] = 0x100 | next;
			}
		} else {
			result[out_pos++] = utf9_str[i++];
		}
	}

	result[out_pos] = 0;
	if (out_len) *out_len = out_pos;
	return result;
}

/*
 * UTF-18 Support (Historical)
 * 18-bit encoding for larger character sets
 */

char *ustr_to_utf18(const uint32_t *utf32_str, size_t len) {
	if (!utf32_str) return NULL;

	/* UTF-18 uses variable length: 1-4 bytes */
	size_t buf_size = len * 4 + 1;
	char *result = (char *)malloc(buf_size);
	if (!result) return NULL;

	size_t out_pos = 0;

	for (size_t i = 0; i < len; i++) {
		uint32_t ch = utf32_str[i];

		if (ch < 0x80) {
			/* 1 byte: 0xxxxxxx */
			result[out_pos++] = (char)ch;
		} else if (ch < 0x4000) {
			/* 2 bytes: 10xxxxxx xxxxxxxx */
			result[out_pos++] = (char)(0x80 | ((ch >> 8) & 0x3F));
			result[out_pos++] = (char)(ch & 0xFF);
		} else if (ch < 0x200000) {
			/* 3 bytes: 110xxxxx xxxxxxxx xxxxxxxx */
			result[out_pos++] = (char)(0xC0 | ((ch >> 16) & 0x1F));
			result[out_pos++] = (char)((ch >> 8) & 0xFF);
			result[out_pos++] = (char)(ch & 0xFF);
		} else {
			/* 4 bytes: 1110xxxx xxxxxxxx xxxxxxxx xxxxxxxx */
			result[out_pos++] = (char)(0xE0 | ((ch >> 24) & 0x0F));
			result[out_pos++] = (char)((ch >> 16) & 0xFF);
			result[out_pos++] = (char)((ch >> 8) & 0xFF);
			result[out_pos++] = (char)(ch & 0xFF);
		}
	}

	result[out_pos] = '\0';
	return result;
}

uint32_t *ustr_from_utf18(const unsigned char *utf18_str, size_t len, size_t *out_len) {
	if (!utf18_str) return NULL;

	uint32_t *result = (uint32_t *)malloc((len + 1) * sizeof(uint32_t));
	if (!result) return NULL;

	size_t out_pos = 0;
	size_t i = 0;

	while (i < len) {
		unsigned char byte1 = utf18_str[i++];

		if ((byte1 & 0x80) == 0) {
			/* 1-byte sequence */
			result[out_pos++] = byte1;
		} else if ((byte1 & 0xC0) == 0x80) {
			/* 2-byte sequence */
			if (i < len) {
				uint32_t ch = ((byte1 & 0x3F) << 8) | utf18_str[i++];
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xE0) == 0xC0) {
			/* 3-byte sequence */
			if (i + 1 < len) {
				uint32_t ch = ((byte1 & 0x1F) << 16) | (utf18_str[i] << 8) | utf18_str[i + 1];
				i += 2;
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xF0) == 0xE0) {
			/* 4-byte sequence */
			if (i + 2 < len) {
				uint32_t ch = ((byte1 & 0x0F) << 24) | (utf18_str[i] << 16) |
				              (utf18_str[i + 1] << 8) | utf18_str[i + 2];
				i += 3;
				result[out_pos++] = ch;
			}
		}
	}

	result[out_pos] = 0;
	if (out_len) *out_len = out_pos;
	return result;
}

/*
 * UTF-EBCDIC Support
 * UTF-8 variant for EBCDIC systems (IBM mainframes)
 * Uses different byte patterns than UTF-8
 */

/* UTF-EBCDIC uses different lead bytes than UTF-8 */
char *ustr_to_utf_ebcdic(const uint32_t *utf32_str, size_t len) {
	if (!utf32_str) return NULL;

	/* Worst case: ~6 bytes per character */
	size_t buf_size = len * 6 + 1;
	char *result = (char *)malloc(buf_size);
	if (!result) return NULL;

	size_t out_pos = 0;

	for (size_t i = 0; i < len; i++) {
		uint32_t ch = utf32_str[i];

		if (ch < 0x80) {
			/* Convert to EBCDIC */
			result[out_pos++] = ascii_to_ebcdic[ch];
		} else if (ch < 0x800) {
			/* 2-byte sequence: 0xC4-0xC5 for lead byte */
			result[out_pos++] = 0xC4 | ((ch >> 6) & 0x01);
			result[out_pos++] = 0x80 | (ch & 0x3F);
		} else if (ch < 0x10000) {
			/* 3-byte sequence: 0xC8-0xCF for lead byte */
			result[out_pos++] = 0xC8 | ((ch >> 12) & 0x07);
			result[out_pos++] = 0x80 | ((ch >> 6) & 0x3F);
			result[out_pos++] = 0x80 | (ch & 0x3F);
		} else if (ch < 0x200000) {
			/* 4-byte sequence: 0xD0-0xD7 for lead byte */
			result[out_pos++] = 0xD0 | ((ch >> 18) & 0x07);
			result[out_pos++] = 0x80 | ((ch >> 12) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 6) & 0x3F);
			result[out_pos++] = 0x80 | (ch & 0x3F);
		} else if (ch < 0x4000000) {
			/* 5-byte sequence: 0xD8-0xDB for lead byte */
			result[out_pos++] = 0xD8 | ((ch >> 24) & 0x03);
			result[out_pos++] = 0x80 | ((ch >> 18) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 12) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 6) & 0x3F);
			result[out_pos++] = 0x80 | (ch & 0x3F);
		} else {
			/* 6-byte sequence: 0xDC-0xDD for lead byte */
			result[out_pos++] = 0xDC | ((ch >> 30) & 0x01);
			result[out_pos++] = 0x80 | ((ch >> 24) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 18) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 12) & 0x3F);
			result[out_pos++] = 0x80 | ((ch >> 6) & 0x3F);
			result[out_pos++] = 0x80 | (ch & 0x3F);
		}
	}

	result[out_pos] = '\0';
	return result;
}

uint32_t *ustr_from_utf_ebcdic(const unsigned char *utf_ebcdic_str, size_t len, size_t *out_len) {
	if (!utf_ebcdic_str) return NULL;

	uint32_t *result = (uint32_t *)malloc((len + 1) * sizeof(uint32_t));
	if (!result) return NULL;

	size_t out_pos = 0;
	size_t i = 0;

	while (i < len) {
		unsigned char byte1 = utf_ebcdic_str[i++];

		if (byte1 < 0x80) {
			/* Single byte - convert from EBCDIC */
			result[out_pos++] = ebcdic_to_ascii[byte1];
		} else if ((byte1 & 0xFE) == 0xC4) {
			/* 2-byte sequence */
			if (i < len) {
				uint32_t ch = ((byte1 & 0x01) << 6) | (utf_ebcdic_str[i++] & 0x3F);
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xF8) == 0xC8) {
			/* 3-byte sequence */
			if (i + 1 < len) {
				uint32_t ch = ((byte1 & 0x07) << 12) |
				              ((utf_ebcdic_str[i] & 0x3F) << 6) |
				              (utf_ebcdic_str[i + 1] & 0x3F);
				i += 2;
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xF8) == 0xD0) {
			/* 4-byte sequence */
			if (i + 2 < len) {
				uint32_t ch = ((byte1 & 0x07) << 18) |
				              ((utf_ebcdic_str[i] & 0x3F) << 12) |
				              ((utf_ebcdic_str[i + 1] & 0x3F) << 6) |
				              (utf_ebcdic_str[i + 2] & 0x3F);
				i += 3;
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xFC) == 0xD8) {
			/* 5-byte sequence */
			if (i + 3 < len) {
				uint32_t ch = ((byte1 & 0x03) << 24) |
				              ((utf_ebcdic_str[i] & 0x3F) << 18) |
				              ((utf_ebcdic_str[i + 1] & 0x3F) << 12) |
				              ((utf_ebcdic_str[i + 2] & 0x3F) << 6) |
				              (utf_ebcdic_str[i + 3] & 0x3F);
				i += 4;
				result[out_pos++] = ch;
			}
		} else if ((byte1 & 0xFE) == 0xDC) {
			/* 6-byte sequence */
			if (i + 4 < len) {
				uint32_t ch = ((byte1 & 0x01) << 30) |
				              ((utf_ebcdic_str[i] & 0x3F) << 24) |
				              ((utf_ebcdic_str[i + 1] & 0x3F) << 18) |
				              ((utf_ebcdic_str[i + 2] & 0x3F) << 12) |
				              ((utf_ebcdic_str[i + 3] & 0x3F) << 6) |
				              (utf_ebcdic_str[i + 4] & 0x3F);
				i += 5;
				result[out_pos++] = ch;
			}
		} else {
			/* Invalid or continuation byte - convert from EBCDIC */
			result[out_pos++] = ebcdic_to_ascii[byte1];
		}
	}

	result[out_pos] = 0;
	if (out_len) *out_len = out_pos;
	return result;
}
