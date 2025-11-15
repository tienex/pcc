/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Unicode String Library
 *
 * Supports UTF-8, UTF-16, UTF-32 with full Unicode operations
 */

#ifndef _PCC_USTRING_H_
#define _PCC_USTRING_H_

#include <stddef.h>
#include <stdint.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Unicode Encodings
 */
typedef enum {
	USTR_UTF8 = 0,
	USTR_UTF16LE,
	USTR_UTF16BE,
	USTR_UTF32LE,
	USTR_UTF32BE
} ustr_encoding_t;

/*
 * Unicode String Types
 */

/* UTF-8 string */
typedef struct {
	char *data;
	size_t byte_length;
	size_t char_count;
	size_t capacity;
} ustr8_t;

/* UTF-16 string */
typedef struct {
	uint16_t *data;
	size_t unit_count;
	size_t char_count;
	size_t capacity;
} ustr16_t;

/* UTF-32 string */
typedef struct {
	uint32_t *data;
	size_t length;
	size_t capacity;
} ustr32_t;

/* Unicode codepoint */
typedef uint32_t uchar_t;

/*
 * Normalization Forms
 */
typedef enum {
	USTR_NFC = 0,   /* Canonical Composition */
	USTR_NFD,       /* Canonical Decomposition */
	USTR_NFKC,      /* Compatibility Composition */
	USTR_NFKD       /* Compatibility Decomposition */
} ustr_normalization_t;

/*
 * String Creation and Destruction
 */

/* Create from UTF-8 */
ustr8_t *ustr8_create(const char *utf8_str);
ustr16_t *ustr16_from_utf8(const char *utf8_str);
ustr32_t *ustr32_from_utf8(const char *utf8_str);

/* Create from UTF-16 */
ustr8_t *ustr8_from_utf16(const uint16_t *utf16_str, size_t len);
ustr16_t *ustr16_create(const uint16_t *utf16_str, size_t len);
ustr32_t *ustr32_from_utf16(const uint16_t *utf16_str, size_t len);

/* Create from UTF-32 */
ustr8_t *ustr8_from_utf32(const uint32_t *utf32_str, size_t len);
ustr16_t *ustr16_from_utf32(const uint32_t *utf32_str, size_t len);
ustr32_t *ustr32_create(const uint32_t *utf32_str, size_t len);

/* Free strings */
void ustr8_free(ustr8_t *str);
void ustr16_free(ustr16_t *str);
void ustr32_free(ustr32_t *str);

/*
 * Encoding Conversion
 */

char *ustr_to_utf8(const void *str, ustr_encoding_t encoding);
uint16_t *ustr_to_utf16(const void *str, ustr_encoding_t encoding, size_t *out_len);
uint32_t *ustr_to_utf32(const void *str, ustr_encoding_t encoding, size_t *out_len);

/* Validate encoding */
int ustr_validate_utf8(const char *str, size_t len);
int ustr_validate_utf16(const uint16_t *str, size_t len);

/*
 * String Operations
 */

/* Length (character count, not bytes) */
size_t ustr8_length(const ustr8_t *str);
size_t ustr16_length(const ustr16_t *str);
size_t ustr32_length(const ustr32_t *str);

/* Concatenation */
ustr8_t *ustr8_concat(const ustr8_t *s1, const ustr8_t *s2);
ustr16_t *ustr16_concat(const ustr16_t *s1, const ustr16_t *s2);
ustr32_t *ustr32_concat(const ustr32_t *s1, const ustr32_t *s2);

/* Substring */
ustr8_t *ustr8_substr(const ustr8_t *str, size_t start, size_t len);
ustr16_t *ustr16_substr(const ustr16_t *str, size_t start, size_t len);
ustr32_t *ustr32_substr(const ustr32_t *str, size_t start, size_t len);

/* Comparison */
int ustr8_compare(const ustr8_t *s1, const ustr8_t *s2);
int ustr16_compare(const ustr16_t *s1, const ustr16_t *s2);
int ustr32_compare(const ustr32_t *s1, const ustr32_t *s2);

/* Case-insensitive comparison */
int ustr8_compare_ci(const ustr8_t *s1, const ustr8_t *s2);
int ustr16_compare_ci(const ustr16_t *s1, const ustr16_t *s2);
int ustr32_compare_ci(const ustr32_t *s1, const ustr32_t *s2);

/* Find */
ssize_t ustr8_find(const ustr8_t *haystack, const ustr8_t *needle);
ssize_t ustr16_find(const ustr16_t *haystack, const ustr16_t *needle);
ssize_t ustr32_find(const ustr32_t *haystack, const ustr32_t *needle);

/*
 * Character Operations
 */

/* Get character at index */
uchar_t ustr8_char_at(const ustr8_t *str, size_t index);
uchar_t ustr16_char_at(const ustr16_t *str, size_t index);
uchar_t ustr32_char_at(const ustr32_t *str, size_t index);

/* Iterator */
typedef struct {
	const void *str;
	size_t pos;
	ustr_encoding_t encoding;
} ustr_iterator_t;

ustr_iterator_t ustr_begin(const void *str, ustr_encoding_t encoding);
int ustr_next(ustr_iterator_t *it, uchar_t *ch);

/*
 * Case Conversion
 */

ustr8_t *ustr8_to_upper(const ustr8_t *str);
ustr8_t *ustr8_to_lower(const ustr8_t *str);
ustr8_t *ustr8_to_title(const ustr8_t *str);

ustr16_t *ustr16_to_upper(const ustr16_t *str);
ustr16_t *ustr16_to_lower(const ustr16_t *str);

ustr32_t *ustr32_to_upper(const ustr32_t *str);
ustr32_t *ustr32_to_lower(const ustr32_t *str);

/*
 * Normalization
 */

ustr8_t *ustr8_normalize(const ustr8_t *str, ustr_normalization_t form);
ustr16_t *ustr16_normalize(const ustr16_t *str, ustr_normalization_t form);
ustr32_t *ustr32_normalize(const ustr32_t *str, ustr_normalization_t form);

/*
 * Unicode Character Properties
 */

int uchar_is_alpha(uchar_t ch);
int uchar_is_digit(uchar_t ch);
int uchar_is_space(uchar_t ch);
int uchar_is_upper(uchar_t ch);
int uchar_is_lower(uchar_t ch);

uchar_t uchar_to_upper(uchar_t ch);
uchar_t uchar_to_lower(uchar_t ch);

/* Get Unicode category */
int uchar_category(uchar_t ch);

/* Get combining class */
int uchar_combining_class(uchar_t ch);

/*
 * Grapheme Clusters
 */

/* Count grapheme clusters */
size_t ustr_grapheme_count(const void *str, ustr_encoding_t encoding);

/* Break into grapheme clusters */
size_t *ustr_grapheme_breaks(const void *str, ustr_encoding_t encoding, size_t *count);

/*
 * Regular Expressions (Unicode-aware)
 */

typedef struct ustr_regex ustr_regex_t;

ustr_regex_t *ustr_regex_compile(const char *pattern);
int ustr_regex_match(ustr_regex_t *regex, const ustr8_t *str);
void ustr_regex_free(ustr_regex_t *regex);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_USTRING_H_ */
