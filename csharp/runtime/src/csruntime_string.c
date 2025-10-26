/*
 * C# Runtime - String Implementation
 * C# strings are immutable UTF-16 encoded strings
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <wchar.h>
#include "../include/csruntime.h"

/* String structure (UTF-16 based) */
struct CSString {
	CSObject base;
	int32_t length;      /* Length in UTF-16 code units */
	uint16_t *data;      /* UTF-16 data */
	char *utf8_cache;    /* Cached UTF-8 conversion */
};

/* ========== String Creation ========== */

CSString *CS_String_Create(const char *utf8) {
	if (!utf8) return NULL;

	CSString *str = (CSString *)CS_AllocObject(sizeof(CSString), 1);
	if (!str) return NULL;

	/* Convert UTF-8 to UTF-16 */
	size_t utf8_len = strlen(utf8);
	int32_t utf16_len = 0;

	/* Simple conversion - assume ASCII for now (full UTF-8 would be more complex) */
	uint16_t *utf16_data = (uint16_t *)CS_Malloc((utf8_len + 1) * sizeof(uint16_t));
	if (!utf16_data) {
		CS_Release((CSObject *)str);
		return NULL;
	}

	for (size_t i = 0; i < utf8_len; i++) {
		utf16_data[i] = (uint16_t)(unsigned char)utf8[i];
	}
	utf16_data[utf8_len] = 0;
	utf16_len = (int32_t)utf8_len;

	str->length = utf16_len;
	str->data = utf16_data;
	str->utf8_cache = NULL;

	return str;
}

CSString *CS_String_CreateFromUTF16(const uint16_t *utf16, int32_t length) {
	if (!utf16 || length < 0) return NULL;

	CSString *str = (CSString *)CS_AllocObject(sizeof(CSString), 1);
	if (!str) return NULL;

	uint16_t *data = (uint16_t *)CS_Malloc((length + 1) * sizeof(uint16_t));
	if (!data) {
		CS_Release((CSObject *)str);
		return NULL;
	}

	memcpy(data, utf16, length * sizeof(uint16_t));
	data[length] = 0;

	str->length = length;
	str->data = data;
	str->utf8_cache = NULL;

	return str;
}

CSString *CS_String_CreateEmpty(void) {
	return CS_String_Create("");
}

/* ========== String Operations ========== */

CSString *CS_String_Concat(CSString *s1, CSString *s2) {
	if (!s1) return s2;
	if (!s2) return s1;

	int32_t new_length = s1->length + s2->length;
	uint16_t *new_data = (uint16_t *)CS_Malloc((new_length + 1) * sizeof(uint16_t));
	if (!new_data) return NULL;

	memcpy(new_data, s1->data, s1->length * sizeof(uint16_t));
	memcpy(new_data + s1->length, s2->data, s2->length * sizeof(uint16_t));
	new_data[new_length] = 0;

	CSString *result = CS_String_CreateFromUTF16(new_data, new_length);
	CS_Free(new_data);

	return result;
}

CSString *CS_String_Substring(CSString *str, int32_t start, int32_t length) {
	if (!str || start < 0 || length < 0 || start + length > str->length) {
		return NULL;
	}

	return CS_String_CreateFromUTF16(str->data + start, length);
}

int32_t CS_String_IndexOf(CSString *str, CSString *value) {
	if (!str || !value || value->length == 0 || value->length > str->length) {
		return -1;
	}

	for (int32_t i = 0; i <= str->length - value->length; i++) {
		int32_t match = 1;
		for (int32_t j = 0; j < value->length; j++) {
			if (str->data[i + j] != value->data[j]) {
				match = 0;
				break;
			}
		}
		if (match) return i;
	}

	return -1;
}

int32_t CS_String_LastIndexOf(CSString *str, CSString *value) {
	if (!str || !value || value->length == 0 || value->length > str->length) {
		return -1;
	}

	for (int32_t i = str->length - value->length; i >= 0; i--) {
		int32_t match = 1;
		for (int32_t j = 0; j < value->length; j++) {
			if (str->data[i + j] != value->data[j]) {
				match = 0;
				break;
			}
		}
		if (match) return i;
	}

	return -1;
}

CSString *CS_String_Replace(CSString *str, CSString *old, CSString *new) {
	if (!str || !old || !new || old->length == 0) {
		return str;
	}

	/* Count occurrences */
	int32_t count = 0;
	int32_t pos = 0;
	while (pos <= str->length - old->length) {
		int32_t match = 1;
		for (int32_t j = 0; j < old->length; j++) {
			if (str->data[pos + j] != old->data[j]) {
				match = 0;
				break;
			}
		}
		if (match) {
			count++;
			pos += old->length;
		} else {
			pos++;
		}
	}

	if (count == 0) return str;

	/* Build new string */
	int32_t new_length = str->length + count * (new->length - old->length);
	uint16_t *new_data = (uint16_t *)CS_Malloc((new_length + 1) * sizeof(uint16_t));
	if (!new_data) return NULL;

	int32_t src_pos = 0, dst_pos = 0;
	while (src_pos < str->length) {
		/* Check for match */
		if (src_pos <= str->length - old->length) {
			int32_t match = 1;
			for (int32_t j = 0; j < old->length; j++) {
				if (str->data[src_pos + j] != old->data[j]) {
					match = 0;
					break;
				}
			}
			if (match) {
				/* Copy replacement */
				memcpy(new_data + dst_pos, new->data, new->length * sizeof(uint16_t));
				src_pos += old->length;
				dst_pos += new->length;
				continue;
			}
		}
		/* Copy original character */
		new_data[dst_pos++] = str->data[src_pos++];
	}
	new_data[new_length] = 0;

	CSString *result = CS_String_CreateFromUTF16(new_data, new_length);
	CS_Free(new_data);

	return result;
}

CSString *CS_String_ToLower(CSString *str) {
	if (!str) return NULL;

	uint16_t *new_data = (uint16_t *)CS_Malloc((str->length + 1) * sizeof(uint16_t));
	if (!new_data) return NULL;

	for (int32_t i = 0; i < str->length; i++) {
		/* Simple ASCII lowercase (full Unicode would be more complex) */
		if (str->data[i] >= 'A' && str->data[i] <= 'Z') {
			new_data[i] = str->data[i] + ('a' - 'A');
		} else {
			new_data[i] = str->data[i];
		}
	}
	new_data[str->length] = 0;

	CSString *result = CS_String_CreateFromUTF16(new_data, str->length);
	CS_Free(new_data);

	return result;
}

CSString *CS_String_ToUpper(CSString *str) {
	if (!str) return NULL;

	uint16_t *new_data = (uint16_t *)CS_Malloc((str->length + 1) * sizeof(uint16_t));
	if (!new_data) return NULL;

	for (int32_t i = 0; i < str->length; i++) {
		/* Simple ASCII uppercase (full Unicode would be more complex) */
		if (str->data[i] >= 'a' && str->data[i] <= 'z') {
			new_data[i] = str->data[i] - ('a' - 'A');
		} else {
			new_data[i] = str->data[i];
		}
	}
	new_data[str->length] = 0;

	CSString *result = CS_String_CreateFromUTF16(new_data, str->length);
	CS_Free(new_data);

	return result;
}

CSString *CS_String_Trim(CSString *str) {
	if (!str) return NULL;

	int32_t start = 0, end = str->length - 1;

	/* Trim leading whitespace */
	while (start < str->length && str->data[start] <= ' ') {
		start++;
	}

	/* Trim trailing whitespace */
	while (end >= start && str->data[end] <= ' ') {
		end--;
	}

	int32_t new_length = end - start + 1;
	if (new_length <= 0) {
		return CS_String_CreateEmpty();
	}

	return CS_String_CreateFromUTF16(str->data + start, new_length);
}

/* ========== String Comparison ========== */

int32_t CS_String_Compare(CSString *s1, CSString *s2) {
	if (!s1 && !s2) return 0;
	if (!s1) return -1;
	if (!s2) return 1;

	int32_t min_len = s1->length < s2->length ? s1->length : s2->length;

	for (int32_t i = 0; i < min_len; i++) {
		if (s1->data[i] != s2->data[i]) {
			return (int32_t)s1->data[i] - (int32_t)s2->data[i];
		}
	}

	return s1->length - s2->length;
}

CSBool CS_String_Equals(CSString *s1, CSString *s2) {
	return CS_String_Compare(s1, s2) == 0;
}

CSBool CS_String_StartsWith(CSString *str, CSString *prefix) {
	if (!str || !prefix) return 0;
	if (prefix->length > str->length) return 0;

	for (int32_t i = 0; i < prefix->length; i++) {
		if (str->data[i] != prefix->data[i]) {
			return 0;
		}
	}

	return 1;
}

CSBool CS_String_EndsWith(CSString *str, CSString *suffix) {
	if (!str || !suffix) return 0;
	if (suffix->length > str->length) return 0;

	int32_t offset = str->length - suffix->length;
	for (int32_t i = 0; i < suffix->length; i++) {
		if (str->data[offset + i] != suffix->data[i]) {
			return 0;
		}
	}

	return 1;
}

/* ========== String Properties ========== */

int32_t CS_String_Length(CSString *str) {
	return str ? str->length : 0;
}

CSBool CS_String_IsNullOrEmpty(CSString *str) {
	return !str || str->length == 0;
}

/* ========== String Formatting ========== */

CSString *CS_String_Format(CSString *format, ...) {
	if (!format) return NULL;

	/* Convert format to UTF-8 for sprintf */
	const char *format_utf8 = CS_String_ToUTF8(format);
	if (!format_utf8) return NULL;

	/* Format the string */
	char buffer[4096];
	va_list args;
	va_start(args, format);
	vsnprintf(buffer, sizeof(buffer), format_utf8, args);
	va_end(args);

	return CS_String_Create(buffer);
}

CSString *CS_String_Interpolate(int count, ...) {
	/* String interpolation support - concatenate all parts */
	if (count <= 0) return CS_String_CreateEmpty();

	va_list args;
	va_start(args, count);

	CSString *result = CS_String_CreateEmpty();
	for (int i = 0; i < count; i++) {
		CSString *part = va_arg(args, CSString *);
		CSString *new_result = CS_String_Concat(result, part);
		if (i > 0) CS_Release((CSObject *)result);
		result = new_result;
	}

	va_end(args);
	return result;
}

/* ========== Conversion ========== */

const char *CS_String_ToUTF8(CSString *str) {
	if (!str) return NULL;

	/* Use cached version if available */
	if (str->utf8_cache) return str->utf8_cache;

	/* Convert UTF-16 to UTF-8 (simple ASCII conversion for now) */
	char *utf8 = (char *)CS_Malloc(str->length + 1);
	if (!utf8) return NULL;

	for (int32_t i = 0; i < str->length; i++) {
		utf8[i] = (char)(str->data[i] & 0xFF);
	}
	utf8[str->length] = '\0';

	str->utf8_cache = utf8;
	return utf8;
}

int32_t CS_String_ToInt32(CSString *str) {
	if (!str) return 0;

	const char *utf8 = CS_String_ToUTF8(str);
	return utf8 ? (int32_t)atoi(utf8) : 0;
}

int64_t CS_String_ToInt64(CSString *str) {
	if (!str) return 0;

	const char *utf8 = CS_String_ToUTF8(str);
	return utf8 ? (int64_t)atoll(utf8) : 0;
}

double CS_String_ToDouble(CSString *str) {
	if (!str) return 0.0;

	const char *utf8 = CS_String_ToUTF8(str);
	return utf8 ? atof(utf8) : 0.0;
}

/* ========== From Values ========== */

CSString *CS_String_FromInt32(int32_t value) {
	char buffer[32];
	snprintf(buffer, sizeof(buffer), "%d", value);
	return CS_String_Create(buffer);
}

CSString *CS_String_FromInt64(int64_t value) {
	char buffer[32];
	snprintf(buffer, sizeof(buffer), "%lld", (long long)value);
	return CS_String_Create(buffer);
}

CSString *CS_String_FromDouble(double value) {
	char buffer[64];
	snprintf(buffer, sizeof(buffer), "%g", value);
	return CS_String_Create(buffer);
}

CSString *CS_String_FromBool(CSBool value) {
	return CS_String_Create(value ? "True" : "False");
}
