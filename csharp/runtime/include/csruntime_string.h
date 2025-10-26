/*
 * C# Runtime - String Support
 */

#ifndef _CSRUNTIME_STRING_H_
#define _CSRUNTIME_STRING_H_

#include "csruntime_types.h"

/* String creation */
CSString *CS_String_Create(const char *utf8);
CSString *CS_String_CreateFromUTF16(const uint16_t *utf16, int32_t length);
CSString *CS_String_CreateEmpty(void);

/* String operations */
CSString *CS_String_Concat(CSString *s1, CSString *s2);
CSString *CS_String_Substring(CSString *str, int32_t start, int32_t length);
int32_t CS_String_IndexOf(CSString *str, CSString *value);
int32_t CS_String_LastIndexOf(CSString *str, CSString *value);
CSString *CS_String_Replace(CSString *str, CSString *old, CSString *new);
CSString *CS_String_ToLower(CSString *str);
CSString *CS_String_ToUpper(CSString *str);
CSString *CS_String_Trim(CSString *str);

/* String comparison */
int32_t CS_String_Compare(CSString *s1, CSString *s2);
CSBool CS_String_Equals(CSString *s1, CSString *s2);
CSBool CS_String_StartsWith(CSString *str, CSString *prefix);
CSBool CS_String_EndsWith(CSString *str, CSString *suffix);

/* String properties */
int32_t CS_String_Length(CSString *str);
CSBool CS_String_IsNullOrEmpty(CSString *str);

/* String formatting */
CSString *CS_String_Format(CSString *format, ...);

/* String interpolation support */
CSString *CS_String_Interpolate(int count, ...);

/* Conversion */
const char *CS_String_ToUTF8(CSString *str);
int32_t CS_String_ToInt32(CSString *str);
int64_t CS_String_ToInt64(CSString *str);
double CS_String_ToDouble(CSString *str);

/* From values */
CSString *CS_String_FromInt32(int32_t value);
CSString *CS_String_FromInt64(int64_t value);
CSString *CS_String_FromDouble(double value);
CSString *CS_String_FromBool(CSBool value);

#endif /* _CSRUNTIME_STRING_H_ */
