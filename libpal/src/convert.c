/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Type conversion functions
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "../include/palrt.h"

PAL_Number pal_numval(PAL_String *str)
{
	double value;

	if (!str || !str->data)
		return 0.0;

	/* Skip leading whitespace */
	const char *p = str->data;
	while (isspace((unsigned char)*p))
		p++;

	/* Try to parse as number */
	if (sscanf(p, "%lf", &value) == 1)
		return value;

	return 0.0;
}

PAL_String *pal_strval(PAL_Number value)
{
	char buffer[64];

	/* Format with appropriate precision */
	if (value == (PAL_LongInt)value) {
		/* Integer value - no decimal places */
		snprintf(buffer, sizeof(buffer), "%.0f", value);
	} else {
		/* Floating point - use default precision */
		snprintf(buffer, sizeof(buffer), "%.10g", value);
	}

	return pal_string_new(buffer);
}

PAL_Logical pal_logical(PAL_Variant *var)
{
	if (!var)
		return PAL_FALSE;

	switch (var->vtype) {
	case PAL_VT_LOGICAL:
		return var->value.v_logical;
	case PAL_VT_SHORTINT:
		return var->value.v_shortint != 0;
	case PAL_VT_SMALLINT:
		return var->value.v_smallint != 0;
	case PAL_VT_LONGINT:
		return var->value.v_longint != 0;
	case PAL_VT_NUMBER:
		return var->value.v_number != 0.0;
	case PAL_VT_CURRENCY:
		return var->value.v_currency != 0.0;
	case PAL_VT_STRING:
		return var->value.v_string.length > 0;
	default:
		return PAL_FALSE;
	}
}

PAL_LongInt pal_toint(PAL_Number value)
{
	return (PAL_LongInt)value;
}

PAL_Number pal_tonum(PAL_LongInt value)
{
	return (PAL_Number)value;
}
