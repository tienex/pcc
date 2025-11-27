/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Team.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Objective-C Literal Code Generation
 */

#include "pass1.h"
#include "objc_literals.h"
#include <string.h>

/*
 * Create NSString literal from C string
 * @"Hello" → [NSString stringWithUTF8String:"Hello"]
 */
P1ND *
objc_string_literal(char *str)
{
	P1ND *receiver, *method, *arg, *call;

	/* Skip the @ prefix if present */
	if (str[0] == '@')
		str++;

	/* Create string argument node */
	arg = bdty(STRING, addstring(stradd(NULL, str)), styp());

	/* For now, return the string directly */
	/* TODO: Generate [NSString stringWithUTF8String:str] */
	return arg;
}

/*
 * Create NSNumber from integer literal
 * @42 → [NSNumber numberWithInt:42]
 */
P1ND *
objc_number_literal_int(P1ND *value)
{
	/* For now, return the integer value directly */
	/* TODO: Generate [NSNumber numberWithInt:value] */
	return value;
}

/*
 * Create NSNumber from floating point literal
 * @3.14 → [NSNumber numberWithDouble:3.14]
 */
P1ND *
objc_number_literal_float(P1ND *value)
{
	/* For now, return the float value directly */
	/* TODO: Generate [NSNumber numberWithDouble:value] */
	return value;
}

/*
 * Create NSNumber from boolean literal
 * @YES → [NSNumber numberWithBool:YES]
 * @NO → [NSNumber numberWithBool:NO]
 */
P1ND *
objc_number_literal_bool(int value)
{
	/* Create boolean constant */
	P1ND *boolval = bcon(value);

	/* For now, return the boolean value directly */
	/* TODO: Generate [NSNumber numberWithBool:value] */
	return boolval;
}

/*
 * Create NSNumber from character literal
 * @'c' → [NSNumber numberWithChar:'c']
 */
P1ND *
objc_number_literal_char(P1ND *value)
{
	/* For now, return the character value directly */
	/* TODO: Generate [NSNumber numberWithChar:value] */
	return value;
}

/*
 * Get appropriate NSNumber method for a given type
 */
const char *
objc_number_method_for_type(TWORD type)
{
	TWORD bt = BTYPE(type);

	switch (bt) {
	case BOOL:
		return "numberWithBool:";
	case CHAR:
		return "numberWithChar:";
	case UCHAR:
		return "numberWithUnsignedChar:";
	case SHORT:
		return "numberWithShort:";
	case USHORT:
		return "numberWithUnsignedShort:";
	case INT:
		return "numberWithInt:";
	case UNSIGNED:
		return "numberWithUnsignedInt:";
	case LONG:
		return "numberWithLong:";
	case ULONG:
		return "numberWithUnsignedLong:";
	case LONGLONG:
		return "numberWithLongLong:";
	case ULONGLONG:
		return "numberWithUnsignedLongLong:";
	case FLOAT:
		return "numberWithFloat:";
	case DOUBLE:
	case LDOUBLE:
		return "numberWithDouble:";
	default:
		return "numberWithInt:";  /* Default fallback */
	}
}

/*
 * Create boxed expression
 * @(expr) → [NSNumber numberWithXXX:expr]
 *
 * The method XXX is chosen based on the expression's type
 */
P1ND *
objc_boxed_expression(P1ND *expr)
{
	const char *method;

	if (expr == NULL)
		return bcon(0);

	/* Determine the appropriate NSNumber method based on expression type */
	method = objc_number_method_for_type(expr->n_type);

	/* For now, return the expression directly */
	/* TODO: Generate [NSNumber <method>:expr] */
	return expr;
}

/*
 * Create array literal
 * @[obj1, obj2, obj3] → [NSArray arrayWithObjects:obj1, obj2, obj3, nil]
 */
P1ND *
objc_array_literal(P1ND *elements)
{
	/* For now, return a placeholder */
	/* TODO: Generate [NSArray arrayWithObjects:...] */
	/*
	 * Implementation outline:
	 * 1. Create NSArray class reference
	 * 2. Build argument list from elements (comma-separated)
	 * 3. Append nil terminator
	 * 4. Generate message send [NSArray arrayWithObjects:args]
	 */
	return bcon(0);
}

/*
 * Create dictionary literal
 * @{key1: val1, key2: val2} →
 *     [NSDictionary dictionaryWithObjectsAndKeys:val1, key1, val2, key2, nil]
 */
P1ND *
objc_dictionary_literal(P1ND *elements)
{
	/* For now, return a placeholder */
	/* TODO: Generate [NSDictionary dictionaryWithObjectsAndKeys:...] */
	/*
	 * Implementation outline:
	 * 1. Create NSDictionary class reference
	 * 2. Build argument list: value1, key1, value2, key2, ...
	 * 3. Append nil terminator
	 * 4. Generate message send [NSDictionary dictionaryWithObjectsAndKeys:args]
	 */
	return bcon(0);
}

/*
 * Helper: Create a message send expression
 * [ClassName methodName:arg]
 */
P1ND *
objc_create_message_send(char *className, char *methodName, P1ND *args)
{
	P1ND *receiver, *message;

	/* For now, return a placeholder */
	/* TODO: Generate proper message send node */
	/*
	 * Implementation outline:
	 * 1. Create class name reference node
	 * 2. Create method selector node
	 * 3. Combine receiver, selector, and arguments
	 * 4. Return message send node
	 */
	return bcon(0);
}
