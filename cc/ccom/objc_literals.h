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
 * Objective-C Literal Support
 *
 * This module provides code generation for Objective-C literals:
 * - @"string" (NSString literals)
 * - @42, @3.14 (NSNumber literals)
 * - @YES, @NO (Boolean literals)
 * - @'c' (Character literals)
 * - @(expr) (Boxed expressions)
 * - @[...] (Array literals)
 * - @{...} (Dictionary literals)
 */

#ifndef OBJC_LITERALS_H
#define OBJC_LITERALS_H

#include "pass1.h"

/*
 * NSString Literal
 * @"Hello" → [NSString stringWithUTF8String:"Hello"]
 */
P1ND *objc_string_literal(char *str);

/*
 * NSNumber Literals
 * @42 → [NSNumber numberWithInt:42]
 * @3.14 → [NSNumber numberWithDouble:3.14]
 * @YES → [NSNumber numberWithBool:YES]
 * @NO → [NSNumber numberWithBool:NO]
 * @'c' → [NSNumber numberWithChar:'c']
 */
P1ND *objc_number_literal_int(P1ND *value);
P1ND *objc_number_literal_float(P1ND *value);
P1ND *objc_number_literal_bool(int value);
P1ND *objc_number_literal_char(P1ND *value);

/*
 * Boxed Expression
 * @(expr) → [NSNumber numberWithXXX:expr]
 *
 * Type is inferred from expression type:
 * - Integer types → numberWithInt/Long/etc
 * - Floating point → numberWithFloat/Double
 * - Boolean → numberWithBool
 */
P1ND *objc_boxed_expression(P1ND *expr);

/*
 * Array Literal
 * @[obj1, obj2, obj3] → [NSArray arrayWithObjects:obj1, obj2, obj3, nil]
 *
 * Returns an NSArray instance containing the specified objects
 */
P1ND *objc_array_literal(P1ND *elements);

/*
 * Dictionary Literal
 * @{key1: val1, key2: val2} → [NSDictionary dictionaryWithObjectsAndKeys:
 *                                  val1, key1, val2, key2, nil]
 *
 * Returns an NSDictionary instance with the specified key-value pairs
 */
P1ND *objc_dictionary_literal(P1ND *elements);

/*
 * Helper: Create a message send expression
 * Used internally to generate [ClassName methodName:arg]
 */
P1ND *objc_create_message_send(char *className, char *methodName, P1ND *args);

/*
 * Helper: Get appropriate NSNumber creation method for a type
 */
const char *objc_number_method_for_type(TWORD type);

#endif /* OBJC_LITERALS_H */
