/*	$Id$	*/
/*
 * Copyright (c) 2025. All rights reserved.
 *
 * Dart Runtime Library - Exception Handling
 */

#include "dart.h"
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

/* Exception handling context */
static jmp_buf *exception_context = NULL;
static DartException *current_exception = NULL;

static void
dart_exception_destructor(DartObject *obj)
{
	DartException *ex = (DartException *)obj;
	if (ex->message) {
		dart_object_release((DartObject *)ex->message);
	}
	if (ex->stack_trace) {
		dart_object_release((DartObject *)ex->stack_trace);
	}
}

DartException *
dart_exception_new(const char *message)
{
	DartException *ex = (DartException *)dart_object_new(DART_TYPE_OBJECT,
	                                                      sizeof(DartException));
	ex->message = dart_string_new(message);
	ex->stack_trace = dart_string_new(""); /* TODO: capture stack trace */
	ex->base.destructor = dart_exception_destructor;
	return ex;
}

void
dart_exception_throw(DartException *ex)
{
	if (current_exception) {
		dart_object_release((DartObject *)current_exception);
	}

	current_exception = ex;
	dart_object_retain((DartObject *)ex);

	if (exception_context) {
		longjmp(*exception_context, 1);
	} else {
		/* Unhandled exception */
		fprintf(stderr, "Unhandled exception: %s\n",
		        dart_string_cstr(ex->message));
		exit(1);
	}
}

DartException *
dart_exception_catch(void)
{
	DartException *ex = current_exception;
	current_exception = NULL;
	return ex;
}
