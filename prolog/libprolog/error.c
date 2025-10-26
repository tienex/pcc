/*
 * Copyright (c) 2025 PCC Prolog Runtime Library
 *
 * Error handling and exceptions
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>
#include "runtime.h"

/* Report error and exit */
void prolog_error(prolog_engine_t *eng, const char *type, const char *msg) {
	fprintf(stderr, "Prolog error [%s]: %s\n", type, msg);

	if (eng->exception_handler) {
		/* Throw exception if handler is set */
		longjmp(*eng->exception_handler, 1);
	}

	/* Otherwise just exit */
	exit(1);
}

/* Throw Prolog exception */
void prolog_throw(prolog_engine_t *eng, word_t exception) {
	eng->exception_term = exception;

	if (eng->exception_handler) {
		longjmp(*eng->exception_handler, 1);
	}

	/* No handler - print and exit */
	fprintf(stderr, "Uncaught exception: ");
	print_term(eng, stderr, exception);
	fprintf(stderr, "\n");
	exit(1);
}

/* Catch exceptions */
int prolog_catch(prolog_engine_t *eng, word_t goal, word_t catcher,
                word_t recovery) {
	jmp_buf handler;
	jmp_buf *old_handler = eng->exception_handler;

	eng->exception_handler = &handler;

	if (setjmp(handler) == 0) {
		/* Try to execute goal */
		int result = prolog_call(eng, goal);
		eng->exception_handler = old_handler;
		return result;
	} else {
		/* Exception was thrown */
		eng->exception_handler = old_handler;

		/* Try to unify exception with catcher */
		if (unify(eng, eng->exception_term, catcher)) {
			/* Execute recovery */
			return prolog_call(eng, recovery);
		} else {
			/* Re-throw */
			prolog_throw(eng, eng->exception_term);
			return 0;
		}
	}
}
