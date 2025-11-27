/*
 * COBOL Runtime Library - Public Header
 */

#ifndef _COBOL_H
#define _COBOL_H

#include "cobolrt.h"

/* Public API for COBOL programs */

/* Initialize COBOL runtime */
#define COB_INIT() __cobol_init()

/* Cleanup COBOL runtime */
#define COB_CLEANUP() __cobol_cleanup()

/* Define a COBOL field */
#define COB_FIELD(name, type, size) \
	static cobol_field_t name = { NULL, size, type, 0, 0, 0 }

/* File open modes */
#define COB_OPEN_INPUT   1
#define COB_OPEN_OUTPUT  2
#define COB_OPEN_IO      3
#define COB_OPEN_EXTEND  4

#endif /* _COBOL_H */
