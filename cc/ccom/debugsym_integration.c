/*	$Id$	*/

/*
 * Universal Debug Symbol Integration with PCC
 *
 * This file provides integration hooks for the universal debug symbol
 * system with the PCC compiler's existing infrastructure.
 */

#include "pass1.h"
#include "debugsym.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Integration state */
static struct {
	int enabled;		/* Debug symbol generation enabled */
	int initialized;	/* System initialized */
	debug_format_t format;	/* Selected format */
	int auto_record;	/* Automatically record symbols */
} integ_state;

/*
 * Initialize universal debug symbols based on configuration
 * Call this after command-line parsing
 */
void
debugsym_integration_init(const char *filename, int enable_debug)
{
	debug_format_t format = DBGFMT_NONE;

	if (!enable_debug) {
		integ_state.enabled = 0;
		return;
	}

	/* Determine format based on target platform */
#if defined(__ELF__) || defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
	/* ELF systems use DWARF */
	format = DBGFMT_DWARF3;  /* Default to DWARF 3 for compatibility */
#elif defined(_WIN32) || defined(__CYGWIN__)
	/* Windows uses CodeView */
	format = DBGFMT_CV8;
#elif defined(__APPLE__) && defined(__MACH__)
	/* macOS uses DWARF (via Mach-O) */
	format = DBGFMT_DWARF3;
#elif defined(__sun) || defined(__SVR4)
	/* Solaris can use either DWARF or STABS */
	format = DBGFMT_DWARF2;
#else
	/* Default to STABS for maximum compatibility */
	format = DBGFMT_STABS;
#endif

	/* Allow environment variable override */
	{
		char *env_format = getenv("PCC_DEBUG_FORMAT");
		if (env_format != NULL) {
			if (strcmp(env_format, "dwarf1") == 0)
				format = DBGFMT_DWARF1;
			else if (strcmp(env_format, "dwarf2") == 0)
				format = DBGFMT_DWARF2;
			else if (strcmp(env_format, "dwarf3") == 0)
				format = DBGFMT_DWARF3;
			else if (strcmp(env_format, "dwarf4") == 0)
				format = DBGFMT_DWARF4;
			else if (strcmp(env_format, "dwarf5") == 0)
				format = DBGFMT_DWARF5;
			else if (strcmp(env_format, "cv4") == 0)
				format = DBGFMT_CV4;
			else if (strcmp(env_format, "cv5") == 0)
				format = DBGFMT_CV5;
			else if (strcmp(env_format, "cv8") == 0)
				format = DBGFMT_CV8;
			else if (strcmp(env_format, "coff") == 0)
				format = DBGFMT_COFF;
			else if (strcmp(env_format, "stabs") == 0)
				format = DBGFMT_STABS;
			else if (strcmp(env_format, "dbx") == 0)
				format = DBGFMT_DBX;
		}
	}

	integ_state.enabled = 1;
	integ_state.format = format;
	integ_state.auto_record = 1;

	/* Initialize the debug symbol system */
	debugsym_init(format);
	debugsym_type_init();
	debugsym_file_begin((char *)filename);

	integ_state.initialized = 1;

	if (getenv("PCC_DEBUG_VERBOSE"))
		fprintf(stderr, "Universal debug symbols: %s format\n",
		    debugsym_format_name(format));
}

/*
 * Finish debug symbol generation
 * Call this before program exit
 */
void
debugsym_integration_finish(void)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_finish();

	if (getenv("PCC_DEBUG_STATS")) {
		debugsym_print_statistics();
		debugsym_type_stats();
	}

	integ_state.initialized = 0;
}

/*
 * Record current line number
 * Call this from the parser for each source line
 */
void
debugsym_integration_line(int line)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_line(line);
}

/*
 * Record a new variable symbol
 * Call this when a variable is defined
 */
void
debugsym_integration_variable(struct symtab *sp)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	if (!integ_state.auto_record)
		return;

	/* Filter out compiler-generated symbols */
	if (sp->sflags & STEMP)
		return;

	/* Filter out system header symbols if desired */
	if (sp->sflags & SINSYS)
		return;

	debugsym_record_variable(sp);
}

/*
 * Record a new function
 * Call this when entering a function definition
 */
void
debugsym_integration_function_begin(struct symtab *sp)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_enter_function(sp);
}

/*
 * Exit a function
 * Call this when leaving a function definition
 */
void
debugsym_integration_function_end(void)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_exit_function();
}

/*
 * Record a function parameter
 * Call this when processing function parameters
 */
void
debugsym_integration_parameter(struct symtab *sp)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	if (!integ_state.auto_record)
		return;

	debugsym_record_parameter(sp);
}

/*
 * Record a type definition
 * Call this when processing typedef, struct, union, enum
 */
void
debugsym_integration_type(struct symtab *sp)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	if (!integ_state.auto_record)
		return;

	debugsym_record_type(sp);
}

/*
 * Enter a new lexical block
 * Call this when entering { }
 */
void
debugsym_integration_block_begin(int level)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_enter_block(level);
}

/*
 * Exit a lexical block
 * Call this when exiting { }
 */
void
debugsym_integration_block_end(int level)
{
	if (!integ_state.enabled || !integ_state.initialized)
		return;

	debugsym_exit_block(level);
}

/*
 * Check if debug symbols are enabled
 */
int
debugsym_integration_enabled(void)
{
	return integ_state.enabled && integ_state.initialized;
}

/*
 * Get current debug format
 */
debug_format_t
debugsym_integration_get_format(void)
{
	return integ_state.format;
}

/*
 * Enable/disable automatic symbol recording
 */
void
debugsym_integration_set_auto_record(int enable)
{
	integ_state.auto_record = enable;
}
