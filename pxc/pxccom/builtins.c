/*
 * Copyright (c) 2025 PCC Xbase++ Compiler
 *
 * Built-in functions for Xbase++
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"

/* Built-in function descriptor */
typedef struct builtin {
	char *name;             /* Function name */
	int min_args;           /* Minimum arguments */
	int max_args;           /* Maximum arguments (-1 = unlimited) */
	TNODE *ret_type;        /* Return type */
} BUILTIN;

/* Built-in function table */
static BUILTIN builtins[] = {
	/* String functions */
	{ "LEN",        1, 1,  NULL },  /* Length of string */
	{ "SUBSTR",     2, 3,  NULL },  /* Substring */
	{ "LEFT",       2, 2,  NULL },  /* Left substring */
	{ "RIGHT",      2, 2,  NULL },  /* Right substring */
	{ "UPPER",      1, 1,  NULL },  /* Convert to uppercase */
	{ "LOWER",      1, 1,  NULL },  /* Convert to lowercase */
	{ "TRIM",       1, 1,  NULL },  /* Trim spaces */
	{ "LTRIM",      1, 1,  NULL },  /* Trim left spaces */
	{ "RTRIM",      1, 1,  NULL },  /* Trim right spaces */
	{ "ALLTRIM",    1, 1,  NULL },  /* Trim all spaces */
	{ "SPACE",      1, 1,  NULL },  /* Create string of spaces */
	{ "REPLICATE",  2, 2,  NULL },  /* Replicate string */
	{ "STUFF",      4, 4,  NULL },  /* Insert substring */
	{ "AT",         2, 3,  NULL },  /* Find substring */
	{ "RAT",        2, 3,  NULL },  /* Find substring from right */
	{ "STRTRAN",    2, 4,  NULL },  /* Replace substring */
	{ "CHR",        1, 1,  NULL },  /* Character from ASCII */
	{ "ASC",        1, 1,  NULL },  /* ASCII value */
	{ "ISALPHA",    1, 1,  NULL },  /* Is alphabetic */
	{ "ISDIGIT",    1, 1,  NULL },  /* Is digit */
	{ "ISLOWER",    1, 1,  NULL },  /* Is lowercase */
	{ "ISUPPER",    1, 1,  NULL },  /* Is uppercase */

	/* Numeric functions */
	{ "ABS",        1, 1,  NULL },  /* Absolute value */
	{ "INT",        1, 1,  NULL },  /* Integer part */
	{ "ROUND",      2, 2,  NULL },  /* Round */
	{ "SQRT",       1, 1,  NULL },  /* Square root */
	{ "EXP",        1, 1,  NULL },  /* Exponential */
	{ "LOG",        1, 1,  NULL },  /* Natural logarithm */
	{ "LOG10",      1, 1,  NULL },  /* Base-10 logarithm */
	{ "SIN",        1, 1,  NULL },  /* Sine */
	{ "COS",        1, 1,  NULL },  /* Cosine */
	{ "TAN",        1, 1,  NULL },  /* Tangent */
	{ "ASIN",       1, 1,  NULL },  /* Arc sine */
	{ "ACOS",       1, 1,  NULL },  /* Arc cosine */
	{ "ATAN",       1, 1,  NULL },  /* Arc tangent */
	{ "MIN",        2, -1, NULL },  /* Minimum value */
	{ "MAX",        2, -1, NULL },  /* Maximum value */
	{ "MOD",        2, 2,  NULL },  /* Modulo */
	{ "RAND",       0, 1,  NULL },  /* Random number */

	/* Type conversion */
	{ "STR",        1, 3,  NULL },  /* Number to string */
	{ "VAL",        1, 1,  NULL },  /* String to number */
	{ "CTOD",       1, 1,  NULL },  /* String to date */
	{ "DTOC",       1, 1,  NULL },  /* Date to string */
	{ "DTOS",       1, 1,  NULL },  /* Date to string (YYYYMMDD) */
	{ "STOD",       1, 1,  NULL },  /* String to date (YYYYMMDD) */
	{ "TRANSFORM",  2, 2,  NULL },  /* Format value */

	/* Date functions */
	{ "DATE",       0, 0,  NULL },  /* Current date */
	{ "YEAR",       1, 1,  NULL },  /* Year from date */
	{ "MONTH",      1, 1,  NULL },  /* Month from date */
	{ "DAY",        1, 1,  NULL },  /* Day from date */
	{ "DOW",        1, 1,  NULL },  /* Day of week */
	{ "CDOW",       1, 1,  NULL },  /* Day name */
	{ "CMONTH",     1, 1,  NULL },  /* Month name */

	/* Array functions */
	{ "ALEN",       1, 2,  NULL },  /* Array length */
	{ "ASIZE",      2, 2,  NULL },  /* Resize array */
	{ "AADD",       2, 2,  NULL },  /* Add element */
	{ "AINS",       2, 2,  NULL },  /* Insert element */
	{ "ADEL",       2, 2,  NULL },  /* Delete element */
	{ "ASORT",      1, 4,  NULL },  /* Sort array */
	{ "ASCAN",      2, 4,  NULL },  /* Search array */
	{ "AEVAL",      2, 4,  NULL },  /* Evaluate codeblock for each element */
	{ "AFILL",      2, 4,  NULL },  /* Fill array */
	{ "ACOPY",      5, 5,  NULL },  /* Copy array elements */
	{ "ACLONE",     1, 1,  NULL },  /* Clone array */

	/* Type checking */
	{ "VALTYPE",    1, 1,  NULL },  /* Get value type */
	{ "TYPE",       1, 1,  NULL },  /* Get expression type */
	{ "ISNIL",      1, 1,  NULL },  /* Is NIL */
	{ "ISNUMBER",   1, 1,  NULL },  /* Is numeric */
	{ "ISCHARACTER",1, 1,  NULL },  /* Is character */
	{ "ISLOGICAL",  1, 1,  NULL },  /* Is logical */
	{ "ISDATE",     1, 1,  NULL },  /* Is date */
	{ "ISARRAY",    1, 1,  NULL },  /* Is array */
	{ "ISOBJECT",   1, 1,  NULL },  /* Is object */
	{ "ISBLOCK",    1, 1,  NULL },  /* Is codeblock */

	/* Database functions */
	{ "RECNO",      0, 0,  NULL },  /* Current record number */
	{ "RECCOUNT",   0, 0,  NULL },  /* Record count */
	{ "EOF",        0, 0,  NULL },  /* End of file */
	{ "BOF",        0, 0,  NULL },  /* Beginning of file */
	{ "DELETED",    0, 0,  NULL },  /* Is deleted */
	{ "FOUND",      0, 0,  NULL },  /* Record found */
	{ "ALIAS",      0, 1,  NULL },  /* Alias name */
	{ "SELECT",     0, 1,  NULL },  /* Work area */
	{ "DBSEEK",     1, 3,  NULL },  /* Seek record */
	{ "DBSKIP",     0, 1,  NULL },  /* Skip records */
	{ "DBGOTO",     1, 1,  NULL },  /* Go to record */
	{ "DBGOTOP",    0, 0,  NULL },  /* Go to top */
	{ "DBGOBOTTOM", 0, 0,  NULL },  /* Go to bottom */
	{ "FIELDNAME",  1, 1,  NULL },  /* Field name */
	{ "FIELDPOS",   1, 1,  NULL },  /* Field position */
	{ "FCOUNT",     0, 0,  NULL },  /* Field count */

	/* I/O functions */
	{ "FOPEN",      1, 2,  NULL },  /* Open file */
	{ "FCREATE",    1, 2,  NULL },  /* Create file */
	{ "FCLOSE",     1, 1,  NULL },  /* Close file */
	{ "FREAD",      3, 3,  NULL },  /* Read file */
	{ "FWRITE",     2, 3,  NULL },  /* Write file */
	{ "FSEEK",      2, 3,  NULL },  /* Seek in file */
	{ "FERROR",     0, 0,  NULL },  /* File error */
	{ "FILE",       1, 1,  NULL },  /* File exists */
	{ "FERASE",     1, 1,  NULL },  /* Delete file */
	{ "FRENAME",    2, 2,  NULL },  /* Rename file */
	{ "MEMOREAD",   1, 1,  NULL },  /* Read entire file */
	{ "MEMOWRIT",   2, 2,  NULL },  /* Write entire file */

	/* Misc functions */
	{ "EMPTY",      1, 1,  NULL },  /* Is empty */
	{ "EVAL",       1, -1, NULL },  /* Evaluate codeblock */
	{ "PROCNAME",   0, 1,  NULL },  /* Procedure name */
	{ "PROCLINE",   0, 1,  NULL },  /* Procedure line */
	{ "ALERT",      1, 3,  NULL },  /* Alert dialog */
	{ "TONE",       1, 2,  NULL },  /* Sound tone */
	{ "INKEY",      0, 1,  NULL },  /* Get key */
	{ "GETENV",     1, 1,  NULL },  /* Get environment variable */
	{ "SETPOS",     2, 2,  NULL },  /* Set cursor position */
	{ "ROW",        0, 0,  NULL },  /* Get cursor row */
	{ "COL",        0, 0,  NULL },  /* Get cursor column */
	{ "QOUT",       1, -1, NULL },  /* Output */

	/* =================================================================
	 * Dialect-Specific Functions (non-duplicates)
	 * ================================================================= */

	/* Additional Clipper Functions */
	{ "ATAIL",      1, 1,  NULL },  /* Last array element */
	{ "SAVESCREEN", 0, 4,  NULL },  /* Save screen */
	{ "RESTSCREEN", 1, 5,  NULL },  /* Restore screen */
	{ "SETCOLOR",   0, 1,  NULL },  /* Set colors */
	{ "SETCURSOR",  0, 1,  NULL },  /* Set cursor */
	{ "DEVPOS",     2, 2,  NULL },  /* Position device */
	{ "DEVOUT",     1, 2,  NULL },  /* Output to device */
	{ "DISPBEGIN",  0, 0,  NULL },  /* Begin display */
	{ "DISPEND",    0, 0,  NULL },  /* End display */
	{ "DISPCOUNT",  0, 0,  NULL },  /* Display nesting count */
	{ "SCROLL",     5, 6,  NULL },  /* Scroll screen region */
	{ "HARDCR",     1, 1,  NULL },  /* Hard carriage return in memo */
	{ "MLCOUNT",    1, 4,  NULL },  /* Memo line count */
	{ "MLPOS",      1, 4,  NULL },  /* Memo line position */
	{ "MLINE",      2, 5,  NULL },  /* Extract memo line */
	{ "STRZERO",    2, 3,  NULL },  /* Number to string with zeros */
	{ "PADR",       2, 3,  NULL },  /* Pad right */
	{ "PADL",       2, 3,  NULL },  /* Pad left */
	{ "PADC",       2, 3,  NULL },  /* Pad center */

	/* FoxPro/Visual FoxPro Functions */
	{ "CREATEOBJECT", 1, -1, NULL }, /* Create object */
	{ "ADDOBJECT",  3, 3,  NULL },  /* Add object */
	{ "REMOVEOBJECT",1,1,  NULL },  /* Remove object */
	{ "DODEFAULT",  0, -1, NULL },  /* Call parent method */
	{ "BINDEVENT",  3, 5,  NULL },  /* Bind event */
	{ "UNBINDEVENTS",0,3,  NULL },  /* Unbind events */
	{ "PEMSTATUS",  3, 3,  NULL },  /* Property/method status */
	{ "AMEMBERS",   2, 3,  NULL },  /* Get object members */
	{ "ADDPROPERTY",2, 3,  NULL },  /* Add property */
	{ "REMOVEPROPERTY",2,2,NULL },  /* Remove property */
	{ "GETWORDCOUNT",1,2,  NULL },  /* Word count */
	{ "GETWORDNUM", 2, 3,  NULL },  /* Get word */
	{ "STREXTRACT", 2, 5,  NULL },  /* Extract string */
	{ "TEXTMERGE",  1, 3,  NULL },  /* Text merge */
	{ "EVALUATE",   1, 1,  NULL },  /* Evaluate expression */
	{ "EXECSCRIPT", 1, -1, NULL },  /* Execute script */
	{ "SYS",        1, -1, NULL },  /* System function */
	{ "CURSORGETPROP",1,2, NULL },  /* Get cursor property */
	{ "CURSORSETPROP",2,3, NULL },  /* Set cursor property */
	{ "SQLEXEC",    1, 4,  NULL },  /* SQL execute */
	{ "SQLCONNECT", 0, 3,  NULL },  /* SQL connect */
	{ "SQLDISCONNECT",1,1, NULL },  /* SQL disconnect */
	{ "CREATEOBJECTEX",2,-1,NULL }, /* Create COM object */
	{ "GETOBJECT",  0, 2,  NULL },  /* Get COM object */
	{ "VARREAD",    0, 0,  NULL },  /* Variable being read */

	/* Harbour/xHarbour Extensions */
	{ "HB_VERSION", 0, 1,  NULL },  /* Harbour version */
	{ "HB_COMPILER",0, 0,  NULL },  /* Compiler version */
	{ "HB_BUILDDATE",0,0,  NULL },  /* Build date */
	{ "HB_VALTOEXP",1, 1,  NULL },  /* Value to expression */
	{ "HB_DESERIALIZE",1,1,NULL },  /* Deserialize */
	{ "HB_SERIALIZE",1, 1,  NULL },  /* Serialize */
	{ "HB_JSONENCODE",1,2, NULL },  /* JSON encode */
	{ "HB_JSONDECODE",1,2, NULL },  /* JSON decode */
	{ "HB_BASE64ENCODE",1,1,NULL }, /* Base64 encode */
	{ "HB_BASE64DECODE",1,1,NULL }, /* Base64 decode */
	{ "HB_MD5",     1, 1,  NULL },  /* MD5 hash */
	{ "HB_SHA1",    1, 1,  NULL },  /* SHA1 hash */
	{ "HB_SHA256",  1, 1,  NULL },  /* SHA256 hash */
	{ "HB_BLOWFISH",2, 3,  NULL },  /* Blowfish encryption */
	{ "HB_AES",     2, 3,  NULL },  /* AES encryption */
	{ "HB_REGEX",   2, 5,  NULL },  /* Regular expression */
	{ "HB_ATOKENS", 1, 3,  NULL },  /* String to tokens */
	{ "HB_HHASKEY", 2, 2,  NULL },  /* Hash has key */
	{ "HB_HGET",    2, 3,  NULL },  /* Hash get */
	{ "HB_HSET",    3, 3,  NULL },  /* Hash set */
	{ "HB_HDEL",    2, 2,  NULL },  /* Hash delete */
	{ "HB_HKEYS",   1, 1,  NULL },  /* Hash keys */
	{ "HB_HVALUES", 1, 1,  NULL },  /* Hash values */
	{ "HB_HPAIRSAT",2, 2,  NULL },  /* Hash pair at */
	{ "HB_HCLONE",  1, 1,  NULL },  /* Hash clone */
	{ "HB_HMERGE",  3, 4,  NULL },  /* Hash merge */
	{ "HB_THREADSTART",1,-1,NULL }, /* Start thread */
	{ "HB_THREADJOIN",1, 2, NULL }, /* Join thread */
	{ "HB_THREADQUIT",0, 1, NULL }, /* Quit thread */
	{ "HB_MUTEXCREATE",0,0,NULL },  /* Create mutex */
	{ "HB_MUTEXLOCK",1, 2, NULL },  /* Lock mutex */
	{ "HB_MUTEXUNLOCK",1,1,NULL },  /* Unlock mutex */

	/* Xbase++ Specific Functions */
	{ "APPOBJECT",  0, 1,  NULL },  /* Application object */
	{ "APPNAME",    0, 1,  NULL },  /* Application name */
	{ "SETAPPFOCUS",0, 1,  NULL },  /* Set app focus */
	{ "BEGINSEQUENCE",0,0, NULL },  /* Begin sequence */
	{ "ENDSEQUENCE",0, 0,  NULL },  /* End sequence */
	{ "THROW",      1, 1,  NULL },  /* Throw exception */
	{ "THREADOBJECT",0,1,  NULL },  /* Thread object */
	{ "THREADSLEEP",1, 1,  NULL },  /* Thread sleep */
	{ "THREADWAIT", 1, 2,  NULL },  /* Thread wait */
	{ "CLASSOBJECT",1, 1,  NULL },  /* Class object */
	{ "CLASSNAME",  1, 1,  NULL },  /* Class name */
	{ "CLASSMETHOD",2, 2,  NULL },  /* Class method */
	{ "OBJECTFROMCODE",1,1,NULL },  /* Object from code */
	{ "CODEBLOCKFROMCODE",1,1,NULL},/* Codeblock from code */
	{ "ISDERIVEDCLASS",2,2,NULL },  /* Is derived class */
	{ "XBASE",      0, 0,  NULL },  /* Xbase identifier */

	/* Additional common functions across dialects */
	{ "NETERR",     0, 1,  NULL },  /* Network error */
	{ "LUPDATE",    0, 1,  NULL },  /* Last update date */
	{ "READKEY",    0, 0,  NULL },  /* Read key */
	{ "READVAR",    0, 0,  NULL },  /* Read variable */
	{ "UPDATED",    0, 0,  NULL },  /* Updated flag */
	{ "SECONDS",    0, 0,  NULL },  /* Seconds since midnight */
	{ "TIME",       0, 0,  NULL },  /* Current time */
	{ "DISKSPACE",  0, 1,  NULL },  /* Disk space */
	{ "DOSDATE",    0, 0,  NULL },  /* DOS date */
	{ "DOSTIME",    0, 0,  NULL },  /* DOS time */
	{ "GETACTIVE",  0, 0,  NULL },  /* Get active window */
	{ "SETKEY",     2, 2,  NULL },  /* Set hotkey */
	{ "NEXTKEY",    0, 1,  NULL },  /* Next key */
	{ "LASTKEY",    0, 1,  NULL },  /* Last key */
	{ "KEYBOARD",   1, 1,  NULL },  /* Stuff keyboard */
	{ "CLEAR",      0, 0,  NULL },  /* Clear gets */
	{ "READMODAL",  0, 1,  NULL },  /* Read modal */
	{ "READFORMAT", 0, 1,  NULL },  /* Read format */
	{ "READKILL",   1, 1,  NULL },  /* Read kill */
	{ "READUPDATED",1, 1,  NULL },  /* Read updated */
	{ "VERSION",    0, 1,  NULL },  /* Version */
	{ "OS",         0, 0,  NULL },  /* Operating system */
	{ "SETENV",     2, 2,  NULL },  /* Set environment (Harbour) */
	{ "RUN",        1, 1,  NULL },  /* Run external command */
	{ "__RUN",      1, 1,  NULL },  /* Run (alternative) */
	{ "DIRCHANGE",  1, 1,  NULL },  /* Change directory */
	{ "DIRMAKE",    1, 1,  NULL },  /* Make directory */
	{ "DIRREMOVE",  1, 1,  NULL },  /* Remove directory */
	{ "ADIR",       1, 5,  NULL },  /* Directory list to array */
	{ "DIRECTORY",  0, 1,  NULL },  /* Directory listing */

	/* Terminator */
	{ NULL,         0, 0,  NULL }
};

/*
 * Initialize built-in functions
 * Adds all built-in functions to the global symbol table
 */
void
init_builtins(void)
{
	BUILTIN *bp;
	SYMTAB *sp;
	TNODE *ftype;
	int i;

	for (bp = builtins; bp->name != NULL; bp++) {
		/* Create function type */
		if (bp->ret_type == NULL)
			bp->ret_type = make_type(TVARIANT);

		ftype = make_func_type(bp->ret_type, NULL);

		/* Install in symbol table at global scope */
		sp = install(bp->name, S_FUNC, ftype, 0);
		sp->sflags |= SEXTERNAL;
	}
}

/*
 * Lookup a built-in function by name
 */
SYMTAB *
lookup_builtin(char *name)
{
	BUILTIN *bp;

	for (bp = builtins; bp->name != NULL; bp++) {
		if (strcasecmp(name, bp->name) == 0) {
			return lookup(bp->name, 0);
		}
	}

	return NULL;
}

/*
 * Check if function call has correct number of arguments
 */
int
check_builtin_args(char *name, int nargs)
{
	BUILTIN *bp;

	for (bp = builtins; bp->name != NULL; bp++) {
		if (strcasecmp(name, bp->name) == 0) {
			if (nargs < bp->min_args) {
				error("too few arguments to %s (expected %d, got %d)",
				      name, bp->min_args, nargs);
				return 0;
			}
			if (bp->max_args != -1 && nargs > bp->max_args) {
				error("too many arguments to %s (expected %d, got %d)",
				      name, bp->max_args, nargs);
				return 0;
			}
			return 1;
		}
	}

	/* Not a built-in, assume user function */
	return 1;
}
