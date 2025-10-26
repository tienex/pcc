/*	$Id$	*/

/*
 * Ruby compiler main entry point
 *
 * Based on PCC ccom structure
 */

#include "config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>
#include <string.h>
#include <stdlib.h>

#include "pass1.h"
#include "pass2.h"

int bdebug, ddebug, edebug, idebug, ndebug;
int odebug, pdebug, sdebug, tdebug, xdebug, wdebug;
int b2debug, c2debug, e2debug, f2debug, g2debug, o2debug;
int r2debug, s2debug, t2debug, u2debug, x2debug;
int gflag, kflag;
int pflag, sflag;
int sspflag;
int sehflag;
int xscp, xssa, xtailcall, xtemps, xdeljumps, xdce, xinline, xccp, xgnu89, xgnu99;
int xuchar;
int freestanding;
char *prgname, *ftitle;

static void prtstats(void);

static void
usage(void)
{
	(void)fprintf(stderr, "usage: %s [option] [infile] [outfile]...\n",
	    prgname);
	exit(1);
}

static void
segvcatch(int a)
{
	char buf[1024];

	snprintf(buf, sizeof buf, "%sinternal compiler error: %s, line %d\n",
	    nerrors ? "" : "major ", ftitle, lineno);
	(void)write(STDERR_FILENO, buf, strlen(buf));
	_exit(1);
}

static void
xopt(char *str)
{
	if (strcmp(str, "ssa") == 0)
		xssa++;
	else if (strcmp(str, "tailcall") == 0)
		xtailcall++;
	else if (strcmp(str, "temps") == 0)
		xtemps++;
	else if (strcmp(str, "deljumps") == 0)
		xdeljumps++;
	else if (strcmp(str, "dce") == 0)
		xdce++;
	else if (strcmp(str, "inline") == 0)
		xinline++;
	else if (strcmp(str, "ccp") == 0)
		xccp++;
	else if (strcmp(str, "scp") == 0)
		xscp++;
	else {
		fprintf(stderr, "unknown -x option '%s'\n", str);
		usage();
	}
}

static void
fflags(char *str)
{
	int flagval = 1;

	if (strncmp("no-", str, 3) == 0) {
		str += 3;
		flagval = 0;
	}

#ifndef PASS2
	if (strcmp(str, "stack-protector") == 0)
		sspflag = flagval;
	else if (strcmp(str, "stack-protector-all") == 0)
		sspflag = flagval;
	else if (strcmp(str, "freestanding") == 0)
		freestanding = flagval;
	else {
		fprintf(stderr, "unknown -f option '%s'\n", str);
		usage();
	}
#endif
}

/* control multiple files */
int
main(int argc, char *argv[])
{
	int ch, sdflag;

	prgname = argv[0];

	while ((ch = getopt(argc, argv, "OT:VW:X:Z:f:gkm:psvwx:")) != -1) {
		switch (ch) {
#ifndef PASS2
		case 'X':	/* pass1 debugging */
			while (*optarg)
				switch (*optarg++) {
				case 'b': ++bdebug; break;
				case 'd': ++ddebug; break;
				case 'e': ++edebug; break;
				case 'i': ++idebug; break;
				case 'n': ++ndebug; break;
				case 'o': ++odebug; break;
				case 'p': ++pdebug; break;
				case 's': ++sdebug; break;
				case 't': ++tdebug; break;
				case 'x': ++xdebug; break;
				default:
					fprintf(stderr, "unknown -X flag '%c'\n",
					    optarg[-1]);
					exit(1);
				}
			break;
#endif
#ifndef PASS1
		case 'Z':	/* pass2 debugging */
			while (*optarg)
				switch (*optarg++) {
				case 'b': ++b2debug; break;
				case 'c': ++c2debug; break;
				case 'e': ++e2debug; break;
				case 'f': ++f2debug; break;
				case 'g': ++g2debug; break;
				case 'n': ++ndebug; break;
				case 'o': ++o2debug; break;
				case 'r': ++r2debug; break;
				case 's': ++s2debug; break;
				case 't': ++t2debug; break;
				case 'u': ++u2debug; break;
				case 'x': ++x2debug; break;
				default:
					fprintf(stderr, "unknown -Z flag '%c'\n",
					    optarg[-1]);
					exit(1);
				}
			break;
#endif
		case 'f': /* Language */
			fflags(optarg);
			break;

		case 'g': /* Debugging */
			++gflag;
			break;

		case 'k': /* PIC code */
			++kflag;
			break;

		case 'p': /* Profiling */
			++pflag;
			break;

		case 's': /* Statistics */
			++sflag;
			break;

		case 'w': /* No warnings emitted */
			++wdebug;
			break;

		case 'x': /* Different settings */
			xopt(optarg);
			break;

		case 'v':
			printf("rcom (Ruby compiler): %s\n", VERSSTR);
			break;

		case '?':
		default:
			usage();
		}
	}
	argc -= optind;
	argv += optind;

	ftitle = strdup("<stdin>");
	if (argc > 0 && strcmp(argv[0], "-") != 0) {
		ftitle = strdup(argv[0]);
		if (freopen(argv[0], "r", stdin) == NULL) {
			fprintf(stderr, "open input file '%s':",
			    argv[0]);
			perror(NULL);
			exit(1);
		}
	}
	if (argc > 1 && strcmp(argv[1], "-") != 0) {
		if (freopen(argv[1], "w", stdout) == NULL) {
			fprintf(stderr, "open output file '%s':",
			    argv[1]);
			perror(NULL);
			exit(1);
		}
	}

	mkdope();
	signal(SIGSEGV, segvcatch);
#ifdef SIGBUS
	signal(SIGBUS, segvcatch);
#endif

#ifndef PASS2
	lineno = 1;
	sdflag = ddebug;
	ddebug = 0;

	/* Initialize compiler */
	ftnend();		/* Initialize function handling */
#endif

	/* Parse the input */
	if (yyparse() != 0 || nerrors > 0) {
		if (nerrors == 0)
			nerrors = 1;
		fprintf(stderr, "%d error(s)\n", nerrors);
		exit(1);
	}

	/* Print statistics if requested */
	if (sflag)
		prtstats();

	return 0;
}

static void
prtstats(void)
{
	fprintf(stderr, "Ruby compiler statistics:\n");
	fprintf(stderr, "Lines processed: %d\n", lineno);
}
