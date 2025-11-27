/*
 * Copyright (c) 2025 PCC Prolog Compiler
 *
 * SWI-Prolog Extensions
 * Modern Prolog features from SWI-Prolog
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pass1.h"
#include "standards.h"

/* SWI-Prolog specific built-ins */
struct swi_builtin {
	char *name;
	int arity;
	char *category;
	char *description;
};

static struct swi_builtin swi_builtins[] = {
	/* Dicts (SWI 7.x+) */
	{".", 3, "dicts", "Dict field access"},
	{"get_dict", 3, "dicts", "Get dict value"},
	{"put_dict", 3, "dicts", "Put dict value"},
	{"put_dict", 4, "dicts", "Put dict value (extended)"},
	{"del_dict", 4, "dicts", "Delete dict key"},
	{":<", 2, "dicts", "Partial dict unification"},
	{">:<", 2, "dicts", "Symmetric dict unification"},
	{"dict_create", 3, "dicts", "Create dict"},
	{"dict_pairs", 3, "dicts", "Convert dict to pairs"},
	{"is_dict", 1, "dicts", "Test if dict"},
	{"is_dict", 2, "dicts", "Test if dict with tag"},

	/* Strings (SWI 7.x+) */
	{"string", 1, "strings", "Test if string"},
	{"string_concat", 3, "strings", "String concatenation"},
	{"string_length", 2, "strings", "String length"},
	{"string_codes", 2, "strings", "String to codes"},
	{"string_chars", 2, "strings", "String to chars"},
	{"atomics_to_string", 2, "strings", "Atomics to string"},
	{"atomics_to_string", 3, "strings", "Atomics to string with separator"},
	{"string_lower", 2, "strings", "Convert to lowercase"},
	{"string_upper", 2, "strings", "Convert to uppercase"},
	{"split_string", 4, "strings", "Split string"},
	{"sub_string", 5, "strings", "Substring"},
	{"re_match", 2, "strings", "Regex match"},
	{"re_matchsub", 4, "strings", "Regex match with substrings"},
	{"re_replace", 4, "strings", "Regex replace"},

	/* Threads */
	{"thread_create", 3, "threads", "Create thread"},
	{"thread_join", 2, "threads", "Join thread"},
	{"thread_detach", 1, "threads", "Detach thread"},
	{"thread_self", 1, "threads", "Get current thread ID"},
	{"thread_exit", 1, "threads", "Exit thread"},
	{"thread_send_message", 2, "threads", "Send message to thread"},
	{"thread_get_message", 1, "threads", "Receive message"},
	{"thread_peek_message", 1, "threads", "Peek at message"},
	{"thread_property", 2, "threads", "Thread properties"},
	{"mutex_create", 1, "threads", "Create mutex"},
	{"mutex_lock", 1, "threads", "Lock mutex"},
	{"mutex_unlock", 1, "threads", "Unlock mutex"},
	{"with_mutex", 2, "threads", "Execute with mutex"},

	/* Global variables */
	{"nb_setval", 2, "global", "Non-backtrackable set"},
	{"nb_getval", 2, "global", "Non-backtrackable get"},
	{"nb_linkval", 2, "global", "Non-backtrackable link"},
	{"nb_current", 2, "global", "Current non-backtrackable"},
	{"b_setval", 2, "global", "Backtrackable set"},
	{"b_getval", 2, "global", "Backtrackable get"},

	/* Records */
	{"recorda", 3, "records", "Record at beginning"},
	{"recordz", 3, "records", "Record at end"},
	{"recorded", 3, "records", "Query record"},
	{"erase", 1, "records", "Erase record"},

	/* Attributed variables */
	{"put_attr", 3, "attvar", "Put attribute"},
	{"get_attr", 3, "attvar", "Get attribute"},
	{"del_attr", 2, "attvar", "Delete attribute"},
	{"attr_unify_hook", 2, "attvar", "Unification hook"},
	{"attribute_goals", 3, "attvar", "Attribute goals"},
	{"put_attrs", 2, "attvar", "Put multiple attributes"},
	{"get_attrs", 2, "attvar", "Get all attributes"},

	/* Rational numbers */
	{"rational", 1, "rational", "Test if rational"},
	{"rational", 3, "rational", "Create rational from parts"},
	{"rationalize", 2, "rational", "Convert to rational"},
	{"rdiv", 2, "rational", "Rational division"},

	/* Format */
	{"format", 2, "io", "Formatted write"},
	{"format", 3, "io", "Formatted write to stream"},
	{"sformat", 3, "io", "Format to string"},

	/* Lists */
	{"append", 2, "lists", "Append list of lists"},
	{"append", 3, "lists", "Append two lists"},
	{"length", 2, "lists", "List length"},
	{"member", 2, "lists", "List membership"},
	{"memberchk", 2, "lists", "Deterministic member"},
	{"reverse", 2, "lists", "Reverse list"},
	{"sort", 2, "lists", "Sort list"},
	{"msort", 2, "lists", "Merge sort (stable)"},
	{"keysort", 2, "lists", "Sort on keys"},
	{"nth0", 3, "lists", "Nth element (0-indexed)"},
	{"nth1", 3, "lists", "Nth element (1-indexed)"},
	{"last", 2, "lists", "Last element"},
	{"sumlist", 2, "lists", "Sum of list"},
	{"max_list", 2, "lists", "Maximum of list"},
	{"min_list", 2, "lists", "Minimum of list"},
	{"numlist", 3, "lists", "Generate number list"},

	/* Association lists (AVL trees) */
	{"list_to_assoc", 2, "assoc", "Create association list"},
	{"assoc_to_list", 2, "assoc", "Convert to list"},
	{"get_assoc", 3, "assoc", "Get value from assoc"},
	{"put_assoc", 4, "assoc", "Put value in assoc"},
	{"del_assoc", 4, "assoc", "Delete from assoc"},

	/* Option processing */
	{"option", 2, "options", "Extract option"},
	{"option", 3, "options", "Extract option with default"},
	{"select_option", 3, "options", "Select and remove option"},
	{"merge_options", 3, "options", "Merge option lists"},

	/* Debugging */
	{"trace", 0, "debug", "Enable trace mode"},
	{"notrace", 0, "debug", "Disable trace mode"},
	{"spy", 1, "debug", "Set spy point"},
	{"nospy", 1, "debug", "Remove spy point"},
	{"debugging", 0, "debug", "Show debug status"},
	{"debug", 3, "debug", "Debug topic message"},
	{"nodebug", 1, "debug", "Disable debug topic"},
	{"assertion", 1, "debug", "Runtime assertion"},

	/* Execution control */
	{"catch", 3, "control", "Exception catching"},
	{"throw", 1, "control", "Throw exception"},
	{"call_cleanup", 2, "control", "Call with cleanup"},
	{"setup_call_cleanup", 3, "control", "Setup-call-cleanup"},
	{"ignore", 1, "control", "Ignore failure"},
	{"forall", 2, "control", "Universal quantification"},
	{"aggregate", 3, "control", "Aggregate solutions"},
	{"aggregate_all", 3, "control", "Aggregate all solutions"},

	/* Module system */
	{"use_module", 1, "modules", "Load module"},
	{"use_module", 2, "modules", "Load module with imports"},
	{"module", 2, "modules", "Define module"},
	{"import", 1, "modules", "Import predicate"},
	{"export", 1, "modules", "Export predicate"},
	{"module_transparent", 1, "modules", "Declare transparent"},

	/* Term expansion */
	{"expand_term", 2, "expansion", "Expand term"},
	{"expand_goal", 2, "expansion", "Expand goal"},
	{"term_expansion", 2, "expansion", "Term expansion hook"},
	{"goal_expansion", 2, "expansion", "Goal expansion hook"},

	/* Operators */
	{"current_op", 3, "operators", "Query operator"},
	{"op", 3, "operators", "Define operator"},

	/* Misc utilities */
	{"between", 3, "utils", "Generate integers"},
	{"succ", 2, "utils", "Successor"},
	{"plus", 3, "utils", "Addition (for constraint solving)"},
	{"divmod", 4, "utils", "Division with remainder"},
	{"nth", 3, "utils", "Nth element (flexible)"},
	{"ground", 1, "utils", "Test if ground"},
	{"cyclic_term", 1, "utils", "Test if cyclic"},
	{"subsumes_term", 2, "utils", "Subsumption test"},
	{"unifiable", 3, "utils", "Unifiability with substitution"},
	{"?=", 2, "utils", "Unifiable test"},

	/* Terminator */
	{NULL, 0, NULL, NULL}
};

/* Initialize SWI-Prolog built-ins */
void init_swi_builtins(void) {
	int i;

	if (!standard_features.swi_extensions)
		return;

	for (i = 0; swi_builtins[i].name != NULL; i++) {
		struct predicate *pred = define_predicate(
			swi_builtins[i].name,
			swi_builtins[i].arity
		);
		pred->is_builtin = 1;
	}
}

/* Print SWI-Prolog extensions by category */
void print_swi_categories(FILE *fp) {
	fprintf(fp, "SWI-Prolog Extensions by Category:\n\n");
	fprintf(fp, "- Dicts (SWI 7.x+)\n");
	fprintf(fp, "- Strings\n");
	fprintf(fp, "- Threads\n");
	fprintf(fp, "- Global Variables\n");
	fprintf(fp, "- Records\n");
	fprintf(fp, "- Attributed Variables\n");
	fprintf(fp, "- Rational Numbers\n");
	fprintf(fp, "- Format\n");
	fprintf(fp, "- Lists\n");
	fprintf(fp, "- Association Lists (AVL trees)\n");
	fprintf(fp, "- Option Processing\n");
	fprintf(fp, "- Debugging\n");
	fprintf(fp, "- Execution Control\n");
	fprintf(fp, "- Module System\n");
	fprintf(fp, "- Term Expansion\n");
	fprintf(fp, "- Operators\n");
	fprintf(fp, "- Utilities\n");
}
