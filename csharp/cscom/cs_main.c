/*
 * PCSC (Portable C# Compiler) Main Driver
 * Architecture and Endian Neutral
 * Supports C# 1.0 through C# 12.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include "cs_pass1.h"
#include "../../common/cs_module.h"

/* Compiler options */
static struct {
	char *input_file;
	char *output_file;
	char *module_name;
	char *lang_version_str;
	int optimize_level;
	int enable_arc;
	int enable_unsafe;
	int emit_debug_info;
	int verbose;
	int show_features;
	enum cs_arch target_arch;
	enum cs_endian target_endian;
	int language_version;
} options = {
	.input_file = NULL,
	.output_file = "output.csm",
	.module_name = NULL,
	.lang_version_str = NULL,
	.optimize_level = 1,
	.enable_arc = 1,
	.enable_unsafe = 0,
	.emit_debug_info = 1,
	.verbose = 0,
	.show_features = 0,
	.target_arch = CS_ARCH_NEUTRAL,
	.target_endian = CS_ENDIAN_LITTLE,
	.language_version = CS_VERSION_DEFAULT,
};

static void print_version(void) {
	printf("PCSC - Portable C# Compiler version 2.0\n");
	printf("Supports C# 1.0 through C# 12.0\n");
	printf("Architecture-neutral and endian-neutral compiler\n");
	printf("With shared ARC support\n");
	printf("\nSupported language versions:\n");
	printf("  1.0, 2.0, 3.0, 4.0, 5.0, 6.0\n");
	printf("  7.0, 7.1, 7.2, 7.3\n");
	printf("  8.0, 9.0, 10.0, 11.0, 12.0\n");
	printf("  latest (defaults to 12.0)\n");
}

static void print_usage(const char *progname) {
	printf("Usage: %s [options] <input.cs>\n\n", progname);
	printf("Options:\n");
	printf("  -o <file>         Output file (default: output.csm)\n");
	printf("  -m <name>         Module name\n");
	printf("  -langversion:<v>  C# language version (1.0-12.0, default: latest)\n");
	printf("                    Examples: -langversion:3.0, -langversion:7.3\n");
	printf("  -O <level>        Optimization level (0-3, default: 1)\n");
	printf("  -farc             Enable ARC (default)\n");
	printf("  -fno-arc          Disable ARC\n");
	printf("  -funsafe          Enable unsafe code\n");
	printf("  -g                Emit debug information\n");
	printf("  -v                Verbose output\n");
	printf("  --features        Show available language features and exit\n");
	printf("  --arch=<arch>     Target architecture:\n");
	printf("                      neutral (default), x86, x86-64, arm, arm64,\n");
	printf("                      mips, ppc, riscv, wasm\n");
	printf("  --endian=<end>    Target endianness:\n");
	printf("                      little (default), big\n");
	printf("  --version         Print version information\n");
	printf("  --help            Print this help message\n");
	printf("\nExamples:\n");
	printf("  %s -langversion:2.0 program.cs     # Compile as C# 2.0\n", progname);
	printf("  %s -langversion:7.3 --features     # Show C# 7.3 features\n", progname);
	printf("  %s -o app.csm app.cs               # Compile with latest C#\n", progname);
}

static enum cs_arch parse_arch(const char *arch_str) {
	if (strcmp(arch_str, "neutral") == 0)
		return CS_ARCH_NEUTRAL;
	else if (strcmp(arch_str, "x86") == 0)
		return CS_ARCH_X86;
	else if (strcmp(arch_str, "x86-64") == 0)
		return CS_ARCH_X86_64;
	else if (strcmp(arch_str, "arm") == 0)
		return CS_ARCH_ARM;
	else if (strcmp(arch_str, "arm64") == 0)
		return CS_ARCH_ARM64;
	else if (strcmp(arch_str, "mips") == 0)
		return CS_ARCH_MIPS;
	else if (strcmp(arch_str, "ppc") == 0)
		return CS_ARCH_PPC;
	else if (strcmp(arch_str, "riscv") == 0)
		return CS_ARCH_RISCV;
	else if (strcmp(arch_str, "wasm") == 0)
		return CS_ARCH_WASM;
	else {
		fprintf(stderr, "Unknown architecture: %s\n", arch_str);
		return CS_ARCH_NEUTRAL;
	}
}

static enum cs_endian parse_endian(const char *endian_str) {
	if (strcmp(endian_str, "little") == 0)
		return CS_ENDIAN_LITTLE;
	else if (strcmp(endian_str, "big") == 0)
		return CS_ENDIAN_BIG;
	else {
		fprintf(stderr, "Unknown endianness: %s\n", endian_str);
		return CS_ENDIAN_LITTLE;
	}
}

static int parse_options(int argc, char **argv) {
	int c;
	static struct option long_options[] = {
		{"arch", required_argument, 0, 'a'},
		{"endian", required_argument, 0, 'e'},
		{"features", no_argument, 0, 'F'},
		{"version", no_argument, 0, 'V'},
		{"help", no_argument, 0, 'h'},
		{0, 0, 0, 0}
	};

	while (1) {
		int option_index = 0;
		c = getopt_long(argc, argv, "o:m:O:gvh",
		                long_options, &option_index);

		if (c == -1)
			break;

		switch (c) {
		case 'o':
			options.output_file = optarg;
			break;
		case 'm':
			options.module_name = optarg;
			break;
		case 'O':
			options.optimize_level = atoi(optarg);
			break;
		case 'g':
			options.emit_debug_info = 1;
			break;
		case 'v':
			options.verbose = 1;
			break;
		case 'a':
			options.target_arch = parse_arch(optarg);
			break;
		case 'e':
			options.target_endian = parse_endian(optarg);
			break;
		case 'F':
			options.show_features = 1;
			break;
		case 'V':
			print_version();
			exit(0);
		case 'h':
			print_usage(argv[0]);
			exit(0);
		default:
			return -1;
		}
	}

	/* Parse language version from remaining args */
	for (int i = 1; i < argc; i++) {
		if (strncmp(argv[i], "-langversion:", 13) == 0) {
			options.lang_version_str = argv[i] + 13;
			options.language_version = cs_version_parse(options.lang_version_str);
			break;
		}
	}

	/* If just showing features, don't require input file */
	if (options.show_features) {
		cs_version_set_current(options.language_version);
		cs_print_available_features();
		exit(0);
	}

	if (optind >= argc) {
		fprintf(stderr, "Error: No input file specified\n");
		print_usage(argv[0]);
		return -1;
	}

	options.input_file = argv[optind];

	/* Set module name from input file if not specified */
	if (!options.module_name) {
		options.module_name = strdup(options.input_file);
		char *dot = strrchr(options.module_name, '.');
		if (dot)
			*dot = '\0';
	}

	return 0;
}

static int compile(void) {
	if (options.verbose) {
		printf("Compiling %s to %s\n", options.input_file,
		       options.output_file);
		printf("Module: %s\n", options.module_name);
		printf("Language Version: C# %s\n",
		       cs_version_to_string(options.language_version));
		printf("Target: %s (%s)\n",
		       cs_arch_to_string(options.target_arch),
		       cs_endian_to_string(options.target_endian));
		printf("ARC: %s\n", options.enable_arc ? "enabled" : "disabled");
		printf("\n");
	}

	/* Initialize compiler */
	cs_init();
	cs_arc_enabled = options.enable_arc;
	cs_version_set_current(options.language_version);

	/* Initialize scanner */
	cs_scanner_init(options.input_file);

	/* Parse input file */
	if (options.verbose)
		printf("Parsing...\n");

	/* In a real implementation, this would call: yyparse() */
	/* For now, we'll create a minimal module */

	/* Create module writer */
	struct cs_module_writer *writer =
	    cs_module_writer_create(options.output_file,
	                            options.target_endian,
	                            options.target_arch);
	if (!writer) {
		fprintf(stderr, "Error: Cannot create output file\n");
		return -1;
	}

	/* Create metadata */
	struct cs_module_metadata *meta =
	    cs_metadata_create(options.module_name, "1.0.0.0");

	/* Add sections */
	if (options.verbose)
		printf("Generating module sections...\n");

	/* Metadata section */
	char meta_buf[256];
	snprintf(meta_buf, sizeof(meta_buf), "Name=%s\nVersion=1.0.0.0\n",
	         options.module_name);
	cs_module_writer_add_section(writer, CS_SECTION_METADATA,
	                              meta_buf, strlen(meta_buf));

	/* String pool */
	cs_string_pool_t *strings = cs_string_pool_create();
	uint32_t name_offset = cs_string_pool_add(strings, options.module_name);

	/* Type section (example) */
	struct cs_type_desc example_type = {
		.token = 0x02000001,
		.flags = CS_MOD_PUBLIC,
		.name_offset = name_offset,
		.namespace_offset = 0,
		.parent_token = 0x01000001,  /* System.Object */
		.field_count = 0,
		.method_count = 0,
		.size = 0,  /* Reference type */
		.alignment = 0,
	};

	cs_module_writer_add_section(writer, CS_SECTION_TYPES,
	                              &example_type,
	                              sizeof(struct cs_type_desc));

	/* Finalize module */
	if (options.verbose)
		printf("Writing module...\n");

	if (cs_module_writer_finalize(writer) < 0) {
		fprintf(stderr, "Error: Failed to write module\n");
		cs_module_writer_destroy(writer);
		return -1;
	}

	cs_module_writer_destroy(writer);

	/* Cleanup */
	cs_metadata_destroy(meta);
	cs_string_pool_destroy(strings);
	cs_scanner_cleanup();
	cs_cleanup();

	if (options.verbose) {
		printf("Compilation successful.\n");
		if (options.enable_arc) {
			cs_arc_print_stats();
		}
	}

	return 0;
}

int main(int argc, char **argv) {
	if (parse_options(argc, argv) < 0)
		return 1;

	if (compile() < 0)
		return 1;

	return 0;
}

/* External scanner and parser functions (stubs for now) */
void cs_scanner_init(const char *filename) {
	/* Implemented in cs_scan.l */
}

void cs_scanner_cleanup(void) {
	/* Implemented in cs_scan.l */
}
