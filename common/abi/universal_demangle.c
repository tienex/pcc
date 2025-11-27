/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Demangler Implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "universal_demangle.h"
#include "demangle.h"

/*
 * Auto-detect ABI from mangled name
 */
abi_kind_t
demangle_detect_abi(const char *name)
{
	if (!name || !*name)
		return ABI_AUTO;

	/* Itanium C++: _Z prefix */
	if (name[0] == '_' && name[1] == 'Z')
		return ABI_ITANIUM;

	/* MSVC C++: ? prefix */
	if (name[0] == '?') {
		/* Check for Digital Mars (has specific patterns) */
		if (strstr(name, "@@QA") || strstr(name, "@@SA"))
			return ABI_DMC;
		return ABI_MSVC;
	}

	/* D Language: _D prefix */
	if (name[0] == '_' && name[1] == 'D')
		return ABI_DLANG;

	/* Swift: $s or _T prefix */
	if (name[0] == '$' && name[1] == 's')
		return ABI_SWIFT;
	if (name[0] == '_' && name[1] == 'T')
		return ABI_SWIFT;

	/* Objective-C: -[Class method] or +[Class method] */
	if ((name[0] == '-' || name[0] == '+') && name[1] == '[')
		return ABI_APPLE_OBJC2;

	/* Apple symbols: _OBJC_ prefix */
	if (strncmp(name, "_OBJC_CLASS_$_", 14) == 0)
		return ABI_APPLE_OBJC2;
	if (strncmp(name, "_OBJC_METACLASS_$_", 18) == 0)
		return ABI_APPLE_OBJC2;
	if (strncmp(name, ".objc_class_name_", 17) == 0)
		return ABI_APPLE_OBJC1;

	/* CoreFoundation: CF or __CF prefix */
	if (strncmp(name, "CF", 2) == 0 || strncmp(name, "__CF", 4) == 0)
		return ABI_COREFOUNDATION;

	/* Java JNI: Java_ prefix */
	if (strncmp(name, "Java_", 5) == 0)
		return ABI_JAVA;

	/* Go: go. prefix */
	if (strncmp(name, "go.", 3) == 0)
		return ABI_GO;

	/* Kotlin: kfun: or kvar: */
	if (strncmp(name, "kfun:", 5) == 0 || strncmp(name, "kvar:", 5) == 0)
		return ABI_KOTLIN;

	/* Dart: Dart_ prefix */
	if (strncmp(name, "Dart_", 5) == 0)
		return ABI_DART;

	/* FreePascal: uppercase with _ */
	if (strncmp(name, "UNIT_", 5) == 0 || strncmp(name, "P$", 2) == 0)
		return ABI_FREEPASCAL;

	/* GNU Pascal: _p_ prefix */
	if (strncmp(name, "_p_", 3) == 0)
		return ABI_GNU_PASCAL;

	/* GNAT Ada: ada__ prefix */
	if (strncmp(name, "ada__", 5) == 0)
		return ABI_GNAT;

	/* Fortran: lowercase with trailing underscore */
	if (name[0] != '_' && strchr(name, '_') == name + strlen(name) - 1) {
		/* Could be Fortran - check for common patterns */
		if (strstr(name, "_MOD_"))
			return ABI_GFORTRAN;  /* Module procedure */
		/* Could also be ifort or NAG, but default to gfortran */
		if (islower(name[0]))
			return ABI_GFORTRAN;
	}

	/* NAG Fortran: all uppercase */
	if (isupper(name[0]) && !strchr(name, '_'))
		return ABI_NAG_FORTRAN;

	/* GHC Haskell: Z-encoded with _closure suffix */
	if (strstr(name, "_closure") || strstr(name, "_info"))
		return ABI_GHC;

	/* OCaml: caml prefix */
	if (strncmp(name, "caml", 4) == 0)
		return ABI_OCAML;

	/* Julia: julia_ or jl_ prefix */
	if (strncmp(name, "julia_", 6) == 0 || strncmp(name, "jl_", 3) == 0)
		return ABI_JULIA;

	/* Nim: __ with module pattern */
	if (strstr(name, "__module_") || strstr(name, "__global_"))
		return ABI_NIM;

	/* V: module__ pattern */
	if (strstr(name, "__") && !strncmp(name, "main__", 6))
		return ABI_VLANG;

	/* LLVM IR: @ prefix */
	if (name[0] == '@')
		return ABI_LLVM_IR;

	/* WebAssembly: $ prefix */
	if (name[0] == '$')
		return ABI_WASM;

	/* Rust: _ZN with Rust-specific patterns */
	if (strncmp(name, "_ZN", 3) == 0) {
		/* Could be Itanium or Rust, check for Rust patterns */
		if (strstr(name, "rustc") || strstr(name, "_hcrate"))
			return ABI_RUST;
		return ABI_ITANIUM;
	}

	/* GNU Old C++: __ prefix with length */
	if (name[0] == '_' && name[1] == '_' && isdigit(name[2]))
		return ABI_GNU_OLD;

	/* Cfront: __F pattern */
	if (strstr(name, "__F"))
		return ABI_CFRONT;

	/* Borland: @ prefix */
	if (name[0] == '@')
		return ABI_BORLAND;

	/* Watcom: W? prefix */
	if (name[0] == 'W' && name[1] == '?')
		return ABI_WATCOM;

	/* CLR/.NET: contains dots in namespace */
	if (strchr(name, '.') && isupper(name[0]))
		return ABI_CLR;

	/* Crystal: * prefix */
	if (name[0] == '*')
		return ABI_CRYSTAL;

	/* Default: assume C symbol (not mangled) */
	return ABI_AUTO;
}

/*
 * Get demangled name using specific ABI
 */
char *
demangle_with_abi(const char *mangled_name, abi_kind_t abi)
{
	if (!mangled_name)
		return NULL;

	switch (abi) {
	case ABI_ITANIUM:
	case ABI_ARM:
	case ABI_RUST:
		return demangle_itanium(mangled_name, DEMANGLE_OPT_NONE);

	case ABI_MSVC:
	case ABI_DMC:
		return demangle_msvc(mangled_name, DEMANGLE_OPT_NONE);

	/* For ABIs without demanglers, return copy */
	case ABI_DLANG:
	case ABI_SWIFT:
	case ABI_GO:
	case ABI_JAVA:
	case ABI_KOTLIN:
	case ABI_DART:
	case ABI_FREEPASCAL:
	case ABI_GNU_PASCAL:
	case ABI_GNAT:
	case ABI_CLR:
	case ABI_CRYSTAL:
	case ABI_APPLE_OBJC1:
	case ABI_APPLE_OBJC2:
	case ABI_COREFOUNDATION:
	case ABI_BORLAND:
	case ABI_WATCOM:
	case ABI_GNU_OLD:
	case ABI_ECERE:
	case ABI_ZIG:
		/* These don't have demanglers yet, return as-is */
		return strdup(mangled_name);

	default:
		return strdup(mangled_name);
	}
}

/*
 * Universal demangler - auto-detect and demangle
 */
char *
demangle_universal(const char *mangled_name)
{
	abi_kind_t abi;

	if (!mangled_name)
		return NULL;

	abi = demangle_detect_abi(mangled_name);

	if (abi == ABI_AUTO)
		return strdup(mangled_name);

	return demangle_with_abi(mangled_name, abi);
}

/*
 * Check if string looks like a mangled name
 */
int
demangle_is_mangled(const char *name)
{
	if (!name || !*name)
		return 0;

	/* Check for common mangling prefixes */
	if (name[0] == '_' && name[1] == 'Z')  /* Itanium */
		return 1;
	if (name[0] == '?')  /* MSVC/DMC */
		return 1;
	if (name[0] == '_' && name[1] == 'D')  /* D */
		return 1;
	if (name[0] == '$' && name[1] == 's')  /* Swift */
		return 1;
	if ((name[0] == '-' || name[0] == '+') && name[1] == '[')  /* ObjC */
		return 1;
	if (strncmp(name, "Java_", 5) == 0)  /* Java */
		return 1;
	if (name[0] == '@')  /* Borland */
		return 1;

	return 0;
}

/*
 * Demangle filter - scan text and demangle all symbols
 */
char *
demangle_filter_text(const char *text)
{
	char *result;
	size_t result_size = strlen(text) * 2;  /* Estimate */
	size_t result_pos = 0;
	const char *p = text;

	result = malloc(result_size);
	if (!result)
		return NULL;

	while (*p) {
		/* Look for potential mangled symbol */
		if (demangle_is_mangled(p)) {
			/* Extract symbol (until whitespace or special char) */
			const char *end = p;
			while (*end && !isspace(*end) && *end != ',' &&
			       *end != ')' && *end != ';')
				end++;

			size_t symbol_len = end - p;
			char *symbol = malloc(symbol_len + 1);
			memcpy(symbol, p, symbol_len);
			symbol[symbol_len] = '\0';

			/* Demangle it */
			char *demangled = demangle_universal(symbol);
			free(symbol);

			/* Append to result */
			size_t dem_len = strlen(demangled);
			if (result_pos + dem_len >= result_size) {
				result_size *= 2;
				result = realloc(result, result_size);
			}
			strcpy(result + result_pos, demangled);
			result_pos += dem_len;
			free(demangled);

			p = end;
		} else {
			/* Copy character as-is */
			if (result_pos + 1 >= result_size) {
				result_size *= 2;
				result = realloc(result, result_size);
			}
			result[result_pos++] = *p++;
		}
	}

	result[result_pos] = '\0';
	return result;
}

/*
 * Get ABI name as string
 */
const char *
demangle_abi_name(abi_kind_t abi)
{
	switch (abi) {
	case ABI_ITANIUM: return "Itanium C++";
	case ABI_MSVC: return "Microsoft Visual C++";
	case ABI_WATCOM: return "Watcom C++";
	case ABI_ARM: return "ARM C++";
	case ABI_BORLAND: return "Borland C++";
	case ABI_GNU_OLD: return "GNU C++ (old)";
	case ABI_DMC: return "Digital Mars C++";
	case ABI_CFRONT: return "AT&T Cfront C++";
	case ABI_EDG: return "Edison Design Group C++";
	case ABI_APPLE_OBJC1: return "Apple Objective-C 1.0";
	case ABI_APPLE_OBJC2: return "Apple Objective-C 2.0";
	case ABI_GNU_OBJC: return "GNU Objective-C";
	case ABI_COREFOUNDATION: return "CoreFoundation";
	case ABI_DLANG: return "D Language";
	case ABI_SWIFT: return "Swift";
	case ABI_RUST: return "Rust";
	case ABI_GO: return "Go";
	case ABI_ZIG: return "Zig";
	case ABI_CRYSTAL: return "Crystal";
	case ABI_NIM: return "Nim";
	case ABI_VLANG: return "V Language";
	case ABI_JULIA: return "Julia";
	case ABI_JAVA: return "Java";
	case ABI_CLR: return ".NET CLR";
	case ABI_DART: return "Dart";
	case ABI_KOTLIN: return "Kotlin/Native";
	case ABI_GHC: return "Glasgow Haskell Compiler (GHC)";
	case ABI_OCAML: return "OCaml";
	case ABI_FSHARP: return "F#";
	case ABI_FREEPASCAL: return "FreePascal";
	case ABI_GNU_PASCAL: return "GNU Pascal";
	case ABI_GFORTRAN: return "GNU Fortran (gfortran)";
	case ABI_IFORT: return "Intel Fortran (ifort)";
	case ABI_NAG_FORTRAN: return "NAG Fortran";
	case ABI_GNAT: return "GNAT Ada";
	case ABI_ECERE: return "eCere SDK";
	case ABI_LLVM_IR: return "LLVM IR";
	case ABI_WASM: return "WebAssembly";
	default: return "Unknown";
	}
}
