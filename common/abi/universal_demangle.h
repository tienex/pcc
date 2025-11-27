/*
 * Copyright (c) 2025 PCC Project
 *
 * Universal Demangler
 *
 * Automatically detects ABI from mangled name and applies correct demangler.
 *
 * Supported ABIs:
 * - Itanium C++ (_Z prefix)
 * - MSVC C++ (? prefix)
 * - Digital Mars (? prefix with specific format)
 * - GNU Old (__<length><name>)
 * - D Language (_D prefix)
 * - Swift ($s or _T prefix)
 * - Rust (_ZN with specific format)
 * - Java (Java_ prefix)
 * - Objective-C (-[Class method] or +[Class method])
 * - Go (go. prefix)
 * - And more...
 */

#ifndef UNIVERSAL_DEMANGLE_H
#define UNIVERSAL_DEMANGLE_H

#include "abi.h"

/*
 * Auto-detect ABI from mangled name
 */
abi_kind_t demangle_detect_abi(const char *mangled_name);

/*
 * Universal demangler - auto-detects and demangles
 */
char *demangle_universal(const char *mangled_name);

/*
 * Demangle with specific ABI (no auto-detection)
 */
char *demangle_with_abi(const char *mangled_name, abi_kind_t abi);

/*
 * Check if string is a mangled name
 */
int demangle_is_mangled(const char *name);

/*
 * Demangle filter for text streams
 * Scans text and demangles all mangled symbols found
 */
char *demangle_filter_text(const char *text);

/*
 * Get human-readable ABI name
 */
const char *demangle_abi_name(abi_kind_t abi);

#endif /* UNIVERSAL_DEMANGLE_H */
