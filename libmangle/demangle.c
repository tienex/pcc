/*
 * Copyright (c) 2025 PCC Project
 * Symbol demangling implementation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "mangle.h"

/* Forward declarations */
static demangle_info_t *demangle_cxx_itanium(const char *mangled);
static demangle_info_t *demangle_cxx_msvc(const char *mangled);
static demangle_info_t *demangle_pcc(const char *mangled);
static demangle_info_t *demangle_simple(const char *mangled, mangle_scheme_t scheme);

demangle_info_t *demangle_symbol(const char *mangled_name) {
	if (!mangled_name || !*mangled_name) {
		return NULL;
	}

	mangle_scheme_t scheme = detect_mangle_scheme(mangled_name);
	return demangle_symbol_scheme(mangled_name, scheme);
}

demangle_info_t *demangle_symbol_scheme(const char *mangled_name,
                                         mangle_scheme_t scheme) {
	if (!mangled_name) {
		return NULL;
	}

	switch (scheme) {
	case MANGLE_CXX_ITANIUM:
		return demangle_cxx_itanium(mangled_name);

	case MANGLE_CXX_MSVC:
		return demangle_cxx_msvc(mangled_name);

	case MANGLE_PCC:
		return demangle_pcc(mangled_name);

	case MANGLE_PASCAL:
	case MANGLE_DELPHI:
	case MANGLE_MODULA2:
	case MANGLE_MODULA3:
	case MANGLE_OBERON:
	case MANGLE_ADA:
		return demangle_simple(mangled_name, scheme);

	case MANGLE_NONE:
	case MANGLE_C:
	default:
		/* C symbols are not mangled */
		return demangle_simple(mangled_name, MANGLE_C);
	}
}

static demangle_info_t *demangle_simple(const char *mangled, mangle_scheme_t scheme) {
	demangle_info_t *info = (demangle_info_t *)calloc(1, sizeof(demangle_info_t));
	if (!info) {
		return NULL;
	}

	info->detected_scheme = scheme;
	info->sym_type = SYM_FUNCTION;  /* Unknown */
	info->param_types = NULL;
	info->param_count = 0;

	/* Parse based on scheme */
	switch (scheme) {
	case MANGLE_DELPHI:
	case MANGLE_OBERON: {
		/* Format: Module.Name */
		const char *dot = strchr(mangled, '.');
		if (dot) {
			size_t scope_len = dot - mangled;
			info->scope = (char *)malloc(scope_len + 1);
			if (info->scope) {
				memcpy(info->scope, mangled, scope_len);
				info->scope[scope_len] = '\0';
			}
			info->name = strdup(dot + 1);
		} else {
			info->name = strdup(mangled);
		}
		break;
	}

	case MANGLE_MODULA2: {
		/* Format: module_name */
		const char *underscore = strchr(mangled, '_');
		if (underscore) {
			size_t scope_len = underscore - mangled;
			info->scope = (char *)malloc(scope_len + 1);
			if (info->scope) {
				memcpy(info->scope, mangled, scope_len);
				info->scope[scope_len] = '\0';
			}
			info->name = strdup(underscore + 1);
		} else {
			info->name = strdup(mangled);
		}
		break;
	}

	case MANGLE_MODULA3:
	case MANGLE_ADA: {
		/* Format: module__name */
		const char *dunder = strstr(mangled, "__");
		if (dunder) {
			size_t scope_len = dunder - mangled;
			info->scope = (char *)malloc(scope_len + 1);
			if (info->scope) {
				memcpy(info->scope, mangled, scope_len);
				info->scope[scope_len] = '\0';
			}
			info->name = strdup(dunder + 2);
		} else {
			info->name = strdup(mangled);
		}
		break;
	}

	case MANGLE_PASCAL:
	default:
		/* Simple name */
		info->name = strdup(mangled);
		break;
	}

	/* Build full name */
	if (info->scope) {
		size_t full_len = strlen(info->scope) + strlen(info->name) + 3;
		info->full_name = (char *)malloc(full_len);
		if (info->full_name) {
			if (scheme == MANGLE_DELPHI || scheme == MANGLE_OBERON) {
				snprintf(info->full_name, full_len, "%s.%s", info->scope, info->name);
			} else if (scheme == MANGLE_MODULA3 || scheme == MANGLE_ADA) {
				snprintf(info->full_name, full_len, "%s::%s", info->scope, info->name);
			} else {
				snprintf(info->full_name, full_len, "%s_%s", info->scope, info->name);
			}
		}
	} else {
		info->full_name = strdup(info->name);
	}

	return info;
}

static demangle_info_t *demangle_cxx_itanium(const char *mangled) {
	/* Simplified C++ Itanium demangling */
	demangle_info_t *info = (demangle_info_t *)calloc(1, sizeof(demangle_info_t));
	if (!info) {
		return NULL;
	}

	info->detected_scheme = MANGLE_CXX_ITANIUM;

	/* Skip _Z prefix */
	const char *p = mangled;
	if (p[0] == '_' && p[1] == 'Z') {
		p += 2;
	}

	/* Check for nested name */
	if (*p == 'N') {
		p++;

		/* Parse nested scopes */
		char scope_buf[512] = "";
		int first = 1;

		while (*p && *p != 'E') {
			if (isdigit(*p)) {
				/* Length-prefixed identifier */
				int len = 0;
				while (isdigit(*p)) {
					len = len * 10 + (*p - '0');
					p++;
				}

				if (len > 0 && *p) {
					if (!first) {
						strcat(scope_buf, "::");
					}
					strncat(scope_buf, p, len);
					p += len;
					first = 0;
				}
			} else {
				p++;
			}
		}

		if (strlen(scope_buf) > 0) {
			/* Last component is the name */
			char *last_sep = strstr(scope_buf, "::");
			if (last_sep && last_sep != scope_buf) {
				/* Find the rightmost :: */
				char *prev_sep = scope_buf;
				char *next_sep;
				while ((next_sep = strstr(prev_sep + 2, "::")) != NULL) {
					prev_sep = next_sep;
				}
				last_sep = prev_sep;

				*last_sep = '\0';
				info->scope = strdup(scope_buf);
				info->name = strdup(last_sep + 2);
			} else {
				info->name = strdup(scope_buf);
			}
		}
	} else if (isdigit(*p)) {
		/* Simple name with length prefix */
		int len = 0;
		while (isdigit(*p)) {
			len = len * 10 + (*p - '0');
			p++;
		}

		if (len > 0) {
			info->name = (char *)malloc(len + 1);
			if (info->name) {
				memcpy(info->name, p, len);
				info->name[len] = '\0';
			}
		}
	}

	/* Build full name */
	if (info->scope && info->name) {
		size_t full_len = strlen(info->scope) + strlen(info->name) + 3;
		info->full_name = (char *)malloc(full_len);
		if (info->full_name) {
			snprintf(info->full_name, full_len, "%s::%s", info->scope, info->name);
		}
	} else if (info->name) {
		info->full_name = strdup(info->name);
	} else {
		info->full_name = strdup(mangled);
	}

	return info;
}

static demangle_info_t *demangle_cxx_msvc(const char *mangled) {
	/* Simplified MSVC demangling */
	demangle_info_t *info = (demangle_info_t *)calloc(1, sizeof(demangle_info_t));
	if (!info) {
		return NULL;
	}

	info->detected_scheme = MANGLE_CXX_MSVC;

	/* Skip ? prefix */
	const char *p = mangled;
	if (*p == '?') {
		p++;
	}

	/* Extract name (up to first @) */
	const char *at = strchr(p, '@');
	if (at) {
		size_t name_len = at - p;
		info->name = (char *)malloc(name_len + 1);
		if (info->name) {
			memcpy(info->name, p, name_len);
			info->name[name_len] = '\0';
		}

		/* Extract scope (between first and second @) */
		p = at + 1;
		at = strchr(p, '@');
		if (at && at != p) {
			size_t scope_len = at - p;
			info->scope = (char *)malloc(scope_len + 1);
			if (info->scope) {
				memcpy(info->scope, p, scope_len);
				info->scope[scope_len] = '\0';
			}
		}
	} else {
		info->name = strdup(p);
	}

	/* Build full name */
	if (info->scope && info->name) {
		size_t full_len = strlen(info->scope) + strlen(info->name) + 3;
		info->full_name = (char *)malloc(full_len);
		if (info->full_name) {
			snprintf(info->full_name, full_len, "%s::%s", info->scope, info->name);
		}
	} else if (info->name) {
		info->full_name = strdup(info->name);
	} else {
		info->full_name = strdup(mangled);
	}

	return info;
}

static demangle_info_t *demangle_pcc(const char *mangled) {
	demangle_info_t *info = (demangle_info_t *)calloc(1, sizeof(demangle_info_t));
	if (!info) {
		return NULL;
	}

	info->detected_scheme = MANGLE_PCC;

	/* Skip _P_ prefix */
	const char *p = mangled;
	if (strncmp(p, "_P_", 3) == 0) {
		p += 3;
	}

	/* Skip language code */
	if (*p) {
		p++;
	}

	/* Parse scope length and scope */
	if (isdigit(*p)) {
		size_t scope_len = 0;
		while (isdigit(*p)) {
			scope_len = scope_len * 10 + (*p - '0');
			p++;
		}

		if (scope_len > 0) {
			info->scope = (char *)malloc(scope_len + 1);
			if (info->scope) {
				memcpy(info->scope, p, scope_len);
				info->scope[scope_len] = '\0';
				p += scope_len;
			}
		}
	}

	/* Parse name length and name */
	if (isdigit(*p)) {
		size_t name_len = 0;
		while (isdigit(*p)) {
			name_len = name_len * 10 + (*p - '0');
			p++;
		}

		if (name_len > 0) {
			info->name = (char *)malloc(name_len + 1);
			if (info->name) {
				memcpy(info->name, p, name_len);
				info->name[name_len] = '\0';
			}
		}
	}

	/* Build full name */
	if (info->scope && info->name) {
		size_t full_len = strlen(info->scope) + strlen(info->name) + 3;
		info->full_name = (char *)malloc(full_len);
		if (info->full_name) {
			snprintf(info->full_name, full_len, "%s::%s", info->scope, info->name);
		}
	} else if (info->name) {
		info->full_name = strdup(info->name);
	} else {
		info->full_name = strdup(mangled);
	}

	return info;
}

char *demangle_name_only(const char *mangled_name) {
	demangle_info_t *info = demangle_symbol(mangled_name);
	if (!info) {
		return strdup(mangled_name);
	}

	char *name = info->name ? strdup(info->name) : strdup(mangled_name);
	demangle_info_free(info);

	return name;
}
