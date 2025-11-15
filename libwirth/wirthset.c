/*
 * Copyright (c) 2025 PCC Wirth Languages Runtime Library
 *
 * Set Operations
 * - 32-bit sets (for sets of 0..31)
 * - 64-bit sets (for sets of 0..63)
 * - 256-element sets (for sets of 0..255, CHAR sets)
 * Supports Pascal, Modula-2, and Oberon set operations
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "wirthrt.h"

/*
 * 32-bit Set Operations
 */

WirthSet32 wirth_set32_empty(void) {
	return 0;
}

WirthSet32 wirth_set32_singleton(int elem) {
	if (elem < 0 || elem >= 32) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set element out of range [0..31]");
		return 0;
	}
	return (WirthSet32)1 << elem;
}

WirthSet32 wirth_set32_range(int low, int high) {
	if (low < 0 || low >= 32 || high < 0 || high >= 32) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set range out of bounds [0..31]");
		return 0;
	}

	if (low > high) {
		return 0;  /* Empty set */
	}

	/* Create mask for range [low..high] */
	WirthSet32 result = 0;
	for (int i = low; i <= high; i++) {
		result |= (WirthSet32)1 << i;
	}
	return result;
}

WirthSet32 wirth_set32_union(WirthSet32 a, WirthSet32 b) {
	return a | b;
}

WirthSet32 wirth_set32_intersection(WirthSet32 a, WirthSet32 b) {
	return a & b;
}

WirthSet32 wirth_set32_difference(WirthSet32 a, WirthSet32 b) {
	return a & ~b;
}

WirthSet32 wirth_set32_sym_diff(WirthSet32 a, WirthSet32 b) {
	return a ^ b;
}

int wirth_set32_contains(WirthSet32 s, int elem) {
	if (elem < 0 || elem >= 32) {
		return 0;
	}
	return (s & ((WirthSet32)1 << elem)) != 0;
}

int wirth_set32_equal(WirthSet32 a, WirthSet32 b) {
	return a == b;
}

int wirth_set32_subset(WirthSet32 a, WirthSet32 b) {
	/* a is subset of b if (a AND b) = a */
	return (a & b) == a;
}

void wirth_set32_incl(WirthSet32 *s, int elem) {
	if (!s) {
		return;
	}
	if (elem < 0 || elem >= 32) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set element out of range [0..31]");
		return;
	}
	*s |= (WirthSet32)1 << elem;
}

void wirth_set32_excl(WirthSet32 *s, int elem) {
	if (!s) {
		return;
	}
	if (elem < 0 || elem >= 32) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set element out of range [0..31]");
		return;
	}
	*s &= ~((WirthSet32)1 << elem);
}

/*
 * 256-element Set Operations (for CHAR sets and larger ranges)
 */

void wirth_set256_init(WirthSet256 s) {
	if (s) {
		memset(s, 0, sizeof(WirthSet256));
	}
}

void wirth_set256_incl(WirthSet256 s, int elem) {
	if (!s) {
		return;
	}
	if (elem < 0 || elem >= 256) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set element out of range [0..255]");
		return;
	}

	int word = elem / 32;
	int bit = elem % 32;
	s[word] |= (uint32_t)1 << bit;
}

void wirth_set256_excl(WirthSet256 s, int elem) {
	if (!s) {
		return;
	}
	if (elem < 0 || elem >= 256) {
		wirth_runtime_error(WIRTH_ERR_RANGE_CHECK, "Set element out of range [0..255]");
		return;
	}

	int word = elem / 32;
	int bit = elem % 32;
	s[word] &= ~((uint32_t)1 << bit);
}

int wirth_set256_contains(WirthSet256 s, int elem) {
	if (!s) {
		return 0;
	}
	if (elem < 0 || elem >= 256) {
		return 0;
	}

	int word = elem / 32;
	int bit = elem % 32;
	return (s[word] & ((uint32_t)1 << bit)) != 0;
}
