/*
 * Copyright (c) 2025 PCC CHILL Runtime Library
 *
 * CHILL POWERSET (set) operations
 */

#include "chill.h"
#include <stdlib.h>
#include <string.h>

/*
 * Calculate number of 64-bit words needed for given max element
 */
static size_t
calc_words(size_t max_elem)
{
	return (max_elem + 63) / 64;
}

/*
 * Powerset creation and management
 */

chill_powerset_t *
chill_powerset_create(size_t max_elem)
{
	chill_powerset_t *set;
	size_t num_words;

	if (max_elem == 0) {
		max_elem = 1;
	}

	num_words = calc_words(max_elem);

	set = (chill_powerset_t *)malloc(sizeof(chill_powerset_t));
	if (set == NULL) {
		chill_raise(CHILL_EXC_OUTOFMEM, "Failed to create powerset");
		return NULL;
	}

	set->bits = (uint64_t *)calloc(num_words, sizeof(uint64_t));
	if (set->bits == NULL) {
		free(set);
		chill_raise(CHILL_EXC_OUTOFMEM, "Failed to allocate powerset bits");
		return NULL;
	}

	set->size = max_elem;

	return set;
}

void
chill_powerset_free(chill_powerset_t *set)
{
	if (set != NULL) {
		if (set->bits != NULL) {
			free(set->bits);
		}
		free(set);
	}
}

/*
 * Powerset element operations
 */

void
chill_powerset_add(chill_powerset_t *set, chill_int_t elem)
{
	size_t word_idx, bit_idx;

	if (set == NULL) {
		return;
	}

	if (elem < 0 || (size_t)elem >= set->size) {
		chill_raise(CHILL_EXC_RANGEFAIL, "Powerset element out of range");
		return;
	}

	word_idx = elem / 64;
	bit_idx = elem % 64;
	set->bits[word_idx] |= (1ULL << bit_idx);
}

void
chill_powerset_remove(chill_powerset_t *set, chill_int_t elem)
{
	size_t word_idx, bit_idx;

	if (set == NULL) {
		return;
	}

	if (elem < 0 || (size_t)elem >= set->size) {
		chill_raise(CHILL_EXC_RANGEFAIL, "Powerset element out of range");
		return;
	}

	word_idx = elem / 64;
	bit_idx = elem % 64;
	set->bits[word_idx] &= ~(1ULL << bit_idx);
}

chill_bool_t
chill_powerset_contains(const chill_powerset_t *set, chill_int_t elem)
{
	size_t word_idx, bit_idx;

	if (set == NULL) {
		return CHILL_FALSE;
	}

	if (elem < 0 || (size_t)elem >= set->size) {
		return CHILL_FALSE;
	}

	word_idx = elem / 64;
	bit_idx = elem % 64;
	return (set->bits[word_idx] & (1ULL << bit_idx)) ? CHILL_TRUE : CHILL_FALSE;
}

/*
 * Powerset set operations
 */

chill_powerset_t *
chill_powerset_union(const chill_powerset_t *s1, const chill_powerset_t *s2)
{
	chill_powerset_t *result;
	size_t max_size, num_words, i;

	if (s1 == NULL && s2 == NULL) {
		return chill_powerset_create(1);
	}

	if (s1 == NULL) {
		max_size = s2->size;
	} else if (s2 == NULL) {
		max_size = s1->size;
	} else {
		max_size = (s1->size > s2->size) ? s1->size : s2->size;
	}

	result = chill_powerset_create(max_size);
	if (result == NULL) {
		return NULL;
	}

	num_words = calc_words(max_size);

	for (i = 0; i < num_words; i++) {
		uint64_t val1 = (s1 != NULL && i < calc_words(s1->size)) ? s1->bits[i] : 0;
		uint64_t val2 = (s2 != NULL && i < calc_words(s2->size)) ? s2->bits[i] : 0;
		result->bits[i] = val1 | val2;
	}

	return result;
}

chill_powerset_t *
chill_powerset_intersection(const chill_powerset_t *s1, const chill_powerset_t *s2)
{
	chill_powerset_t *result;
	size_t max_size, num_words, i;

	if (s1 == NULL || s2 == NULL) {
		return chill_powerset_create(1);
	}

	max_size = (s1->size > s2->size) ? s1->size : s2->size;
	result = chill_powerset_create(max_size);
	if (result == NULL) {
		return NULL;
	}

	num_words = calc_words(max_size);

	for (i = 0; i < num_words; i++) {
		uint64_t val1 = (i < calc_words(s1->size)) ? s1->bits[i] : 0;
		uint64_t val2 = (i < calc_words(s2->size)) ? s2->bits[i] : 0;
		result->bits[i] = val1 & val2;
	}

	return result;
}

chill_powerset_t *
chill_powerset_difference(const chill_powerset_t *s1, const chill_powerset_t *s2)
{
	chill_powerset_t *result;
	size_t num_words, i;

	if (s1 == NULL) {
		return chill_powerset_create(1);
	}

	result = chill_powerset_create(s1->size);
	if (result == NULL) {
		return NULL;
	}

	num_words = calc_words(s1->size);

	for (i = 0; i < num_words; i++) {
		uint64_t val1 = s1->bits[i];
		uint64_t val2 = (s2 != NULL && i < calc_words(s2->size)) ? s2->bits[i] : 0;
		result->bits[i] = val1 & ~val2;
	}

	return result;
}
