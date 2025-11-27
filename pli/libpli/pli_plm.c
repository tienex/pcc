/*
 * Copyright (c) 2025 PCC PL/I Runtime Library
 *
 * PL/M-specific functions
 */

#include "pli_runtime.h"
#include <stdlib.h>
#include <string.h>

/* Note: I/O port functions are architecture-specific
 * These are stub implementations that can be replaced
 * with inline assembly or platform-specific code
 */

#ifdef __i386__
/* x86 specific implementations */

uint8_t pli_input(uint16_t port) {
	uint8_t value;
	__asm__ volatile ("inb %1, %0" : "=a"(value) : "d"(port));
	return value;
}

void pli_output(uint16_t port, uint8_t value) {
	__asm__ volatile ("outb %0, %1" : : "a"(value), "d"(port));
}

#elif defined(__x86_64__)
/* x86-64 specific implementations */

uint8_t pli_input(uint16_t port) {
	uint8_t value;
	__asm__ volatile ("inb %1, %0" : "=a"(value) : "d"(port));
	return value;
}

void pli_output(uint16_t port, uint8_t value) {
	__asm__ volatile ("outb %0, %1" : : "a"(value), "d"(port));
}

#else
/* Generic stub implementations for non-x86 architectures */

uint8_t pli_input(uint16_t port) {
	/* Stub - return 0 or implement platform-specific code */
	(void)port;
	return 0;
}

void pli_output(uint16_t port, uint8_t value) {
	/* Stub - do nothing or implement platform-specific code */
	(void)port;
	(void)value;
}

#endif

/* Bit manipulation functions */

/* SHL - Shift left */
uint8_t pli_shl(uint8_t value, uint8_t count) {
	if (count >= 8) return 0;
	return value << count;
}

/* SHR - Shift right */
uint8_t pli_shr(uint8_t value, uint8_t count) {
	if (count >= 8) return 0;
	return value >> count;
}

/* ROL - Rotate left */
uint8_t pli_rol(uint8_t value, uint8_t count) {
	count &= 7;  /* Mod 8 */
	return (value << count) | (value >> (8 - count));
}

/* ROR - Rotate right */
uint8_t pli_ror(uint8_t value, uint8_t count) {
	count &= 7;  /* Mod 8 */
	return (value >> count) | (value << (8 - count));
}

/* HIGH - Get high byte of word */
uint8_t pli_high_byte(uint16_t value) {
	return (uint8_t)(value >> 8);
}

/* LOW - Get low byte of word */
uint8_t pli_low_byte(uint16_t value) {
	return (uint8_t)(value & 0xFF);
}

/* DOUBLE - Combine two bytes into word */
uint16_t pli_double(uint8_t low, uint8_t high) {
	return ((uint16_t)high << 8) | low;
}

/* LAST - Get last index of array (PL/M uses 0-based indexing) */
uint8_t pli_last(void *array) {
	/* This is a compile-time function in PL/M
	 * Runtime implementation would need array descriptor
	 * Return 0 as placeholder
	 */
	(void)array;
	return 0;
}

/* MOVE - Block memory move */
void pli_move(void *dest, const void *src, size_t count) {
	if (!dest || !src) return;
	memmove(dest, src, count);
}
