/*
 * Copyright (c) 2025 PCC Project
 * BGI Driver Management
 */

#include "bgi_driver.h"
#include <stdlib.h>
#include <string.h>

#define MAX_GRAPHICS_DRIVERS 16
#define MAX_FONT_DRIVERS 16

/* Driver registries */
static struct {
	const bgi_graphics_driver_t *graphics_drivers[MAX_GRAPHICS_DRIVERS];
	int num_graphics_drivers;

	const bgi_font_driver_t *font_drivers[MAX_FONT_DRIVERS];
	int num_font_drivers;

	/* Current drivers */
	const bgi_graphics_driver_t *current_graphics;
	const bgi_font_driver_t *current_font;
} driver_state = {0};

/*
 * Registration
 */

int bgi_register_graphics_driver(const bgi_graphics_driver_t *driver) {
	if (!driver || driver_state.num_graphics_drivers >= MAX_GRAPHICS_DRIVERS) {
		return -1;
	}

	driver_state.graphics_drivers[driver_state.num_graphics_drivers++] = driver;
	return 0;
}

int bgi_register_font_driver(const bgi_font_driver_t *driver) {
	if (!driver || driver_state.num_font_drivers >= MAX_FONT_DRIVERS) {
		return -1;
	}

	driver_state.font_drivers[driver_state.num_font_drivers++] = driver;
	return 0;
}

/*
 * Loading
 */

bgi_graphics_driver_t *bgi_load_graphics_driver(const char *name) {
	if (!name) return NULL;

	for (int i = 0; i < driver_state.num_graphics_drivers; i++) {
		if (strcmp(driver_state.graphics_drivers[i]->name, name) == 0) {
			return (bgi_graphics_driver_t *)driver_state.graphics_drivers[i];
		}
	}

	return NULL;
}

bgi_font_driver_t *bgi_load_font_driver(const char *name) {
	if (!name) return NULL;

	for (int i = 0; i < driver_state.num_font_drivers; i++) {
		if (strcmp(driver_state.font_drivers[i]->name, name) == 0) {
			return (bgi_font_driver_t *)driver_state.font_drivers[i];
		}
	}

	return NULL;
}

/*
 * Query
 */

int bgi_get_num_graphics_drivers(void) {
	return driver_state.num_graphics_drivers;
}

int bgi_get_num_font_drivers(void) {
	return driver_state.num_font_drivers;
}

const char *bgi_get_graphics_driver_name(int index) {
	if (index < 0 || index >= driver_state.num_graphics_drivers) {
		return NULL;
	}
	return driver_state.graphics_drivers[index]->name;
}

const char *bgi_get_font_driver_name(int index) {
	if (index < 0 || index >= driver_state.num_font_drivers) {
		return NULL;
	}
	return driver_state.font_drivers[index]->name;
}

/*
 * Auto-registration of built-in drivers
 */

static void __attribute__((constructor)) register_builtin_drivers(void) {
	/* Register graphics drivers */
#if defined(__DOS__) || defined(__MSDOS__)
	bgi_register_graphics_driver(&bgi_vga_driver);
	bgi_register_graphics_driver(&bgi_vesa_driver);
#elif defined(_WIN32)
	bgi_register_graphics_driver(&bgi_gdi_driver);
#else
	bgi_register_graphics_driver(&bgi_x11_driver);
#endif

	/* Register font drivers */
	bgi_register_font_driver(&bgi_font_8x8);
	bgi_register_font_driver(&bgi_font_8x16);
	bgi_register_font_driver(&bgi_font_triplex);
	bgi_register_font_driver(&bgi_font_small);
	bgi_register_font_driver(&bgi_font_sansserif);
	bgi_register_font_driver(&bgi_font_gothic);
}
