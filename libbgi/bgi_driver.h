/*
 * Copyright (c) 2025 PCC Project
 * BGI Driver Architecture
 * Loadable font and graphics drivers
 */

#ifndef BGI_DRIVER_H
#define BGI_DRIVER_H

#include "bgi.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Driver types */
typedef enum {
	BGI_DRIVER_GRAPHICS,
	BGI_DRIVER_FONT
} bgi_driver_type_t;

/* Graphics driver interface */
typedef struct bgi_graphics_driver {
	const char *name;
	int id;

	/* Initialization */
	int (*detect)(void);
	int (*init)(int mode, int *width, int *height, int *colors);
	void (*shutdown)(void);

	/* Drawing primitives */
	void (*putpixel)(int x, int y, int color);
	int (*getpixel)(int x, int y);
	void (*line)(int x1, int y1, int x2, int y2);
	void (*hline)(int x1, int x2, int y);
	void (*vline)(int x, int y1, int y2);
	void (*rectangle)(int x1, int y1, int x2, int y2);
	void (*bar)(int x1, int y1, int x2, int y2);
	void (*circle)(int x, int y, int radius);
	void (*ellipse)(int x, int y, int rx, int ry);
	void (*arc)(int x, int y, int start, int end, int radius);

	/* Filling */
	void (*floodfill)(int x, int y, int border);

	/* Palette */
	void (*setpalette)(int index, int r, int g, int b);
	void (*getpalette)(int index, int *r, int *g, int *b);

	/* Mode info */
	int (*getmodeinfo)(int mode, int *width, int *height, int *colors);

	/* Page flipping */
	void (*setvisualpage)(int page);
	void (*setactivepage)(int page);

} bgi_graphics_driver_t;

/* Font driver interface */
typedef struct bgi_font_driver {
	const char *name;
	int id;

	/* Font info */
	int (*get_width)(void);
	int (*get_height)(void);
	int (*get_baseline)(void);

	/* Character rendering */
	void (*render_char)(char ch, int x, int y, int color);
	void (*render_string)(const char *str, int x, int y, int color);

	/* Metrics */
	int (*char_width)(char ch);
	int (*string_width)(const char *str);

	/* Scaling */
	void (*set_scale)(int scale);
	int (*get_scale)(void);

} bgi_font_driver_t;

/* Driver registration */
int bgi_register_graphics_driver(const bgi_graphics_driver_t *driver);
int bgi_register_font_driver(const bgi_font_driver_t *driver);

/* Driver loading */
bgi_graphics_driver_t *bgi_load_graphics_driver(const char *name);
bgi_font_driver_t *bgi_load_font_driver(const char *name);

/* Driver query */
int bgi_get_num_graphics_drivers(void);
int bgi_get_num_font_drivers(void);
const char *bgi_get_graphics_driver_name(int index);
const char *bgi_get_font_driver_name(int index);

/* Built-in drivers */
extern bgi_graphics_driver_t bgi_vga_driver;
extern bgi_graphics_driver_t bgi_vesa_driver;
extern bgi_graphics_driver_t bgi_gdi_driver;
extern bgi_graphics_driver_t bgi_x11_driver;

extern bgi_font_driver_t bgi_font_8x8;
extern bgi_font_driver_t bgi_font_8x16;
extern bgi_font_driver_t bgi_font_triplex;
extern bgi_font_driver_t bgi_font_small;
extern bgi_font_driver_t bgi_font_sansserif;
extern bgi_font_driver_t bgi_font_gothic;

#ifdef __cplusplus
}
#endif

#endif /* BGI_DRIVER_H */
