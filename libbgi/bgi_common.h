/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Common Helper Functions
 *
 * Shared implementations for all BGI backends
 */

#ifndef _BGI_COMMON_H
#define _BGI_COMMON_H

#include "graphics.h"

/* Common state that all backends should maintain */
struct bgi_common_state {
	int width, height;
	int current_x, current_y;
	int current_color, current_bkcolor;
	int line_style;
	unsigned short line_pattern;
	int line_thickness;
	int fill_style, fill_color;
	unsigned char fill_pattern[8];
	int text_justify_h, text_justify_v;
	int text_direction;
	int text_font, text_charsize;
	struct viewporttype viewport;
	struct arccoordstype last_arc;
	int write_mode;
	int visual_page, active_page;
};

/* Backend capability flags */
#define BGI_CAP_HARDWARE_LINE      (1 << 0)
#define BGI_CAP_HARDWARE_RECT      (1 << 1)
#define BGI_CAP_HARDWARE_CIRCLE    (1 << 2)
#define BGI_CAP_HARDWARE_ARC       (1 << 3)
#define BGI_CAP_HARDWARE_ELLIPSE   (1 << 4)
#define BGI_CAP_HARDWARE_FILL      (1 << 5)
#define BGI_CAP_HARDWARE_TEXT      (1 << 6)
#define BGI_CAP_HARDWARE_PATTERN   (1 << 7)
#define BGI_CAP_PALETTE_CHANGE     (1 << 8)
#define BGI_CAP_MULTIPLE_PAGES     (1 << 9)

/* Function pointers for backend-specific operations */
struct bgi_backend_ops {
	/* Required: pixel drawing */
	void (*putpixel)(int x, int y, int color);
	unsigned int (*getpixel)(int x, int y);

	/* Optional: hardware-accelerated primitives */
	void (*hw_line)(int x1, int y1, int x2, int y2);
	void (*hw_rect)(int left, int top, int right, int bottom);
	void (*hw_filled_rect)(int left, int top, int right, int bottom);
	void (*hw_circle)(int x, int y, int radius);
	void (*hw_filled_circle)(int x, int y, int radius);
	void (*hw_arc)(int x, int y, int stangle, int endangle, int radius);
	void (*hw_ellipse)(int x, int y, int stangle, int endangle, int xr, int yr);

	/* Optional: pattern fill support */
	void (*hw_pattern_fill)(int left, int top, int right, int bottom);

	/* Optional: text rendering */
	void (*hw_text)(int x, int y, const char *text);

	/* Optional: palette operations */
	void (*set_palette_color)(int index, int r, int g, int b);

	/* Optional: page flipping */
	void (*set_visual_page)(int page);
	void (*set_active_page)(int page);

	/* Capabilities */
	unsigned int caps;
};

/* Bresenham line algorithm (software fallback) */
void bgi_draw_line_bresenham(int x1, int y1, int x2, int y2,
                              void (*putpixel)(int, int, int),
                              int color, unsigned short pattern);

/* Bresenham circle algorithm (software fallback) */
void bgi_draw_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color);

/* Filled circle (software fallback) */
void bgi_fill_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color);

/* Ellipse drawing */
void bgi_draw_ellipse(int x, int y, int stangle, int endangle,
                      int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color);

/* Filled ellipse */
void bgi_fill_ellipse(int x, int y, int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color);

/* Arc drawing */
void bgi_draw_arc(int x, int y, int stangle, int endangle, int radius,
                  void (*putpixel)(int, int, int),
                  int color, struct arccoordstype *coords);

/* Polygon drawing */
void bgi_draw_polygon(int numpoints, const int *polypoints,
                      void (*line_func)(int, int, int, int));

/* Filled polygon (scanline algorithm) */
void bgi_fill_polygon(int numpoints, const int *polypoints,
                      void (*putpixel)(int, int, int),
                      int color, int width, int height);

/* Flood fill (boundary fill algorithm) */
void bgi_flood_fill(int x, int y, int border,
                    unsigned int (*getpixel)(int, int),
                    void (*putpixel)(int, int, int),
                    int fill_color, int width, int height);

/* Pattern fill for rectangle */
void bgi_pattern_fill_rect(int left, int top, int right, int bottom,
                            void (*putpixel)(int, int, int),
                            const unsigned char *pattern, int color);

/* Pattern definitions for standard fill styles */
extern const unsigned char bgi_fill_patterns[12][8];

/* Simple bitmap text rendering (8x8 font) */
void bgi_draw_text_simple(int x, int y, const char *text,
                          void (*putpixel)(int, int, int),
                          int color, int horiz, int vert);

/* Text metrics for simple font */
int bgi_text_width_simple(const char *text);
int bgi_text_height_simple(void);

/* Utility functions */
int bgi_abs(int x);
int bgi_min(int a, int b);
int bgi_max(int a, int b);
void bgi_swap(int *a, int *b);

/* Angle conversion (degrees to coordinates) */
void bgi_angle_to_point(int angle, int radius, int *x, int *y);

#endif /* _BGI_COMMON_H */
