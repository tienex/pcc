/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Common Helper Functions Implementation
 */

#include "bgi_common.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* Standard fill patterns */
const unsigned char bgi_fill_patterns[12][8] = {
	/* EMPTY_FILL */
	{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
	/* SOLID_FILL */
	{0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
	/* LINE_FILL */
	{0xFF, 0xFF, 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00},
	/* LTSLASH_FILL */
	{0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80},
	/* SLASH_FILL */
	{0xE0, 0xC1, 0x83, 0x07, 0x0E, 0x1C, 0x38, 0x70},
	/* BKSLASH_FILL */
	{0xF0, 0x78, 0x3C, 0x1E, 0x0F, 0x87, 0xC3, 0xE1},
	/* LTBKSLASH_FILL */
	{0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x01},
	/* HATCH_FILL */
	{0xFF, 0x88, 0x88, 0x88, 0xFF, 0x88, 0x88, 0x88},
	/* XHATCH_FILL */
	{0x81, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x81},
	/* INTERLEAVE_FILL */
	{0xCC, 0x33, 0xCC, 0x33, 0xCC, 0x33, 0xCC, 0x33},
	/* WIDE_DOT_FILL */
	{0x80, 0x00, 0x08, 0x00, 0x80, 0x00, 0x08, 0x00},
	/* CLOSE_DOT_FILL */
	{0x88, 0x00, 0x22, 0x00, 0x88, 0x00, 0x22, 0x00}
};

/* Utility functions */
int bgi_abs(int x) {
	return x < 0 ? -x : x;
}

int bgi_min(int a, int b) {
	return a < b ? a : b;
}

int bgi_max(int a, int b) {
	return a > b ? a : b;
}

void bgi_swap(int *a, int *b) {
	int temp = *a;
	*a = *b;
	*b = temp;
}

/* Angle conversion */
void bgi_angle_to_point(int angle, int radius, int *x, int *y) {
	double rad = angle * M_PI / 180.0;
	*x = (int)(radius * cos(rad));
	*y = (int)(radius * sin(rad));
}

/* Bresenham line algorithm with pattern support */
void bgi_draw_line_bresenham(int x1, int y1, int x2, int y2,
                              void (*putpixel)(int, int, int),
                              int color, unsigned short pattern) {
	int dx = bgi_abs(x2 - x1);
	int dy = bgi_abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1;
	int sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;
	int e2;
	int pattern_pos = 0;

	while (1) {
		/* Draw pixel if pattern bit is set */
		if (pattern & (1 << (pattern_pos & 15))) {
			putpixel(x1, y1, color);
		}

		if (x1 == x2 && y1 == y2)
			break;

		e2 = 2 * err;
		if (e2 > -dy) {
			err -= dy;
			x1 += sx;
		}
		if (e2 < dx) {
			err += dx;
			y1 += sy;
		}
		pattern_pos++;
	}
}

/* Bresenham circle algorithm */
void bgi_draw_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color) {
	int dx = 0;
	int dy = radius;
	int d = 1 - radius;

	while (dx <= dy) {
		/* Draw 8 symmetric points */
		putpixel(x + dx, y + dy, color);
		putpixel(x - dx, y + dy, color);
		putpixel(x + dx, y - dy, color);
		putpixel(x - dx, y - dy, color);
		putpixel(x + dy, y + dx, color);
		putpixel(x - dy, y + dx, color);
		putpixel(x + dy, y - dx, color);
		putpixel(x - dy, y - dx, color);

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

/* Filled circle */
void bgi_fill_circle_bresenham(int x, int y, int radius,
                                void (*putpixel)(int, int, int),
                                int color) {
	int dx = 0;
	int dy = radius;
	int d = 1 - radius;
	int cx;

	while (dx <= dy) {
		/* Draw horizontal lines for fill */
		for (cx = x - dx; cx <= x + dx; cx++) {
			putpixel(cx, y + dy, color);
			putpixel(cx, y - dy, color);
		}
		for (cx = x - dy; cx <= x + dy; cx++) {
			putpixel(cx, y + dx, color);
			putpixel(cx, y - dx, color);
		}

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

/* Ellipse drawing (Bresenham-based) */
void bgi_draw_ellipse(int cx, int cy, int stangle, int endangle,
                      int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color) {
	int x, y;
	int xr2 = xradius * xradius;
	int yr2 = yradius * yradius;
	int two_xr2 = 2 * xr2;
	int two_yr2 = 2 * yr2;
	int dx, dy, px, py;
	int draw_full = (stangle == 0 && endangle == 360);

	/* Region 1 */
	x = 0;
	y = yradius;
	px = 0;
	py = two_xr2 * y;
	dx = yr2 * ((x * 4) + 6);
	dy = xr2 * ((1 - y) * 4);

	while (px < py) {
		if (draw_full) {
			putpixel(cx + x, cy + y, color);
			putpixel(cx - x, cy + y, color);
			putpixel(cx + x, cy - y, color);
			putpixel(cx - x, cy - y, color);
		}

		x++;
		px += two_yr2;
		if (dx < dy) {
			dx += two_yr2;
		} else {
			y--;
			py -= two_xr2;
			dy -= two_xr2;
			dx += two_yr2 - two_xr2;
		}
	}

	/* Region 2 */
	dx = yr2 * ((x * 2) + 3);
	dy = xr2 * ((y - 1) * 2);
	px = two_yr2 * x;
	py = two_xr2 * y;

	while (y > 0) {
		if (draw_full) {
			putpixel(cx + x, cy + y, color);
			putpixel(cx - x, cy + y, color);
			putpixel(cx + x, cy - y, color);
			putpixel(cx - x, cy - y, color);
		}

		y--;
		py -= two_xr2;
		if (dy < dx) {
			dy += two_xr2;
		} else {
			x++;
			px += two_yr2;
			dx += two_yr2;
			dy += two_xr2 - two_yr2;
		}
	}
}

/* Filled ellipse */
void bgi_fill_ellipse(int cx, int cy, int xradius, int yradius,
                      void (*putpixel)(int, int, int),
                      int color) {
	int x, y, px;
	int xr2 = xradius * xradius;
	int yr2 = yradius * yradius;
	int two_xr2 = 2 * xr2;
	int two_yr2 = 2 * yr2;
	int dx, dy, py;

	/* Region 1 */
	x = 0;
	y = yradius;
	px = 0;
	py = two_xr2 * y;
	dx = yr2 * ((x * 4) + 6);
	dy = xr2 * ((1 - y) * 4);

	while (px < py) {
		/* Draw horizontal lines for fill */
		for (int sx = cx - x; sx <= cx + x; sx++) {
			putpixel(sx, cy + y, color);
			putpixel(sx, cy - y, color);
		}

		x++;
		px += two_yr2;
		if (dx < dy) {
			dx += two_yr2;
		} else {
			y--;
			py -= two_xr2;
			dy -= two_xr2;
			dx += two_yr2 - two_xr2;
		}
	}

	/* Region 2 */
	while (y >= 0) {
		for (int sx = cx - x; sx <= cx + x; sx++) {
			putpixel(sx, cy + y, color);
			putpixel(sx, cy - y, color);
		}

		y--;
		py -= two_xr2;
		if (dy < dx) {
			dy += two_xr2;
		} else {
			x++;
			px += two_yr2;
			dx += two_yr2;
			dy += two_xr2 - two_yr2;
		}
	}
}

/* Arc drawing with coordinate tracking */
void bgi_draw_arc(int cx, int cy, int stangle, int endangle, int radius,
                  void (*putpixel)(int, int, int),
                  int color, struct arccoordstype *coords) {
	int x = 0;
	int y = radius;
	int d = 1 - radius;
	double start_rad = stangle * M_PI / 180.0;
	double end_rad = endangle * M_PI / 180.0;

	if (coords) {
		coords->x = cx;
		coords->y = cy;
		coords->xstart = cx + (int)(radius * cos(start_rad));
		coords->ystart = cy - (int)(radius * sin(start_rad));
		coords->xend = cx + (int)(radius * cos(end_rad));
		coords->yend = cy - (int)(radius * sin(end_rad));
	}

	while (x <= y) {
		int points[8][2] = {
			{cx + x, cy - y}, {cx + y, cy - x},
			{cx + y, cy + x}, {cx + x, cy + y},
			{cx - x, cy + y}, {cx - y, cy + x},
			{cx - y, cy - x}, {cx - x, cy - y}
		};

		for (int i = 0; i < 8; i++) {
			int px = points[i][0] - cx;
			int py = cy - points[i][1];
			double angle = atan2(py, px) * 180.0 / M_PI;
			if (angle < 0) angle += 360.0;

			if ((stangle <= endangle && angle >= stangle && angle <= endangle) ||
			    (stangle > endangle && (angle >= stangle || angle <= endangle))) {
				putpixel(points[i][0], points[i][1], color);
			}
		}

		if (d < 0) {
			d += 2 * x + 3;
		} else {
			d += 2 * (x - y) + 5;
			y--;
		}
		x++;
	}
}

/* Polygon drawing */
void bgi_draw_polygon(int numpoints, const int *polypoints,
                      void (*line_func)(int, int, int, int)) {
	int i;
	for (i = 0; i < numpoints - 1; i++) {
		line_func(polypoints[i * 2], polypoints[i * 2 + 1],
		          polypoints[(i + 1) * 2], polypoints[(i + 1) * 2 + 1]);
	}
	/* Close polygon */
	line_func(polypoints[(numpoints - 1) * 2], polypoints[(numpoints - 1) * 2 + 1],
	          polypoints[0], polypoints[1]);
}

/* Scanline polygon fill */
void bgi_fill_polygon(int numpoints, const int *polypoints,
                      void (*putpixel)(int, int, int),
                      int color, int width, int height) {
	int y, i, j, nodes, swap;
	int *nodeX;
	int miny = height, maxy = 0;

	nodeX = (int *)malloc(numpoints * sizeof(int));
	if (!nodeX) return;

	/* Find Y bounds */
	for (i = 0; i < numpoints; i++) {
		int py = polypoints[i * 2 + 1];
		if (py < miny) miny = py;
		if (py > maxy) maxy = py;
	}

	/* Scanline fill */
	for (y = miny; y <= maxy; y++) {
		nodes = 0;

		/* Build edge list for this scanline */
		j = numpoints - 1;
		for (i = 0; i < numpoints; i++) {
			int y1 = polypoints[i * 2 + 1];
			int y2 = polypoints[j * 2 + 1];

			if ((y1 < y && y2 >= y) || (y2 < y && y1 >= y)) {
				int x1 = polypoints[i * 2];
				int x2 = polypoints[j * 2];
				nodeX[nodes++] = x1 + (y - y1) * (x2 - x1) / (y2 - y1);
			}
			j = i;
		}

		/* Sort nodes */
		i = 0;
		while (i < nodes - 1) {
			if (nodeX[i] > nodeX[i + 1]) {
				swap = nodeX[i];
				nodeX[i] = nodeX[i + 1];
				nodeX[i + 1] = swap;
				if (i) i--;
			} else {
				i++;
			}
		}

		/* Fill between node pairs */
		for (i = 0; i < nodes; i += 2) {
			if (i + 1 < nodes) {
				for (j = nodeX[i]; j <= nodeX[i + 1]; j++) {
					if (j >= 0 && j < width && y >= 0 && y < height) {
						putpixel(j, y, color);
					}
				}
			}
		}
	}

	free(nodeX);
}

/* Boundary flood fill */
void bgi_flood_fill(int x, int y, int border,
                    unsigned int (*getpixel)(int, int),
                    void (*putpixel)(int, int, int),
                    int fill_color, int width, int height) {
	struct point {
		int x, y;
	};
	struct point *stack;
	int stack_size = 0;
	int stack_capacity = 1000;
	unsigned int current_color;

	if (x < 0 || x >= width || y < 0 || y >= height)
		return;

	current_color = getpixel(x, y);
	if (current_color == border || current_color == (unsigned int)fill_color)
		return;

	stack = (struct point *)malloc(stack_capacity * sizeof(struct point));
	if (!stack) return;

	stack[stack_size].x = x;
	stack[stack_size].y = y;
	stack_size++;

	while (stack_size > 0) {
		struct point p = stack[--stack_size];
		int px = p.x;
		int py = p.y;

		if (px < 0 || px >= width || py < 0 || py >= height)
			continue;

		if (getpixel(px, py) == current_color) {
			putpixel(px, py, fill_color);

			/* Add neighbors to stack */
			if (stack_size + 4 > stack_capacity) {
				stack_capacity *= 2;
				stack = (struct point *)realloc(stack, stack_capacity * sizeof(struct point));
				if (!stack) return;
			}

			stack[stack_size].x = px + 1; stack[stack_size].y = py; stack_size++;
			stack[stack_size].x = px - 1; stack[stack_size].y = py; stack_size++;
			stack[stack_size].x = px; stack[stack_size].y = py + 1; stack_size++;
			stack[stack_size].x = px; stack[stack_size].y = py - 1; stack_size++;
		}
	}

	free(stack);
}

/* Pattern fill for rectangle */
void bgi_pattern_fill_rect(int left, int top, int right, int bottom,
                            void (*putpixel)(int, int, int),
                            const unsigned char *pattern, int color) {
	int x, y;
	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			int py = y & 7;
			int px = x & 7;
			if (pattern[py] & (1 << (7 - px))) {
				putpixel(x, y, color);
			}
		}
	}
}

/* Simple 8x8 bitmap font */
static const unsigned char font_8x8[128][8] = {
	/* Space (32) */
	[32] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
	/* A (65) */
	[65] = {0x18, 0x3C, 0x66, 0x66, 0x7E, 0x66, 0x66, 0x00},
	/* Add more characters as needed... */
};

void bgi_draw_text_simple(int x, int y, const char *text,
                          void (*putpixel)(int, int, int),
                          int color, int horiz, int vert) {
	int i, cx = x, cy = y;
	int len = 0;

	while (text[len]) len++;

	/* Adjust for horizontal justification */
	if (horiz == CENTER_TEXT) cx -= (len * 8) / 2;
	else if (horiz == RIGHT_TEXT) cx -= len * 8;

	/* Adjust for vertical justification */
	if (vert == CENTER_TEXT) cy -= 4;
	else if (vert == BOTTOM_TEXT) cy -= 8;

	for (i = 0; text[i]; i++) {
		unsigned char c = text[i];
		if (c < 128) {
			for (int row = 0; row < 8; row++) {
				for (int col = 0; col < 8; col++) {
					if (font_8x8[c][row] & (1 << (7 - col))) {
						putpixel(cx + col, cy + row, color);
					}
				}
			}
		}
		cx += 8;
	}
}

int bgi_text_width_simple(const char *text) {
	int len = 0;
	while (text[len]) len++;
	return len * 8;
}

int bgi_text_height_simple(void) {
	return 8;
}
