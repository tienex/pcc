/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Cairo (2D vector graphics)
 *
 * Cross-platform 2D vector graphics using Cairo library
 * Works on: Linux, BSD, macOS, Windows
 * Requires: libcairo
 * Features: Anti-aliasing, high-quality rendering, PDF/SVG export
 */

#include "../graphics.h"
#include <cairo/cairo.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct {
	cairo_surface_t *surface;
	cairo_t *cr;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	double colors[16][3];
	int initialized;
} cairo_state;

/* EGA palette in RGB (0.0-1.0 range) */
static const double ega_rgb[16][3] = {
	{0.0, 0.0, 0.0},      /* BLACK */
	{0.0, 0.0, 0.67},     /* BLUE */
	{0.0, 0.67, 0.0},     /* GREEN */
	{0.0, 0.67, 0.67},    /* CYAN */
	{0.67, 0.0, 0.0},     /* RED */
	{0.67, 0.0, 0.67},    /* MAGENTA */
	{0.67, 0.33, 0.0},    /* BROWN */
	{0.67, 0.67, 0.67},   /* LIGHTGRAY */
	{0.33, 0.33, 0.33},   /* DARKGRAY */
	{0.33, 0.33, 1.0},    /* LIGHTBLUE */
	{0.33, 1.0, 0.33},    /* LIGHTGREEN */
	{0.33, 1.0, 1.0},     /* LIGHTCYAN */
	{1.0, 0.33, 0.33},    /* LIGHTRED */
	{1.0, 0.33, 1.0},     /* LIGHTMAGENTA */
	{1.0, 1.0, 0.33},     /* YELLOW */
	{1.0, 1.0, 1.0}       /* WHITE */
};

void initgraph(int *driver, int *mode, const char *path) {
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Create image surface (ARGB32) */
	cairo_state.surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
	if (cairo_surface_status(cairo_state.surface) != CAIRO_STATUS_SUCCESS) {
		*driver = grNotDetected;
		return;
	}

	/* Create Cairo context */
	cairo_state.cr = cairo_create(cairo_state.surface);
	if (cairo_status(cairo_state.cr) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(cairo_state.surface);
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		memcpy(cairo_state.colors[i], ega_rgb[i], sizeof(double) * 3);

	cairo_state.width = width;
	cairo_state.height = height;
	cairo_state.current_color = WHITE;
	cairo_state.current_bkcolor = BLACK;
	cairo_state.current_x = 0;
	cairo_state.current_y = 0;
	cairo_state.line_style = SOLID_LINE;
	cairo_state.line_pattern = 0xFFFF;
	cairo_state.fill_style = SOLID_FILL;
	cairo_state.fill_color = WHITE;
	cairo_state.text_justify_h = LEFT_TEXT;
	cairo_state.text_justify_v = TOP_TEXT;
	cairo_state.initialized = 1;

	/* Set initial drawing state */
	cairo_set_line_width(cairo_state.cr, 1.0);
	cairo_set_line_cap(cairo_state.cr, CAIRO_LINE_CAP_SQUARE);
	cairo_set_line_join(cairo_state.cr, CAIRO_LINE_JOIN_MITER);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!cairo_state.initialized)
		return;

	/* Optionally save to file before closing */
	if (cairo_state.surface)
		cairo_surface_write_to_png(cairo_state.surface, "bgi_output.png");

	if (cairo_state.cr)
		cairo_destroy(cairo_state.cr);
	if (cairo_state.surface)
		cairo_surface_destroy(cairo_state.surface);

	memset(&cairo_state, 0, sizeof(cairo_state));
}

int getmaxx(void) { return cairo_state.width - 1; }
int getmaxy(void) { return cairo_state.height - 1; }

void setcolor(int color) {
	cairo_state.current_color = color & 0x0F;
	cairo_set_source_rgb(cairo_state.cr,
	                      cairo_state.colors[cairo_state.current_color][0],
	                      cairo_state.colors[cairo_state.current_color][1],
	                      cairo_state.colors[cairo_state.current_color][2]);
}

int getcolor(void) { return cairo_state.current_color; }
void setbkcolor(int color) { cairo_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return cairo_state.current_bkcolor; }

void cleardevice(void) {
	cairo_save(cairo_state.cr);
	cairo_set_source_rgb(cairo_state.cr,
	                      cairo_state.colors[cairo_state.current_bkcolor][0],
	                      cairo_state.colors[cairo_state.current_bkcolor][1],
	                      cairo_state.colors[cairo_state.current_bkcolor][2]);
	cairo_paint(cairo_state.cr);
	cairo_restore(cairo_state.cr);
}

void putpixel(int x, int y, int color) {
	if (x < 0 || x >= cairo_state.width || y < 0 || y >= cairo_state.height)
		return;

	cairo_save(cairo_state.cr);
	cairo_set_source_rgb(cairo_state.cr,
	                      cairo_state.colors[color & 0x0F][0],
	                      cairo_state.colors[color & 0x0F][1],
	                      cairo_state.colors[color & 0x0F][2]);
	cairo_rectangle(cairo_state.cr, x, y, 1, 1);
	cairo_fill(cairo_state.cr);
	cairo_restore(cairo_state.cr);
}

unsigned int getpixel(int x, int y) {
	unsigned char *data;
	unsigned char r, g, b;
	int stride, i;

	if (x < 0 || x >= cairo_state.width || y < 0 || y >= cairo_state.height)
		return 0;

	cairo_surface_flush(cairo_state.surface);
	data = cairo_image_surface_get_data(cairo_state.surface);
	stride = cairo_image_surface_get_stride(cairo_state.surface);

	/* ARGB32 format: BGRA in memory */
	b = data[y * stride + x * 4 + 0];
	g = data[y * stride + x * 4 + 1];
	r = data[y * stride + x * 4 + 2];

	/* Find closest EGA color */
	for (i = 0; i < 16; i++) {
		int er = (int)(cairo_state.colors[i][0] * 255) - r;
		int eg = (int)(cairo_state.colors[i][1] * 255) - g;
		int eb = (int)(cairo_state.colors[i][2] * 255) - b;
		if (er * er + eg * eg + eb * eb < 100)
			return i;
	}

	return 0;
}

void moveto(int x, int y) {
	cairo_state.current_x = x;
	cairo_state.current_y = y;
}

void moverel(int dx, int dy) {
	cairo_state.current_x += dx;
	cairo_state.current_y += dy;
}

int getx(void) { return cairo_state.current_x; }
int gety(void) { return cairo_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	cairo_move_to(cairo_state.cr, x1 + 0.5, y1 + 0.5);
	cairo_line_to(cairo_state.cr, x2 + 0.5, y2 + 0.5);
	cairo_stroke(cairo_state.cr);
}

void lineto(int x, int y) {
	line(cairo_state.current_x, cairo_state.current_y, x, y);
	cairo_state.current_x = x;
	cairo_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = cairo_state.current_x + dx;
	int y2 = cairo_state.current_y + dy;
	line(cairo_state.current_x, cairo_state.current_y, x2, y2);
	cairo_state.current_x = x2;
	cairo_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	cairo_rectangle(cairo_state.cr, left + 0.5, top + 0.5,
	                right - left, bottom - top);
	cairo_stroke(cairo_state.cr);
}

void circle(int x, int y, int radius) {
	cairo_arc(cairo_state.cr, x + 0.5, y + 0.5, radius, 0, 2 * M_PI);
	cairo_stroke(cairo_state.cr);
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	double dashes[16];
	int dash_count = 0;

	cairo_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		cairo_state.line_pattern = 0xFFFF;
		cairo_set_dash(cairo_state.cr, NULL, 0, 0);
		break;
	case DOTTED_LINE:
		cairo_state.line_pattern = 0xCCCC;
		dashes[0] = 2; dashes[1] = 2;
		cairo_set_dash(cairo_state.cr, dashes, 2, 0);
		break;
	case CENTER_LINE:
		cairo_state.line_pattern = 0xF8F8;
		dashes[0] = 6; dashes[1] = 2; dashes[2] = 2; dashes[3] = 2;
		cairo_set_dash(cairo_state.cr, dashes, 4, 0);
		break;
	case DASHED_LINE:
		cairo_state.line_pattern = 0xF0F0;
		dashes[0] = 4; dashes[1] = 4;
		cairo_set_dash(cairo_state.cr, dashes, 2, 0);
		break;
	case USERBIT_LINE:
		cairo_state.line_pattern = pattern;
		/* Convert pattern to dashes */
		for (int i = 0; i < 16; i++) {
			if (pattern & (1 << (15 - i)))
				dashes[dash_count++] = 1;
		}
		if (dash_count > 0)
			cairo_set_dash(cairo_state.cr, dashes, dash_count, 0);
		break;
	default:
		cairo_state.line_pattern = 0xFFFF;
		cairo_set_dash(cairo_state.cr, NULL, 0, 0);
	}

	if (thickness > 0)
		cairo_set_line_width(cairo_state.cr, thickness);
}

void setfillstyle(int pattern, int color) {
	cairo_state.fill_style = pattern;
	cairo_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	cairo_save(cairo_state.cr);
	cairo_set_source_rgb(cairo_state.cr,
	                      cairo_state.colors[cairo_state.fill_color & 0x0F][0],
	                      cairo_state.colors[cairo_state.fill_color & 0x0F][1],
	                      cairo_state.colors[cairo_state.fill_color & 0x0F][2]);
	cairo_rectangle(cairo_state.cr, left, top, right - left + 1, bottom - top + 1);
	cairo_fill(cairo_state.cr);
	cairo_restore(cairo_state.cr);
}

void settextjustify(int horiz, int vert) {
	cairo_state.text_justify_h = horiz;
	cairo_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	cairo_font_extents_t extents;
	cairo_font_extents(cairo_state.cr, &extents);
	return (int)extents.height;
}

int textwidth(const char *textstring) {
	cairo_text_extents_t extents;
	cairo_text_extents(cairo_state.cr, textstring, &extents);
	return (int)extents.width;
}

const char *grapherrormsg(int errorcode) {
	switch (errorcode) {
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	case grNotDetected: return "Graphics hardware not detected";
	case grFileNotFound: return "Driver file not found";
	case grInvalidDriver: return "Invalid graphics driver";
	case grNoLoadMem: return "Insufficient memory to load driver";
	default: return "Unknown error";
	}
}

int graphresult(void) {
	return cairo_state.initialized ? grOk : grNoInitGraph;
}
