/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using GGI (General Graphics Interface)
 *
 * Legacy Linux graphics abstraction layer
 * Works on: Linux with LibGGI
 * Requires: libggi
 * Features: Multiple display targets, hardware acceleration
 */

#include "../graphics.h"
#include <ggi/ggi.h>
#include <stdlib.h>
#include <string.h>

static struct {
	ggi_visual_t visual;
	ggi_mode mode;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	ggi_pixel colors[16];
	int initialized;
} ggi_state;

static const ggi_color ega_colors[16] = {
	{0x0000, 0x0000, 0x0000}, {0x0000, 0x0000, 0xAAAA},
	{0x0000, 0xAAAA, 0x0000}, {0x0000, 0xAAAA, 0xAAAA},
	{0xAAAA, 0x0000, 0x0000}, {0xAAAA, 0x0000, 0xAAAA},
	{0xAAAA, 0x5555, 0x0000}, {0xAAAA, 0xAAAA, 0xAAAA},
	{0x5555, 0x5555, 0x5555}, {0x5555, 0x5555, 0xFFFF},
	{0x5555, 0xFFFF, 0x5555}, {0x5555, 0xFFFF, 0xFFFF},
	{0xFFFF, 0x5555, 0x5555}, {0xFFFF, 0x5555, 0xFFFF},
	{0xFFFF, 0xFFFF, 0x5555}, {0xFFFF, 0xFFFF, 0xFFFF}
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

	if (ggiInit() != 0) {
		*driver = grNotDetected;
		return;
	}

	ggi_state.visual = ggiOpen(NULL);
	if (!ggi_state.visual) {
		ggiExit();
		*driver = grNotDetected;
		return;
	}

	ggiSetSimpleMode(ggi_state.visual, width, height, 1, GT_AUTO);
	ggiGetMode(ggi_state.visual, &ggi_state.mode);

	for (i = 0; i < 16; i++)
		ggi_state.colors[i] = ggiMapColor(ggi_state.visual, &ega_colors[i]);

	ggi_state.width = width;
	ggi_state.height = height;
	ggi_state.current_color = WHITE;
	ggi_state.current_bkcolor = BLACK;
	ggi_state.current_x = 0;
	ggi_state.current_y = 0;
	ggi_state.line_style = SOLID_LINE;
	ggi_state.line_pattern = 0xFFFF;
	ggi_state.fill_style = SOLID_FILL;
	ggi_state.fill_color = WHITE;
	ggi_state.text_justify_h = LEFT_TEXT;
	ggi_state.text_justify_v = TOP_TEXT;
	ggi_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	if (!ggi_state.initialized) return;
	if (ggi_state.visual) ggiClose(ggi_state.visual);
	ggiExit();
	memset(&ggi_state, 0, sizeof(ggi_state));
}

int getmaxx(void) { return ggi_state.width - 1; }
int getmaxy(void) { return ggi_state.height - 1; }
void setcolor(int color) { ggi_state.current_color = color & 0x0F; }
int getcolor(void) { return ggi_state.current_color; }
void setbkcolor(int color) { ggi_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return ggi_state.current_bkcolor; }

void cleardevice(void) {
	ggiSetGCForeground(ggi_state.visual, ggi_state.colors[ggi_state.current_bkcolor]);
	ggiDrawBox(ggi_state.visual, 0, 0, ggi_state.width, ggi_state.height);
	ggiFlush(ggi_state.visual);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < ggi_state.width && y >= 0 && y < ggi_state.height) {
		ggiPutPixel(ggi_state.visual, x, y, ggi_state.colors[color & 0x0F]);
		ggiFlush(ggi_state.visual);
	}
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= ggi_state.width || y < 0 || y >= ggi_state.height) return 0;
	ggi_pixel p = ggiGetPixel(ggi_state.visual, x, y);
	for (int i = 0; i < 16; i++)
		if (ggi_state.colors[i] == p) return i;
	return 0;
}

void moveto(int x, int y) { ggi_state.current_x = x; ggi_state.current_y = y; }
void moverel(int dx, int dy) { ggi_state.current_x += dx; ggi_state.current_y += dy; }
int getx(void) { return ggi_state.current_x; }
int gety(void) { return ggi_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	ggiSetGCForeground(ggi_state.visual, ggi_state.colors[ggi_state.current_color]);
	ggiDrawLine(ggi_state.visual, x1, y1, x2, y2);
	ggiFlush(ggi_state.visual);
}

void lineto(int x, int y) {
	line(ggi_state.current_x, ggi_state.current_y, x, y);
	ggi_state.current_x = x; ggi_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = ggi_state.current_x + dx, y2 = ggi_state.current_y + dy;
	line(ggi_state.current_x, ggi_state.current_y, x2, y2);
	ggi_state.current_x = x2; ggi_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	ggiSetGCForeground(ggi_state.visual, ggi_state.colors[ggi_state.current_color]);
	ggiDrawBox(ggi_state.visual, left, top, right - left + 1, bottom - top + 1);
	ggiFlush(ggi_state.visual);
}

void circle(int x, int y, int radius) {
	/* GGI doesn't have native circle, use pixel approximation */
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, ggi_state.current_color);
		putpixel(x - dx, y + dy, ggi_state.current_color);
		putpixel(x + dx, y - dy, ggi_state.current_color);
		putpixel(x - dx, y - dy, ggi_state.current_color);
		putpixel(x + dy, y + dx, ggi_state.current_color);
		putpixel(x - dy, y + dx, ggi_state.current_color);
		putpixel(x + dy, y - dx, ggi_state.current_color);
		putpixel(x - dy, y - dx, ggi_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	ggi_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: ggi_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: ggi_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: ggi_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: ggi_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: ggi_state.line_pattern = pattern; break;
	default: ggi_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	ggi_state.fill_style = pattern;
	ggi_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	ggiSetGCForeground(ggi_state.visual, ggi_state.colors[ggi_state.fill_color & 0x0F]);
	ggiDrawBox(ggi_state.visual, left, top, right - left + 1, bottom - top + 1);
	ggiFlush(ggi_state.visual);
}

void settextjustify(int horiz, int vert) {
	ggi_state.text_justify_h = horiz;
	ggi_state.text_justify_v = vert;
}

int textheight(const char *textstring) { return 12; }
int textwidth(const char *textstring) { return strlen(textstring) * 8; }

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
	return ggi_state.initialized ? grOk : grNoInitGraph;
}
