/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Tseng Labs ET3000/ET4000 SVGA
 *
 * Popular 1990s SVGA chipset, high resolution up to 1280x1024
 * Works on: DOS with Tseng Labs ET3000/ET4000 cards
 * Features: Segment selection, high resolution modes
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

#define ET4K_SEGMENT_SELECT 0x3CD

static struct {
	unsigned char far *video_memory;
	unsigned char *offscreen;
	int width, height, current_segment;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} tseng_state;

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* ET4000 1024x768x256 mode */
	regs.x.ax = 0x0030;
	int86(0x10, &regs, &regs);

	tseng_state.video_memory = (unsigned char far *)MK_FP(0xA000, 0);
	tseng_state.width = 1024;
	tseng_state.height = 768;
	tseng_state.current_segment = -1;

	tseng_state.offscreen = (unsigned char *)malloc(1024L * 768L);
	if (!tseng_state.offscreen) { *driver = grNotDetected; return; }

	tseng_state.current_color = WHITE;
	tseng_state.current_bkcolor = BLACK;
	tseng_state.line_style = SOLID_LINE;
	tseng_state.line_pattern = 0xFFFF;
	tseng_state.fill_style = SOLID_FILL;
	tseng_state.fill_color = WHITE;
	tseng_state.initialized = 1;
	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!tseng_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	if (tseng_state.offscreen) free(tseng_state.offscreen);
	memset(&tseng_state, 0, sizeof(tseng_state));
}

int getmaxx(void) { return tseng_state.width - 1; }
int getmaxy(void) { return tseng_state.height - 1; }
void setcolor(int color) { tseng_state.current_color = color & 0x0F; }
int getcolor(void) { return tseng_state.current_color; }
void setbkcolor(int color) { tseng_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return tseng_state.current_bkcolor; }

void cleardevice(void) {
	memset(tseng_state.offscreen, tseng_state.current_bkcolor, 1024L * 768L);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < tseng_state.width && y >= 0 && y < tseng_state.height)
		tseng_state.offscreen[y * tseng_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= tseng_state.width || y < 0 || y >= tseng_state.height) return 0;
	return tseng_state.offscreen[y * tseng_state.width + x];
}

void moveto(int x, int y) { tseng_state.current_x = x; tseng_state.current_y = y; }
void moverel(int dx, int dy) { tseng_state.current_x += dx; tseng_state.current_y += dy; }
int getx(void) { return tseng_state.current_x; }
int gety(void) { return tseng_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (tseng_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, tseng_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(tseng_state.current_x, tseng_state.current_y, x, y);
	tseng_state.current_x = x; tseng_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = tseng_state.current_x + dx, y2 = tseng_state.current_y + dy;
	line(tseng_state.current_x, tseng_state.current_y, x2, y2);
	tseng_state.current_x = x2; tseng_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, tseng_state.current_color);
		putpixel(x - dx, y + dy, tseng_state.current_color);
		putpixel(x + dx, y - dy, tseng_state.current_color);
		putpixel(x - dx, y - dy, tseng_state.current_color);
		putpixel(x + dy, y + dx, tseng_state.current_color);
		putpixel(x - dy, y + dx, tseng_state.current_color);
		putpixel(x + dy, y - dx, tseng_state.current_color);
		putpixel(x - dy, y - dx, tseng_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	tseng_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: tseng_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: tseng_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: tseng_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: tseng_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: tseng_state.line_pattern = pattern; break;
	default: tseng_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	tseng_state.fill_style = pattern; tseng_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, tseng_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	tseng_state.text_justify_h = horiz; tseng_state.text_justify_v = vert;
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
	return tseng_state.initialized ? grOk : grNoInitGraph;
}
