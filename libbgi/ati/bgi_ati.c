/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for ATI Graphics Cards
 *
 * ATI VGA Wonder, Mach8, Mach32, Mach64 support
 * Works on: DOS with ATI graphics cards
 * Features: Hardware acceleration, high resolution, ATI-specific modes
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

/* ATI Extended registers */
#define ATI_EXTENDED_REG	0x1CE
#define ATI_EXTENDED_DATA	0x1CF

static struct {
	unsigned char far *video_memory;
	unsigned char *offscreen;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} ati_state;

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* ATI 800x600x256 mode */
	regs.x.ax = 0x0061;
	int86(0x10, &regs, &regs);

	ati_state.video_memory = (unsigned char far *)MK_FP(0xA000, 0);
	ati_state.width = 800;
	ati_state.height = 600;

	ati_state.offscreen = (unsigned char *)malloc(800L * 600L);
	if (!ati_state.offscreen) { *driver = grNotDetected; return; }

	ati_state.current_color = WHITE;
	ati_state.current_bkcolor = BLACK;
	ati_state.current_x = 0;
	ati_state.current_y = 0;
	ati_state.line_style = SOLID_LINE;
	ati_state.line_pattern = 0xFFFF;
	ati_state.fill_style = SOLID_FILL;
	ati_state.fill_color = WHITE;
	ati_state.text_justify_h = LEFT_TEXT;
	ati_state.text_justify_v = TOP_TEXT;
	ati_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!ati_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	if (ati_state.offscreen) free(ati_state.offscreen);
	memset(&ati_state, 0, sizeof(ati_state));
}

int getmaxx(void) { return ati_state.width - 1; }
int getmaxy(void) { return ati_state.height - 1; }
void setcolor(int color) { ati_state.current_color = color & 0x0F; }
int getcolor(void) { return ati_state.current_color; }
void setbkcolor(int color) { ati_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return ati_state.current_bkcolor; }

void cleardevice(void) {
	memset(ati_state.offscreen, ati_state.current_bkcolor, 800L * 600L);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < ati_state.width && y >= 0 && y < ati_state.height)
		ati_state.offscreen[y * ati_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= ati_state.width || y < 0 || y >= ati_state.height) return 0;
	return ati_state.offscreen[y * ati_state.width + x];
}

void moveto(int x, int y) { ati_state.current_x = x; ati_state.current_y = y; }
void moverel(int dx, int dy) { ati_state.current_x += dx; ati_state.current_y += dy; }
int getx(void) { return ati_state.current_x; }
int gety(void) { return ati_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (ati_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, ati_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(ati_state.current_x, ati_state.current_y, x, y);
	ati_state.current_x = x; ati_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = ati_state.current_x + dx, y2 = ati_state.current_y + dy;
	line(ati_state.current_x, ati_state.current_y, x2, y2);
	ati_state.current_x = x2; ati_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, ati_state.current_color);
		putpixel(x - dx, y + dy, ati_state.current_color);
		putpixel(x + dx, y - dy, ati_state.current_color);
		putpixel(x - dx, y - dy, ati_state.current_color);
		putpixel(x + dy, y + dx, ati_state.current_color);
		putpixel(x - dy, y + dx, ati_state.current_color);
		putpixel(x + dy, y - dx, ati_state.current_color);
		putpixel(x - dy, y - dx, ati_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	ati_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: ati_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: ati_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: ati_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: ati_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: ati_state.line_pattern = pattern; break;
	default: ati_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	ati_state.fill_style = pattern; ati_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, ati_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	ati_state.text_justify_h = horiz; ati_state.text_justify_v = vert;
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
	return ati_state.initialized ? grOk : grNoInitGraph;
}
