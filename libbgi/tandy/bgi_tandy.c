/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Tandy 1000 Graphics
 *
 * Tandy Graphics Adapter (TGA) support
 * Works on: Tandy 1000 series computers
 * Features: 320x200x16, Tandy-specific modes
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

static struct {
	unsigned char far *video_memory;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} tandy_state;

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Tandy 320x200x16 mode */
	regs.x.ax = 0x0009;
	int86(0x10, &regs, &regs);

	tandy_state.video_memory = (unsigned char far *)MK_FP(0xB800, 0);
	tandy_state.width = 320;
	tandy_state.height = 200;
	tandy_state.current_color = WHITE;
	tandy_state.current_bkcolor = BLACK;
	tandy_state.line_style = SOLID_LINE;
	tandy_state.line_pattern = 0xFFFF;
	tandy_state.fill_style = SOLID_FILL;
	tandy_state.fill_color = WHITE;
	tandy_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!tandy_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	memset(&tandy_state, 0, sizeof(tandy_state));
}

int getmaxx(void) { return tandy_state.width - 1; }
int getmaxy(void) { return tandy_state.height - 1; }
void setcolor(int color) { tandy_state.current_color = color & 0x0F; }
int getcolor(void) { return tandy_state.current_color; }
void setbkcolor(int color) { tandy_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return tandy_state.current_bkcolor; }

void cleardevice(void) {
	int i;
	for (i = 0; i < 16000; i++)
		tandy_state.video_memory[i] = 0;
}

void putpixel(int x, int y, int color) {
	unsigned char far *ptr;
	int offset, bit_pos;
	if (x < 0 || x >= tandy_state.width || y < 0 || y >= tandy_state.height) return;

	offset = (y * 80) + (x / 4);
	bit_pos = (3 - (x % 4)) * 2;
	ptr = tandy_state.video_memory + offset;
	*ptr = (*ptr & ~(0x03 << bit_pos)) | ((color & 0x03) << bit_pos);
}

unsigned int getpixel(int x, int y) {
	unsigned char far *ptr;
	int offset, bit_pos;
	if (x < 0 || x >= tandy_state.width || y < 0 || y >= tandy_state.height) return 0;

	offset = (y * 80) + (x / 4);
	bit_pos = (3 - (x % 4)) * 2;
	ptr = tandy_state.video_memory + offset;
	return (*ptr >> bit_pos) & 0x03;
}

void moveto(int x, int y) { tandy_state.current_x = x; tandy_state.current_y = y; }
void moverel(int dx, int dy) { tandy_state.current_x += dx; tandy_state.current_y += dy; }
int getx(void) { return tandy_state.current_x; }
int gety(void) { return tandy_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (tandy_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, tandy_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(tandy_state.current_x, tandy_state.current_y, x, y);
	tandy_state.current_x = x; tandy_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = tandy_state.current_x + dx, y2 = tandy_state.current_y + dy;
	line(tandy_state.current_x, tandy_state.current_y, x2, y2);
	tandy_state.current_x = x2; tandy_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, tandy_state.current_color);
		putpixel(x - dx, y + dy, tandy_state.current_color);
		putpixel(x + dx, y - dy, tandy_state.current_color);
		putpixel(x - dx, y - dy, tandy_state.current_color);
		putpixel(x + dy, y + dx, tandy_state.current_color);
		putpixel(x - dy, y + dx, tandy_state.current_color);
		putpixel(x + dy, y - dx, tandy_state.current_color);
		putpixel(x - dy, y - dx, tandy_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	tandy_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: tandy_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: tandy_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: tandy_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: tandy_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: tandy_state.line_pattern = pattern; break;
	default: tandy_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	tandy_state.fill_style = pattern; tandy_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, tandy_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	tandy_state.text_justify_h = horiz; tandy_state.text_justify_v = vert;
}

int textheight(const char *textstring) { return 8; }
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
	return tandy_state.initialized ? grOk : grNoInitGraph;
}
