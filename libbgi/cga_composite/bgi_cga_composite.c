/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for CGA Composite (NTSC artifact colors)
 *
 * CGA composite video with NTSC artifact colors
 * Works on: DOS with IBM CGA adapter and composite monitor
 * Features: 16 artifact colors via NTSC composite encoding
 * Note: Colors appear only on composite monitors, not RGB
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

#define CGA_COMPOSITE_MODE 0x04  /* 320x200 4-color mode */
#define CGA_PALETTE_REG 0x3D9

static struct {
	unsigned char far *video_memory;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	int initialized;
} cgac_state;

/* Artifact color mapping (composite NTSC) */
static const unsigned char artifact_colors[16] = {
	0x00, 0x55, 0xAA, 0xFF,  /* Base CGA colors */
	0x11, 0x22, 0x33, 0x44,  /* Artifact colors */
	0x66, 0x77, 0x88, 0x99,  /* More artifacts */
	0xBB, 0xCC, 0xDD, 0xEE   /* Extended artifacts */
};

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;

	if (*driver == DETECT) { *driver = CGA; *mode = CGAC0; }

	/* Set CGA 320x200 4-color mode */
	regs.h.ah = 0x00;
	regs.h.al = CGA_COMPOSITE_MODE;
	int86(0x10, &regs, &regs);

	/* Enable composite color burst */
	outp(CGA_PALETTE_REG, 0x30);  /* Palette 1, color burst on */

	cgac_state.video_memory = (unsigned char far *)MK_FP(0xB800, 0);
	cgac_state.width = 320;
	cgac_state.height = 200;
	cgac_state.current_color = WHITE;
	cgac_state.current_bkcolor = BLACK;
	cgac_state.current_x = 0;
	cgac_state.current_y = 0;
	cgac_state.line_style = SOLID_LINE;
	cgac_state.line_pattern = 0xFFFF;
	cgac_state.fill_style = SOLID_FILL;
	cgac_state.fill_color = WHITE;
	cgac_state.text_justify_h = LEFT_TEXT;
	cgac_state.text_justify_v = TOP_TEXT;
	cgac_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!cgac_state.initialized) return;
	regs.h.ah = 0x00;
	regs.h.al = 0x03;  /* Text mode */
	int86(0x10, &regs, &regs);
	memset(&cgac_state, 0, sizeof(cgac_state));
}

int getmaxx(void) { return cgac_state.width - 1; }
int getmaxy(void) { return cgac_state.height - 1; }
void setcolor(int color) { cgac_state.current_color = color & 0x0F; }
int getcolor(void) { return cgac_state.current_color; }
void setbkcolor(int color) { cgac_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return cgac_state.current_bkcolor; }

void cleardevice(void) {
	_fmemset(cgac_state.video_memory, cgac_state.current_bkcolor, 16384);
}

void putpixel(int x, int y, int color) {
	unsigned char far *ptr;
	int offset, shift;
	unsigned char mask, pixel_val;

	if (x < 0 || x >= cgac_state.width || y < 0 || y >= cgac_state.height)
		return;

	/* CGA 320x200: 4 pixels per byte */
	offset = (y * 80) + (x >> 2);
	if (y & 1) offset += 8192;  /* Interlaced scanlines */
	shift = 6 - ((x & 3) << 1);
	mask = ~(3 << shift);
	pixel_val = artifact_colors[color & 0x0F] & 3;

	ptr = cgac_state.video_memory + offset;
	*ptr = (*ptr & mask) | (pixel_val << shift);
}

unsigned int getpixel(int x, int y) {
	unsigned char far *ptr;
	int offset, shift;

	if (x < 0 || x >= cgac_state.width || y < 0 || y >= cgac_state.height)
		return 0;

	offset = (y * 80) + (x >> 2);
	if (y & 1) offset += 8192;
	shift = 6 - ((x & 3) << 1);

	ptr = cgac_state.video_memory + offset;
	return (*ptr >> shift) & 3;
}

void moveto(int x, int y) { cgac_state.current_x = x; cgac_state.current_y = y; }
void moverel(int dx, int dy) { cgac_state.current_x += dx; cgac_state.current_y += dy; }
int getx(void) { return cgac_state.current_x; }
int gety(void) { return cgac_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	while (1) {
		if (cgac_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, cgac_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(cgac_state.current_x, cgac_state.current_y, x, y);
	cgac_state.current_x = x; cgac_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = cgac_state.current_x + dx, y2 = cgac_state.current_y + dy;
	line(cgac_state.current_x, cgac_state.current_y, x2, y2);
	cgac_state.current_x = x2; cgac_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, cgac_state.current_color);
		putpixel(x - dx, y + dy, cgac_state.current_color);
		putpixel(x + dx, y - dy, cgac_state.current_color);
		putpixel(x - dx, y - dy, cgac_state.current_color);
		putpixel(x + dy, y + dx, cgac_state.current_color);
		putpixel(x - dy, y + dx, cgac_state.current_color);
		putpixel(x + dy, y - dx, cgac_state.current_color);
		putpixel(x - dy, y - dx, cgac_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	cgac_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: cgac_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: cgac_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: cgac_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: cgac_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: cgac_state.line_pattern = pattern; break;
	default: cgac_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	cgac_state.fill_style = pattern;
	cgac_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, cgac_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	cgac_state.text_justify_h = horiz;
	cgac_state.text_justify_v = vert;
}

int textheight(const char *textstring) { return 8; }
int textwidth(const char *textstring) { return _fstrlen(textstring) * 8; }

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
	return cgac_state.initialized ? grOk : grNoInitGraph;
}
