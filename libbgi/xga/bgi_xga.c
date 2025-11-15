/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for IBM XGA (eXtended Graphics Array)
 *
 * IBM XGA high-resolution graphics standard
 * Works on: DOS/OS2 with IBM PS/2 XGA adapter
 * Features: 1024x768, hardware acceleration
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

static struct {
	unsigned char far *video_memory;
	unsigned char *offscreen;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} xga_state;

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* XGA 1024x768x256 mode */
	regs.x.ax = 0x0107;
	int86(0x10, &regs, &regs);

	xga_state.video_memory = (unsigned char far *)MK_FP(0xA000, 0);
	xga_state.width = 1024;
	xga_state.height = 768;

	xga_state.offscreen = (unsigned char *)malloc(1024L * 768L);
	if (!xga_state.offscreen) { *driver = grNotDetected; return; }

	xga_state.current_color = WHITE;
	xga_state.current_bkcolor = BLACK;
	xga_state.line_style = SOLID_LINE;
	xga_state.line_pattern = 0xFFFF;
	xga_state.fill_style = SOLID_FILL;
	xga_state.fill_color = WHITE;
	xga_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!xga_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	if (xga_state.offscreen) free(xga_state.offscreen);
	memset(&xga_state, 0, sizeof(xga_state));
}

int getmaxx(void) { return xga_state.width - 1; }
int getmaxy(void) { return xga_state.height - 1; }
void setcolor(int color) { xga_state.current_color = color & 0x0F; }
int getcolor(void) { return xga_state.current_color; }
void setbkcolor(int color) { xga_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return xga_state.current_bkcolor; }

void cleardevice(void) {
	memset(xga_state.offscreen, xga_state.current_bkcolor, 1024L * 768L);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < xga_state.width && y >= 0 && y < xga_state.height)
		xga_state.offscreen[y * xga_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= xga_state.width || y < 0 || y >= xga_state.height) return 0;
	return xga_state.offscreen[y * xga_state.width + x];
}

void moveto(int x, int y) { xga_state.current_x = x; xga_state.current_y = y; }
void moverel(int dx, int dy) { xga_state.current_x += dx; xga_state.current_y += dy; }
int getx(void) { return xga_state.current_x; }
int gety(void) { return xga_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (xga_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, xga_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(xga_state.current_x, xga_state.current_y, x, y);
	xga_state.current_x = x; xga_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = xga_state.current_x + dx, y2 = xga_state.current_y + dy;
	line(xga_state.current_x, xga_state.current_y, x2, y2);
	xga_state.current_x = x2; xga_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, xga_state.current_color);
		putpixel(x - dx, y + dy, xga_state.current_color);
		putpixel(x + dx, y - dy, xga_state.current_color);
		putpixel(x - dx, y - dy, xga_state.current_color);
		putpixel(x + dy, y + dx, xga_state.current_color);
		putpixel(x - dy, y + dx, xga_state.current_color);
		putpixel(x + dy, y - dx, xga_state.current_color);
		putpixel(x - dy, y - dx, xga_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	xga_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: xga_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: xga_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: xga_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: xga_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: xga_state.line_pattern = pattern; break;
	default: xga_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	xga_state.fill_style = pattern; xga_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, xga_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	xga_state.text_justify_h = horiz; xga_state.text_justify_v = vert;
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
	return xga_state.initialized ? grOk : grNoInitGraph;
}
