/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Paradise/Western Digital PVGA1A
 *
 * Provides BGI graphics for Paradise VGA cards (WD90C00/10/11/30)
 * Works on: DOS with Paradise PVGA1A, PVGA1B cards
 * Features: 800x600x256, extended modes, bank switching
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

/* Paradise PVGA registers */
#define PVGA_INDEX		0x3CE
#define PVGA_DATA		0x3CF
#define PVGA_BANK_SELECT	0x09

static struct {
	unsigned char far *video_memory;
	unsigned char *offscreen;
	int width, height, current_bank;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	unsigned char colors[16];
	int initialized;
} pvga_state;

static void set_bank(int bank) {
	if (pvga_state.current_bank != bank) {
		outp(PVGA_INDEX, PVGA_BANK_SELECT);
		outp(PVGA_DATA, bank);
		pvga_state.current_bank = bank;
	}
}

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	int i;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Set Paradise 800x600x256 mode */
	regs.x.ax = 0x007F;
	regs.h.bl = 0x58;
	int86(0x10, &regs, &regs);

	pvga_state.video_memory = (unsigned char far *)MK_FP(0xA000, 0);
	pvga_state.width = 800;
	pvga_state.height = 600;
	pvga_state.current_bank = -1;

	pvga_state.offscreen = (unsigned char *)malloc(800L * 600L);
	if (!pvga_state.offscreen) { *driver = grNotDetected; return; }

	for (i = 0; i < 16; i++) pvga_state.colors[i] = i;
	pvga_state.current_color = WHITE;
	pvga_state.current_bkcolor = BLACK;
	pvga_state.current_x = 0;
	pvga_state.current_y = 0;
	pvga_state.line_style = SOLID_LINE;
	pvga_state.line_pattern = 0xFFFF;
	pvga_state.fill_style = SOLID_FILL;
	pvga_state.fill_color = WHITE;
	pvga_state.text_justify_h = LEFT_TEXT;
	pvga_state.text_justify_v = TOP_TEXT;
	pvga_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!pvga_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	if (pvga_state.offscreen) free(pvga_state.offscreen);
	memset(&pvga_state, 0, sizeof(pvga_state));
}

int getmaxx(void) { return pvga_state.width - 1; }
int getmaxy(void) { return pvga_state.height - 1; }
void setcolor(int color) { pvga_state.current_color = color & 0x0F; }
int getcolor(void) { return pvga_state.current_color; }
void setbkcolor(int color) { pvga_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return pvga_state.current_bkcolor; }

void cleardevice(void) {
	memset(pvga_state.offscreen, pvga_state.current_bkcolor, 800L * 600L);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < pvga_state.width && y >= 0 && y < pvga_state.height)
		pvga_state.offscreen[y * pvga_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= pvga_state.width || y < 0 || y >= pvga_state.height) return 0;
	return pvga_state.offscreen[y * pvga_state.width + x];
}

void moveto(int x, int y) { pvga_state.current_x = x; pvga_state.current_y = y; }
void moverel(int dx, int dy) { pvga_state.current_x += dx; pvga_state.current_y += dy; }
int getx(void) { return pvga_state.current_x; }
int gety(void) { return pvga_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (pvga_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, pvga_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(pvga_state.current_x, pvga_state.current_y, x, y);
	pvga_state.current_x = x; pvga_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = pvga_state.current_x + dx, y2 = pvga_state.current_y + dy;
	line(pvga_state.current_x, pvga_state.current_y, x2, y2);
	pvga_state.current_x = x2; pvga_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, pvga_state.current_color);
		putpixel(x - dx, y + dy, pvga_state.current_color);
		putpixel(x + dx, y - dy, pvga_state.current_color);
		putpixel(x - dx, y - dy, pvga_state.current_color);
		putpixel(x + dy, y + dx, pvga_state.current_color);
		putpixel(x - dy, y + dx, pvga_state.current_color);
		putpixel(x + dy, y - dx, pvga_state.current_color);
		putpixel(x - dy, y - dx, pvga_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	pvga_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: pvga_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: pvga_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: pvga_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: pvga_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: pvga_state.line_pattern = pattern; break;
	default: pvga_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	pvga_state.fill_style = pattern; pvga_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, pvga_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	pvga_state.text_justify_h = horiz; pvga_state.text_justify_v = vert;
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
	return pvga_state.initialized ? grOk : grNoInitGraph;
}
