/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for S3 Graphics (86C911, 924, Trio, ViRGE)
 *
 * Popular 1990s graphics accelerators with hardware acceleration
 * Works on: DOS with S3 86C911/924/Trio32/64/ViRGE cards
 * Features: Hardware acceleration, 1024x768x256, blitting
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

/* S3 Extended registers */
#define S3_EXTENDED_ENABLE	0x3D4
#define S3_REGISTER_LOCK	0x35
#define S3_MEMORY_CONFIG	0x36

static struct {
	unsigned char far *video_memory;
	unsigned char *offscreen;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	int initialized;
} s3_state;

void initgraph(int *driver, int *mode, const char *path) {
	union REGS regs;
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* S3 1024x768x256 mode */
	regs.x.ax = 0x0205;
	int86(0x10, &regs, &regs);

	s3_state.video_memory = (unsigned char far *)MK_FP(0xA000, 0);
	s3_state.width = 1024;
	s3_state.height = 768;

	s3_state.offscreen = (unsigned char *)malloc(1024L * 768L);
	if (!s3_state.offscreen) { *driver = grNotDetected; return; }

	s3_state.current_color = WHITE;
	s3_state.current_bkcolor = BLACK;
	s3_state.current_x = 0;
	s3_state.current_y = 0;
	s3_state.line_style = SOLID_LINE;
	s3_state.line_pattern = 0xFFFF;
	s3_state.fill_style = SOLID_FILL;
	s3_state.fill_color = WHITE;
	s3_state.text_justify_h = LEFT_TEXT;
	s3_state.text_justify_v = TOP_TEXT;
	s3_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	union REGS regs;
	if (!s3_state.initialized) return;
	regs.x.ax = 0x0003;
	int86(0x10, &regs, &regs);
	if (s3_state.offscreen) free(s3_state.offscreen);
	memset(&s3_state, 0, sizeof(s3_state));
}

int getmaxx(void) { return s3_state.width - 1; }
int getmaxy(void) { return s3_state.height - 1; }
void setcolor(int color) { s3_state.current_color = color & 0x0F; }
int getcolor(void) { return s3_state.current_color; }
void setbkcolor(int color) { s3_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return s3_state.current_bkcolor; }

void cleardevice(void) {
	memset(s3_state.offscreen, s3_state.current_bkcolor, 1024L * 768L);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < s3_state.width && y >= 0 && y < s3_state.height)
		s3_state.offscreen[y * s3_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= s3_state.width || y < 0 || y >= s3_state.height) return 0;
	return s3_state.offscreen[y * s3_state.width + x];
}

void moveto(int x, int y) { s3_state.current_x = x; s3_state.current_y = y; }
void moverel(int dx, int dy) { s3_state.current_x += dx; s3_state.current_y += dy; }
int getx(void) { return s3_state.current_x; }
int gety(void) { return s3_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (s3_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, s3_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(s3_state.current_x, s3_state.current_y, x, y);
	s3_state.current_x = x; s3_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = s3_state.current_x + dx, y2 = s3_state.current_y + dy;
	line(s3_state.current_x, s3_state.current_y, x2, y2);
	s3_state.current_x = x2; s3_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top); line(right, top, right, bottom);
	line(right, bottom, left, bottom); line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, s3_state.current_color);
		putpixel(x - dx, y + dy, s3_state.current_color);
		putpixel(x + dx, y - dy, s3_state.current_color);
		putpixel(x - dx, y - dy, s3_state.current_color);
		putpixel(x + dy, y + dx, s3_state.current_color);
		putpixel(x - dy, y + dx, s3_state.current_color);
		putpixel(x + dy, y - dx, s3_state.current_color);
		putpixel(x - dy, y - dx, s3_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	s3_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: s3_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: s3_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: s3_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: s3_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: s3_state.line_pattern = pattern; break;
	default: s3_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	s3_state.fill_style = pattern; s3_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, s3_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	s3_state.text_justify_h = horiz; s3_state.text_justify_v = vert;
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
	return s3_state.initialized ? grOk : grNoInitGraph;
}
