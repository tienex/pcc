/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Tektronix 4014 vector graphics terminal
 *
 * Classic vector graphics terminal protocol
 * Works on: Tektronix 4010/4014 terminals, xterm -t, emulators
 * Features: 1024x780 resolution, vector drawing
 * Output: Tek 4014 escape sequences to stdout
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TEK_WIDTH  1024
#define TEK_HEIGHT 780

/* Tektronix control codes */
#define TEK_GS  0x1D  /* Graphics mode */
#define TEK_US  0x1F  /* Alpha mode */
#define TEK_ESC 0x1B  /* Escape */
#define TEK_FF  0x0C  /* Form feed (clear screen) */

static struct {
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	int pen_down;
	int initialized;
} tek_state;

/* Encode Tektronix coordinate (12-bit value split into 5 bits + 7 bits) */
static void tek_encode_coord(int x, int y) {
	int hix = (x >> 5) & 0x1F;
	int lox = x & 0x1F;
	int hiy = (y >> 5) & 0x1F;
	int loy = y & 0x1F;

	/* Tektronix coordinate encoding:
	 * HIY (bit 7-12 of Y) | 0x20
	 * LOY (bit 2-6 of Y)  | 0x60
	 * HIX (bit 7-12 of X) | 0x20
	 * LOX (bit 2-6 of X)  | 0x40
	 */
	putchar((hiy | 0x20));
	putchar((loy | 0x60));
	putchar((hix | 0x20));
	putchar((lox | 0x40));
}

/* Move to position in graphics mode */
static void tek_move(int x, int y) {
	putchar(TEK_GS);  /* Enter graphics mode */
	tek_encode_coord(x, y);
	tek_state.pen_down = 0;
}

/* Draw to position in graphics mode */
static void tek_draw(int x, int y) {
	tek_encode_coord(x, y);
	tek_state.pen_down = 1;
}

void initgraph(int *driver, int *mode, const char *path) {
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Tektronix 4014 has fixed 1024x780 resolution */
	tek_state.width = TEK_WIDTH;
	tek_state.height = TEK_HEIGHT;

	tek_state.current_color = WHITE;
	tek_state.current_bkcolor = BLACK;
	tek_state.current_x = 0;
	tek_state.current_y = 0;
	tek_state.line_style = SOLID_LINE;
	tek_state.line_pattern = 0xFFFF;
	tek_state.fill_style = SOLID_FILL;
	tek_state.fill_color = WHITE;
	tek_state.text_justify_h = LEFT_TEXT;
	tek_state.text_justify_v = TOP_TEXT;
	tek_state.pen_down = 0;
	tek_state.initialized = 1;

	/* Enter Tektronix mode */
	printf("%c%c", TEK_ESC, TEK_FF);
	fflush(stdout);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!tek_state.initialized)
		return;

	/* Exit to alpha mode */
	putchar(TEK_US);
	/* Reset terminal */
	printf("%c[?38l", TEK_ESC);  /* Exit Tek mode in xterm */
	fflush(stdout);

	memset(&tek_state, 0, sizeof(tek_state));
}

int getmaxx(void) { return tek_state.width - 1; }
int getmaxy(void) { return tek_state.height - 1; }

void setcolor(int color) {
	/* Tek 4014 is monochrome, but track color for compatibility */
	tek_state.current_color = color & 0x0F;
}

int getcolor(void) { return tek_state.current_color; }
void setbkcolor(int color) { tek_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return tek_state.current_bkcolor; }

void cleardevice(void) {
	putchar(TEK_ESC);
	putchar(TEK_FF);
	fflush(stdout);
	tek_state.pen_down = 0;
}

void putpixel(int x, int y, int color) {
	if (x < 0 || x >= tek_state.width || y < 0 || y >= tek_state.height)
		return;

	/* Draw a point by drawing to the same position twice */
	tek_move(x, y);
	tek_draw(x, y);
	fflush(stdout);
}

unsigned int getpixel(int x, int y) {
	/* Tek 4014 doesn't support pixel reading */
	return 0;
}

void moveto(int x, int y) {
	tek_state.current_x = x;
	tek_state.current_y = y;
	tek_move(x, tek_state.height - y - 1);  /* Flip Y coordinate */
	fflush(stdout);
}

void moverel(int dx, int dy) {
	moveto(tek_state.current_x + dx, tek_state.current_y + dy);
}

int getx(void) { return tek_state.current_x; }
int gety(void) { return tek_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	if (tek_state.line_pattern == 0xFFFF) {
		/* Solid line: use native Tek vector drawing */
		tek_move(x1, tek_state.height - y1 - 1);
		tek_draw(x2, tek_state.height - y2 - 1);
	} else {
		/* Patterned line: draw pixels */
		int dx = abs(x2 - x1), dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy, e2, pattern_pos = 0;

		while (1) {
			if (tek_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, tek_state.current_color);
			if (x1 == x2 && y1 == y2)
				break;
			e2 = 2 * err;
			if (e2 > -dy) { err -= dy; x1 += sx; }
			if (e2 < dx) { err += dx; y1 += sy; }
			pattern_pos++;
		}
	}
	fflush(stdout);
}

void lineto(int x, int y) {
	line(tek_state.current_x, tek_state.current_y, x, y);
	tek_state.current_x = x;
	tek_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = tek_state.current_x + dx;
	int y2 = tek_state.current_y + dy;
	line(tek_state.current_x, tek_state.current_y, x2, y2);
	tek_state.current_x = x2;
	tek_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int segments = 64, i;
	int x1, y1, x2, y2;

	x1 = x + radius;
	y1 = y;

	for (i = 1; i <= segments; i++) {
		float angle = 2.0f * 3.14159265f * i / segments;
		x2 = x + (int)(radius * cosf(angle));
		y2 = y + (int)(radius * sinf(angle));
		line(x1, y1, x2, y2);
		x1 = x2;
		y1 = y2;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	tek_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: tek_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: tek_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: tek_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: tek_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: tek_state.line_pattern = pattern; break;
	default: tek_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	tek_state.fill_style = pattern;
	tek_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	/* Fill rectangle with horizontal lines */
	int y;
	for (y = top; y <= bottom; y++)
		line(left, y, right, y);
}

void settextjustify(int horiz, int vert) {
	tek_state.text_justify_h = horiz;
	tek_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	return 14;  /* Tek 4014 character height */
}

int textwidth(const char *textstring) {
	return strlen(textstring) * 9;  /* Tek 4014 character width */
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
	return tek_state.initialized ? grOk : grNoInitGraph;
}
