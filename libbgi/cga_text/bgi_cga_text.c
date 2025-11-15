/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using CGA Text Mode (80x25) as Graphics
 *
 * Provides BGI graphics using IBM PC text mode with block characters
 * Works on: DOS, ANSI terminals, VGA text mode
 * Resolution: 160x50 pixels (using half-block characters)
 * Features: 16 foreground colors, 8 background colors, CP437 block chars
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <conio.h>
#include <dos.h>

/* Text mode screen size */
#define TEXT_WIDTH	80
#define TEXT_HEIGHT	25
#define PIXEL_WIDTH	(TEXT_WIDTH * 2)	/* 160 pixels wide */
#define PIXEL_HEIGHT	(TEXT_HEIGHT * 2)	/* 50 pixels high */

/* CP437 block characters */
#define CHAR_EMPTY	' '	/* Empty */
#define CHAR_LOWER	0xDC	/* ▄ Lower half block */
#define CHAR_UPPER	0xDF	/* ▀ Upper half block */
#define CHAR_FULL	0xDB	/* █ Full block */

/* Internal state */
static struct {
	unsigned char *screen;		/* 80x25 character buffer */
	unsigned char *colors;		/* 80x25 color attribute buffer */
	unsigned char *pixels;		/* 160x50 pixel buffer */
	int current_color;
	int current_bkcolor;
	int current_x;
	int current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style;
	int fill_color;
	int text_justify_h;
	int text_justify_v;
	int initialized;
} cgatext_state;

static void
update_char(int col, int row)
{
	int upper_pixel, lower_pixel;
	unsigned char ch, attr;
	int fg, bg;

	if (col < 0 || col >= TEXT_WIDTH || row < 0 || row >= TEXT_HEIGHT)
		return;

	/* Get the two vertical pixels for this character cell */
	upper_pixel = cgatext_state.pixels[row * 2 * PIXEL_WIDTH + col * 2];
	lower_pixel = cgatext_state.pixels[(row * 2 + 1) * PIXEL_WIDTH + col * 2];

	/* Determine character and colors based on pixel combination */
	if (upper_pixel == 0 && lower_pixel == 0) {
		/* Both pixels are background */
		ch = CHAR_EMPTY;
		fg = cgatext_state.current_bkcolor;
		bg = cgatext_state.current_bkcolor;
	} else if (upper_pixel != 0 && lower_pixel == 0) {
		/* Upper pixel is foreground, lower is background */
		ch = CHAR_UPPER;
		fg = upper_pixel;
		bg = cgatext_state.current_bkcolor;
	} else if (upper_pixel == 0 && lower_pixel != 0) {
		/* Lower pixel is foreground, upper is background */
		ch = CHAR_LOWER;
		fg = lower_pixel;
		bg = cgatext_state.current_bkcolor;
	} else if (upper_pixel == lower_pixel) {
		/* Both pixels same color */
		ch = CHAR_FULL;
		fg = upper_pixel;
		bg = upper_pixel;
	} else {
		/* Different colors - use upper for fg, lower for bg */
		ch = CHAR_UPPER;
		fg = upper_pixel;
		bg = lower_pixel;
	}

	/* Store character and attribute */
	cgatext_state.screen[row * TEXT_WIDTH + col] = ch;
	cgatext_state.colors[row * TEXT_WIDTH + col] = (bg << 4) | (fg & 0x0F);
}

static void
update_display(void)
{
	int col, row;
	unsigned char far *video;

	/* Update all character cells */
	for (row = 0; row < TEXT_HEIGHT; row++) {
		for (col = 0; col < TEXT_WIDTH; col++) {
			update_char(col, row);
		}
	}

	/* Write to video memory (CGA/VGA text mode at 0xB800:0000) */
	video = (unsigned char far *)MK_FP(0xB800, 0);
	for (row = 0; row < TEXT_HEIGHT; row++) {
		for (col = 0; col < TEXT_WIDTH; col++) {
			int offset = (row * TEXT_WIDTH + col) * 2;
			video[offset] = cgatext_state.screen[row * TEXT_WIDTH + col];
			video[offset + 1] = cgatext_state.colors[row * TEXT_WIDTH + col];
		}
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	union REGS regs;

	if (*driver == DETECT) {
		*driver = CGA;
		*mode = CGAC0;
	}

	/* Set text mode 80x25 color */
	regs.h.ah = 0x00;
	regs.h.al = 0x03;	/* 80x25 16-color text */
	int86(0x10, &regs, &regs);

	/* Allocate buffers */
	cgatext_state.screen = (unsigned char *)malloc(TEXT_WIDTH * TEXT_HEIGHT);
	cgatext_state.colors = (unsigned char *)malloc(TEXT_WIDTH * TEXT_HEIGHT);
	cgatext_state.pixels = (unsigned char *)calloc(PIXEL_WIDTH * PIXEL_HEIGHT, 1);

	if (!cgatext_state.screen || !cgatext_state.colors || !cgatext_state.pixels) {
		if (cgatext_state.screen) free(cgatext_state.screen);
		if (cgatext_state.colors) free(cgatext_state.colors);
		if (cgatext_state.pixels) free(cgatext_state.pixels);
		*driver = grNotDetected;
		return;
	}

	cgatext_state.current_color = WHITE;
	cgatext_state.current_bkcolor = BLACK;
	cgatext_state.current_x = 0;
	cgatext_state.current_y = 0;
	cgatext_state.line_style = SOLID_LINE;
	cgatext_state.line_pattern = 0xFFFF;
	cgatext_state.fill_style = SOLID_FILL;
	cgatext_state.fill_color = WHITE;
	cgatext_state.text_justify_h = LEFT_TEXT;
	cgatext_state.text_justify_v = TOP_TEXT;
	cgatext_state.initialized = 1;

	/* Hide cursor */
	regs.h.ah = 0x01;
	regs.h.ch = 0x20;	/* Bit 5 = hide cursor */
	regs.h.cl = 0x00;
	int86(0x10, &regs, &regs);

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	union REGS regs;

	if (!cgatext_state.initialized)
		return;

	/* Restore cursor */
	regs.h.ah = 0x01;
	regs.h.ch = 0x06;
	regs.h.cl = 0x07;
	int86(0x10, &regs, &regs);

	/* Clear screen */
	regs.h.ah = 0x00;
	regs.h.al = 0x03;
	int86(0x10, &regs, &regs);

	if (cgatext_state.screen) free(cgatext_state.screen);
	if (cgatext_state.colors) free(cgatext_state.colors);
	if (cgatext_state.pixels) free(cgatext_state.pixels);

	memset(&cgatext_state, 0, sizeof(cgatext_state));
}

int
getmaxx(void)
{
	return PIXEL_WIDTH - 1;
}

int
getmaxy(void)
{
	return PIXEL_HEIGHT - 1;
}

void
setcolor(int color)
{
	cgatext_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return cgatext_state.current_color;
}

void
setbkcolor(int color)
{
	cgatext_state.current_bkcolor = color & 0x07;	/* Only 8 background colors */
}

int
getbkcolor(void)
{
	return cgatext_state.current_bkcolor;
}

void
cleardevice(void)
{
	memset(cgatext_state.pixels, 0, PIXEL_WIDTH * PIXEL_HEIGHT);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < PIXEL_WIDTH && y >= 0 && y < PIXEL_HEIGHT) {
		cgatext_state.pixels[y * PIXEL_WIDTH + x] = color & 0x0F;
		/* Update the character cell containing this pixel */
		update_char(x / 2, y / 2);
		/* Partial update - just write this one character */
		{
			unsigned char far *video = (unsigned char far *)MK_FP(0xB800, 0);
			int row = y / 2;
			int col = x / 2;
			int offset = (row * TEXT_WIDTH + col) * 2;
			video[offset] = cgatext_state.screen[row * TEXT_WIDTH + col];
			video[offset + 1] = cgatext_state.colors[row * TEXT_WIDTH + col];
		}
	}
}

unsigned int
getpixel(int x, int y)
{
	if (x < 0 || x >= PIXEL_WIDTH || y < 0 || y >= PIXEL_HEIGHT)
		return 0;

	return cgatext_state.pixels[y * PIXEL_WIDTH + x];
}

void
moveto(int x, int y)
{
	cgatext_state.current_x = x;
	cgatext_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	cgatext_state.current_x += dx;
	cgatext_state.current_y += dy;
}

int
getx(void)
{
	return cgatext_state.current_x;
}

int
gety(void)
{
	return cgatext_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	/* Bresenham's line algorithm */
	int dx = abs(x2 - x1);
	int dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1;
	int sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy;
	int e2;
	int pattern_pos = 0;

	while (1) {
		if (cgatext_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, cgatext_state.current_color);

		if (x1 == x2 && y1 == y2)
			break;

		e2 = 2 * err;
		if (e2 > -dy) {
			err -= dy;
			x1 += sx;
		}
		if (e2 < dx) {
			err += dx;
			y1 += sy;
		}

		pattern_pos++;
	}
}

void
lineto(int x, int y)
{
	line(cgatext_state.current_x, cgatext_state.current_y, x, y);
	cgatext_state.current_x = x;
	cgatext_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = cgatext_state.current_x + dx;
	int y2 = cgatext_state.current_y + dy;
	line(cgatext_state.current_x, cgatext_state.current_y, x2, y2);
	cgatext_state.current_x = x2;
	cgatext_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void
circle(int x, int y, int radius)
{
	/* Midpoint circle algorithm */
	int dx = 0;
	int dy = radius;
	int d = 1 - radius;

	while (dx <= dy) {
		putpixel(x + dx, y + dy, cgatext_state.current_color);
		putpixel(x - dx, y + dy, cgatext_state.current_color);
		putpixel(x + dx, y - dy, cgatext_state.current_color);
		putpixel(x - dx, y - dy, cgatext_state.current_color);
		putpixel(x + dy, y + dx, cgatext_state.current_color);
		putpixel(x - dy, y + dx, cgatext_state.current_color);
		putpixel(x + dy, y - dx, cgatext_state.current_color);
		putpixel(x - dy, y - dx, cgatext_state.current_color);

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	cgatext_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		cgatext_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		cgatext_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		cgatext_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		cgatext_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		cgatext_state.line_pattern = pattern;
		break;
	default:
		cgatext_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	cgatext_state.fill_style = pattern;
	cgatext_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, cgatext_state.fill_color);
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	cgatext_state.text_justify_h = horiz;
	cgatext_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 2;	/* 2 pixels per character row */
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * 2;	/* 2 pixels per character */
}

const char *
grapherrormsg(int errorcode)
{
	switch (errorcode) {
	case grOk:
		return "No error";
	case grNoInitGraph:
		return "Graphics not initialized";
	case grNotDetected:
		return "Graphics hardware not detected";
	case grFileNotFound:
		return "Driver file not found";
	case grInvalidDriver:
		return "Invalid graphics driver";
	case grNoLoadMem:
		return "Insufficient memory to load driver";
	default:
		return "Unknown error";
	}
}

int
graphresult(void)
{
	return cgatext_state.initialized ? grOk : grNoInitGraph;
}
