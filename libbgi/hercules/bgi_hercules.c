/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Hercules Graphics Card
 *
 * Provides BGI graphics on DOS with Hercules card
 * Works on: DOS, IBM PC with Hercules Graphics Card
 * Resolution: 720x348, monochrome
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Hercules constants */
#define HERC_WIDTH		720
#define HERC_HEIGHT		348
#define HERC_SEGMENT		0xB000
#define HERC_BYTES_PER_LINE	90	/* 720 / 8 */

/* Hercules I/O ports */
#define HERC_INDEX		0x3B4
#define HERC_DATA		0x3B5
#define HERC_CONFIG		0x3B8
#define HERC_STATUS		0x3BA

/* Internal state */
static struct {
	unsigned char FAR *herc_mem;	/* Hercules memory pointer */
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
} herc_state;

/* Hercules CRTC values for graphics mode */
static const unsigned char herc_graphics_mode[12] = {
	0x35,	/* Horizontal total */
	0x2D,	/* Horizontal displayed */
	0x2E,	/* Horizontal sync position */
	0x07,	/* Horizontal sync width */
	0x5B,	/* Vertical total */
	0x02,	/* Vertical total adjust */
	0x57,	/* Vertical displayed */
	0x57,	/* Vertical sync position */
	0x02,	/* Interlace mode */
	0x03,	/* Maximum scan line */
	0x00,	/* Cursor start */
	0x00	/* Cursor end */
};

static void
set_hercules_graphics_mode(void)
{
	int i;

	/* Disable video */
	outp(HERC_CONFIG, 0x00);

	/* Set CRTC registers for graphics mode */
	for (i = 0; i < 12; i++) {
		outp(HERC_INDEX, i);
		outp(HERC_DATA, herc_graphics_mode[i]);
	}

	/* Enable graphics mode */
	outp(HERC_CONFIG, 0x0A);	/* Graphics mode, video enabled */
}

static void
set_hercules_text_mode(void)
{
	/* Disable video */
	outp(HERC_CONFIG, 0x00);

	/* Reset to text mode (let BIOS handle it) */
	outp(HERC_CONFIG, 0x28);	/* Text mode, video enabled */
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = HERCMONO;
		*mode = HERCMONOHI;
	}

	/* Set Hercules graphics mode */
	set_hercules_graphics_mode();

	/* Get pointer to Hercules memory */
#ifdef __WATCOMC__
	herc_state.herc_mem = (unsigned char FAR *)MK_FP(HERC_SEGMENT, 0);
#else
	herc_state.herc_mem = (unsigned char FAR *)((unsigned long)HERC_SEGMENT << 16);
#endif

	herc_state.current_color = 1;	/* White (on) */
	herc_state.current_bkcolor = 0;	/* Black (off) */
	herc_state.current_x = 0;
	herc_state.current_y = 0;
	herc_state.line_style = SOLID_LINE;
	herc_state.line_pattern = 0xFFFF;
	herc_state.fill_style = SOLID_FILL;
	herc_state.fill_color = 1;
	herc_state.text_justify_h = LEFT_TEXT;
	herc_state.text_justify_v = TOP_TEXT;
	herc_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!herc_state.initialized)
		return;

	/* Return to text mode */
	set_hercules_text_mode();

	memset(&herc_state, 0, sizeof(herc_state));
}

int
getmaxx(void)
{
	return HERC_WIDTH - 1;
}

int
getmaxy(void)
{
	return HERC_HEIGHT - 1;
}

void
setcolor(int color)
{
	herc_state.current_color = (color != 0) ? 1 : 0;
}

int
getcolor(void)
{
	return herc_state.current_color;
}

void
setbkcolor(int color)
{
	herc_state.current_bkcolor = (color != 0) ? 1 : 0;
}

int
getbkcolor(void)
{
	return herc_state.current_bkcolor;
}

void
cleardevice(void)
{
	unsigned int i;
	unsigned char fill_value = herc_state.current_bkcolor ? 0xFF : 0x00;

	/* Clear all 32KB of Hercules video memory */
	for (i = 0; i < 0x8000; i++)
		herc_state.herc_mem[i] = fill_value;
}

void
putpixel(int x, int y, int color)
{
	unsigned int offset;
	unsigned char mask, bit;
	unsigned int bank;

	if (x < 0 || x >= HERC_WIDTH || y < 0 || y >= HERC_HEIGHT)
		return;

	/* Hercules has 4 interlaced banks of 8KB each */
	bank = y & 3;
	offset = (y >> 2) * HERC_BYTES_PER_LINE + (x >> 3) + (bank * 0x2000);
	bit = 7 - (x & 7);
	mask = 1 << bit;

	if (color)
		herc_state.herc_mem[offset] |= mask;
	else
		herc_state.herc_mem[offset] &= ~mask;
}

unsigned int
getpixel(int x, int y)
{
	unsigned int offset;
	unsigned char bit;
	unsigned int bank;

	if (x < 0 || x >= HERC_WIDTH || y < 0 || y >= HERC_HEIGHT)
		return 0;

	bank = y & 3;
	offset = (y >> 2) * HERC_BYTES_PER_LINE + (x >> 3) + (bank * 0x2000);
	bit = 7 - (x & 7);

	return (herc_state.herc_mem[offset] & (1 << bit)) ? 1 : 0;
}

void
moveto(int x, int y)
{
	herc_state.current_x = x;
	herc_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	herc_state.current_x += dx;
	herc_state.current_y += dy;
}

int
getx(void)
{
	return herc_state.current_x;
}

int
gety(void)
{
	return herc_state.current_y;
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
		/* Check line pattern for styled lines */
		if (herc_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, herc_state.current_color);

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
	line(herc_state.current_x, herc_state.current_y, x, y);
	herc_state.current_x = x;
	herc_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = herc_state.current_x + dx;
	int y2 = herc_state.current_y + dy;
	line(herc_state.current_x, herc_state.current_y, x2, y2);
	herc_state.current_x = x2;
	herc_state.current_y = y2;
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
		putpixel(x + dx, y + dy, herc_state.current_color);
		putpixel(x - dx, y + dy, herc_state.current_color);
		putpixel(x + dx, y - dy, herc_state.current_color);
		putpixel(x - dx, y - dy, herc_state.current_color);
		putpixel(x + dy, y + dx, herc_state.current_color);
		putpixel(x - dy, y + dx, herc_state.current_color);
		putpixel(x + dy, y - dx, herc_state.current_color);
		putpixel(x - dy, y - dx, herc_state.current_color);

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
	herc_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		herc_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		herc_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		herc_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		herc_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		herc_state.line_pattern = pattern;
		break;
	default:
		herc_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	herc_state.fill_style = pattern;
	herc_state.fill_color = (color != 0) ? 1 : 0;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, herc_state.fill_color);
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	herc_state.text_justify_h = horiz;
	herc_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 14;	/* Hercules 9x14 font */
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * 9;
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
	return herc_state.initialized ? grOk : grNoInitGraph;
}
