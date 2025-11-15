/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for CGA (Color Graphics Adapter)
 *
 * Provides BGI graphics on DOS with CGA hardware
 * Works on: DOS, IBM PC with CGA card
 * Resolution: 320x200, 4 colors (mode 4)
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* CGA constants */
#define CGA_WIDTH		320
#define CGA_HEIGHT		200
#define CGA_SEGMENT		0xB800
#define CGA_MODE_4		0x04	/* 320x200x4 */
#define CGA_MODE_6		0x06	/* 640x200x2 */
#define TEXT_MODE		0x03

/* CGA palettes */
#define CGA_PALETTE_0		0	/* Black, Green, Red, Brown */
#define CGA_PALETTE_1		1	/* Black, Cyan, Magenta, White */

/* Internal state */
static struct {
	unsigned char FAR *cga_mem;	/* CGA memory pointer */
	int current_color;
	int current_bkcolor;
	int current_palette;
	int current_x;
	int current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style;
	int fill_color;
	int text_justify_h;
	int text_justify_v;
	int initialized;
} cga_state;

/* CGA color mapping to palette colors */
static const unsigned char palette_0_map[4] = {0, 2, 4, 6};	/* Black, Green, Red, Brown */
static const unsigned char palette_1_map[4] = {0, 3, 5, 7};	/* Black, Cyan, Magenta, White */

static void
set_video_mode(unsigned char mode)
{
	union REGS regs;
	regs.h.ah = 0x00;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

static void
set_cga_palette(int palette)
{
	union REGS regs;
	regs.h.ah = 0x0B;
	regs.h.bh = 0x01;	/* Set palette */
	regs.h.bl = palette;
	int86(0x10, &regs, &regs);
}

static void
set_cga_background(int color)
{
	union REGS regs;
	regs.h.ah = 0x0B;
	regs.h.bh = 0x00;	/* Set background */
	regs.h.bl = color & 0x0F;
	int86(0x10, &regs, &regs);
}

static unsigned char
map_color_to_cga(int color)
{
	const unsigned char *palette_map;

	if (cga_state.current_palette == CGA_PALETTE_0)
		palette_map = palette_0_map;
	else
		palette_map = palette_1_map;

	/* Map BGI color to CGA palette color (0-3) */
	switch (color & 0x0F) {
	case BLACK:
		return 0;
	case BLUE:
	case CYAN:
		return 1;
	case GREEN:
	case RED:
	case MAGENTA:
		return 2;
	case BROWN:
	case LIGHTGRAY:
	case DARKGRAY:
	case LIGHTBLUE:
	case LIGHTGREEN:
	case LIGHTCYAN:
	case LIGHTRED:
	case LIGHTMAGENTA:
	case YELLOW:
	case WHITE:
		return 3;
	default:
		return 0;
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = CGA;
		*mode = CGAC0;
	}

	/* Set CGA mode 320x200x4 */
	set_video_mode(CGA_MODE_4);

	/* Get pointer to CGA memory */
#ifdef __WATCOMC__
	cga_state.cga_mem = (unsigned char FAR *)MK_FP(CGA_SEGMENT, 0);
#else
	cga_state.cga_mem = (unsigned char FAR *)((unsigned long)CGA_SEGMENT << 16);
#endif

	/* Set palette 1 (cyan, magenta, white) by default */
	cga_state.current_palette = CGA_PALETTE_1;
	set_cga_palette(cga_state.current_palette);

	cga_state.current_color = 3;	/* White in palette */
	cga_state.current_bkcolor = BLACK;
	cga_state.current_x = 0;
	cga_state.current_y = 0;
	cga_state.line_style = SOLID_LINE;
	cga_state.line_pattern = 0xFFFF;
	cga_state.fill_style = SOLID_FILL;
	cga_state.fill_color = 3;
	cga_state.text_justify_h = LEFT_TEXT;
	cga_state.text_justify_v = TOP_TEXT;
	cga_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!cga_state.initialized)
		return;

	/* Return to text mode */
	set_video_mode(TEXT_MODE);

	memset(&cga_state, 0, sizeof(cga_state));
}

int
getmaxx(void)
{
	return CGA_WIDTH - 1;
}

int
getmaxy(void)
{
	return CGA_HEIGHT - 1;
}

void
setcolor(int color)
{
	cga_state.current_color = map_color_to_cga(color);
}

int
getcolor(void)
{
	return cga_state.current_color;
}

void
setbkcolor(int color)
{
	cga_state.current_bkcolor = color & 0x0F;
	set_cga_background(cga_state.current_bkcolor);
}

int
getbkcolor(void)
{
	return cga_state.current_bkcolor;
}

void
cleardevice(void)
{
	unsigned int i;

	/* Clear CGA memory */
	for (i = 0; i < 0x4000; i++)	/* 16KB video memory */
		cga_state.cga_mem[i] = 0;
}

void
putpixel(int x, int y, int color)
{
	unsigned int offset;
	unsigned char pixel_color, mask, shift;
	unsigned char old_value;

	if (x < 0 || x >= CGA_WIDTH || y < 0 || y >= CGA_HEIGHT)
		return;

	pixel_color = map_color_to_cga(color);

	/* CGA memory layout: even scanlines at 0x0000, odd at 0x2000 */
	offset = (y & 1) * 0x2000 + (y >> 1) * (CGA_WIDTH / 4) + (x >> 2);

	/* 4 pixels per byte, 2 bits each */
	shift = 6 - ((x & 3) << 1);
	mask = ~(3 << shift);

	/* Read-modify-write */
	old_value = cga_state.cga_mem[offset];
	cga_state.cga_mem[offset] = (old_value & mask) | (pixel_color << shift);
}

unsigned int
getpixel(int x, int y)
{
	unsigned int offset;
	unsigned char shift;
	unsigned char value;

	if (x < 0 || x >= CGA_WIDTH || y < 0 || y >= CGA_HEIGHT)
		return 0;

	/* CGA memory layout */
	offset = (y & 1) * 0x2000 + (y >> 1) * (CGA_WIDTH / 4) + (x >> 2);

	shift = 6 - ((x & 3) << 1);
	value = (cga_state.cga_mem[offset] >> shift) & 3;

	return value;
}

void
moveto(int x, int y)
{
	cga_state.current_x = x;
	cga_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	cga_state.current_x += dx;
	cga_state.current_y += dy;
}

int
getx(void)
{
	return cga_state.current_x;
}

int
gety(void)
{
	return cga_state.current_y;
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
		if (cga_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, cga_state.current_color);

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
	line(cga_state.current_x, cga_state.current_y, x, y);
	cga_state.current_x = x;
	cga_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = cga_state.current_x + dx;
	int y2 = cga_state.current_y + dy;
	line(cga_state.current_x, cga_state.current_y, x2, y2);
	cga_state.current_x = x2;
	cga_state.current_y = y2;
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
		putpixel(x + dx, y + dy, cga_state.current_color);
		putpixel(x - dx, y + dy, cga_state.current_color);
		putpixel(x + dx, y - dy, cga_state.current_color);
		putpixel(x - dx, y - dy, cga_state.current_color);
		putpixel(x + dy, y + dx, cga_state.current_color);
		putpixel(x - dy, y + dx, cga_state.current_color);
		putpixel(x + dy, y - dx, cga_state.current_color);
		putpixel(x - dy, y - dx, cga_state.current_color);

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
	cga_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		cga_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		cga_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		cga_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		cga_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		cga_state.line_pattern = pattern;
		break;
	default:
		cga_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	cga_state.fill_style = pattern;
	cga_state.fill_color = map_color_to_cga(color);
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, cga_state.fill_color);
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	cga_state.text_justify_h = horiz;
	cga_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 8;	/* CGA 8x8 font */
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * 8;
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
	return cga_state.initialized ? grOk : grNoInitGraph;
}
