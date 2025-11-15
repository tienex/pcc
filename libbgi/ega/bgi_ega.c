/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for EGA (Enhanced Graphics Adapter)
 *
 * Provides BGI graphics on DOS with EGA hardware
 * Works on: DOS, IBM PC/XT/AT with EGA card
 * Resolution: 640x350, 16 colors
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* EGA constants */
#define EGA_WIDTH		640
#define EGA_HEIGHT		350
#define EGA_SEGMENT		0xA000
#define EGA_MODE		0x10	/* 640x350x16 */
#define TEXT_MODE		0x03

/* EGA I/O ports */
#define EGA_SEQUENCER_INDEX	0x3C4
#define EGA_SEQUENCER_DATA	0x3C5
#define EGA_GRAPHICS_INDEX	0x3CE
#define EGA_GRAPHICS_DATA	0x3CF

/* EGA Sequencer registers */
#define SEQ_MAP_MASK		0x02

/* EGA Graphics registers */
#define GFX_SET_RESET		0x00
#define GFX_ENABLE_SET_RESET	0x01
#define GFX_DATA_ROTATE		0x03
#define GFX_READ_MAP_SELECT	0x04
#define GFX_MODE		0x05
#define GFX_MISC		0x06
#define GFX_COLOR_DONT_CARE	0x07
#define GFX_BIT_MASK		0x08

/* Internal state */
static struct {
	unsigned char FAR *ega_mem;	/* EGA memory pointer */
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
} ega_state;

static void
set_video_mode(unsigned char mode)
{
	union REGS regs;
	regs.h.ah = 0x00;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

static void
ega_write_pixel(int x, int y, int color)
{
	unsigned long offset;
	unsigned char mask;

	if (x < 0 || x >= EGA_WIDTH || y < 0 || y >= EGA_HEIGHT)
		return;

	/* Calculate offset and bit mask */
	offset = (unsigned long)y * (EGA_WIDTH / 8) + (x / 8);
	mask = 0x80 >> (x & 7);

	/* Set bit mask */
	outp(EGA_GRAPHICS_INDEX, GFX_BIT_MASK);
	outp(EGA_GRAPHICS_DATA, mask);

	/* Set color */
	outp(EGA_GRAPHICS_INDEX, GFX_SET_RESET);
	outp(EGA_GRAPHICS_DATA, color);

	/* Enable set/reset */
	outp(EGA_GRAPHICS_INDEX, GFX_ENABLE_SET_RESET);
	outp(EGA_GRAPHICS_DATA, 0x0F);

	/* Write to video memory (value doesn't matter) */
	ega_state.ega_mem[offset] |= 0xFF;

	/* Restore defaults */
	outp(EGA_GRAPHICS_INDEX, GFX_BIT_MASK);
	outp(EGA_GRAPHICS_DATA, 0xFF);
	outp(EGA_GRAPHICS_INDEX, GFX_ENABLE_SET_RESET);
	outp(EGA_GRAPHICS_DATA, 0x00);
}

static int
ega_read_pixel(int x, int y)
{
	unsigned long offset;
	unsigned char mask, plane;
	int color = 0;

	if (x < 0 || x >= EGA_WIDTH || y < 0 || y >= EGA_HEIGHT)
		return 0;

	offset = (unsigned long)y * (EGA_WIDTH / 8) + (x / 8);
	mask = 0x80 >> (x & 7);

	/* Read from each plane */
	for (plane = 0; plane < 4; plane++) {
		outp(EGA_GRAPHICS_INDEX, GFX_READ_MAP_SELECT);
		outp(EGA_GRAPHICS_DATA, plane);

		if (ega_state.ega_mem[offset] & mask)
			color |= (1 << plane);
	}

	return color;
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = EGA;
		*mode = EGAHI;
	}

	/* Set EGA mode 640x350x16 */
	set_video_mode(EGA_MODE);

	/* Get pointer to EGA memory */
#ifdef __WATCOMC__
	ega_state.ega_mem = (unsigned char FAR *)MK_FP(EGA_SEGMENT, 0);
#else
	ega_state.ega_mem = (unsigned char FAR *)((unsigned long)EGA_SEGMENT << 16);
#endif

	/* Initialize graphics mode */
	outp(EGA_GRAPHICS_INDEX, GFX_MODE);
	outp(EGA_GRAPHICS_DATA, 0x00);	/* Write mode 0, read mode 0 */

	outp(EGA_SEQUENCER_INDEX, SEQ_MAP_MASK);
	outp(EGA_SEQUENCER_DATA, 0x0F);	/* Enable all planes */

	ega_state.current_color = WHITE;
	ega_state.current_bkcolor = BLACK;
	ega_state.current_x = 0;
	ega_state.current_y = 0;
	ega_state.line_style = SOLID_LINE;
	ega_state.line_pattern = 0xFFFF;
	ega_state.fill_style = SOLID_FILL;
	ega_state.fill_color = WHITE;
	ega_state.text_justify_h = LEFT_TEXT;
	ega_state.text_justify_v = TOP_TEXT;
	ega_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!ega_state.initialized)
		return;

	/* Return to text mode */
	set_video_mode(TEXT_MODE);

	memset(&ega_state, 0, sizeof(ega_state));
}

int
getmaxx(void)
{
	return EGA_WIDTH - 1;
}

int
getmaxy(void)
{
	return EGA_HEIGHT - 1;
}

void
setcolor(int color)
{
	ega_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return ega_state.current_color;
}

void
setbkcolor(int color)
{
	ega_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return ega_state.current_bkcolor;
}

void
cleardevice(void)
{
	int x, y;

	/* Clear screen by writing background color to all pixels */
	for (y = 0; y < EGA_HEIGHT; y++) {
		for (x = 0; x < EGA_WIDTH; x++) {
			ega_write_pixel(x, y, ega_state.current_bkcolor);
		}
	}
}

void
putpixel(int x, int y, int color)
{
	ega_write_pixel(x, y, color);
}

unsigned int
getpixel(int x, int y)
{
	return ega_read_pixel(x, y);
}

void
moveto(int x, int y)
{
	ega_state.current_x = x;
	ega_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	ega_state.current_x += dx;
	ega_state.current_y += dy;
}

int
getx(void)
{
	return ega_state.current_x;
}

int
gety(void)
{
	return ega_state.current_y;
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
		if (ega_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, ega_state.current_color);

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
	line(ega_state.current_x, ega_state.current_y, x, y);
	ega_state.current_x = x;
	ega_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = ega_state.current_x + dx;
	int y2 = ega_state.current_y + dy;
	line(ega_state.current_x, ega_state.current_y, x2, y2);
	ega_state.current_x = x2;
	ega_state.current_y = y2;
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
		putpixel(x + dx, y + dy, ega_state.current_color);
		putpixel(x - dx, y + dy, ega_state.current_color);
		putpixel(x + dx, y - dy, ega_state.current_color);
		putpixel(x - dx, y - dy, ega_state.current_color);
		putpixel(x + dy, y + dx, ega_state.current_color);
		putpixel(x - dy, y + dx, ega_state.current_color);
		putpixel(x + dy, y - dx, ega_state.current_color);
		putpixel(x - dy, y - dx, ega_state.current_color);

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
	ega_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		ega_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		ega_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		ega_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		ega_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		ega_state.line_pattern = pattern;
		break;
	default:
		ega_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	ega_state.fill_style = pattern;
	ega_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, ega_state.fill_color);
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	ega_state.text_justify_h = horiz;
	ega_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 14;	/* EGA 8x14 font */
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
	return ega_state.initialized ? grOk : grNoInitGraph;
}
