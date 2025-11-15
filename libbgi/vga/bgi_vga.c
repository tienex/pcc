/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using VGA Mode 13h (320x200x256)
 *
 * Provides BGI graphics on DOS via direct VGA hardware access
 * Works on: DOS (real mode, protected mode), DOSEMU, DOSBox
 * Requires: VGA-compatible graphics adapter
 */

#include "../graphics.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* VGA Mode 13h constants */
#define VGA_WIDTH		320
#define VGA_HEIGHT		200
#define VGA_SEGMENT		0xA000
#define VGA_MODE_13H		0x13
#define TEXT_MODE		0x03

/* VGA I/O ports */
#define VGA_INPUT_STATUS	0x3DA
#define VGA_PALETTE_MASK	0x3C6
#define VGA_PALETTE_READ	0x3C7
#define VGA_PALETTE_WRITE	0x3C8
#define VGA_PALETTE_DATA	0x3C9

/* Internal state */
static struct {
	unsigned char FAR *vga_mem;	/* VGA memory pointer */
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
} vga_state;

/* EGA palette (first 16 colors of VGA) */
static const unsigned char ega_palette[16][3] = {
	{0x00, 0x00, 0x00},	/* BLACK */
	{0x00, 0x00, 0xAA},	/* BLUE */
	{0x00, 0xAA, 0x00},	/* GREEN */
	{0x00, 0xAA, 0xAA},	/* CYAN */
	{0xAA, 0x00, 0x00},	/* RED */
	{0xAA, 0x00, 0xAA},	/* MAGENTA */
	{0xAA, 0x55, 0x00},	/* BROWN */
	{0xAA, 0xAA, 0xAA},	/* LIGHTGRAY */
	{0x55, 0x55, 0x55},	/* DARKGRAY */
	{0x55, 0x55, 0xFF},	/* LIGHTBLUE */
	{0x55, 0xFF, 0x55},	/* LIGHTGREEN */
	{0x55, 0xFF, 0xFF},	/* LIGHTCYAN */
	{0xFF, 0x55, 0x55},	/* LIGHTRED */
	{0xFF, 0x55, 0xFF},	/* LIGHTMAGENTA */
	{0xFF, 0xFF, 0x55},	/* YELLOW */
	{0xFF, 0xFF, 0xFF}	/* WHITE */
};

static void
set_vga_mode(unsigned char mode)
{
	union REGS regs;
	regs.h.ah = 0x00;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

static void
set_vga_palette(void)
{
	int i;

	/* Set EGA palette */
	for (i = 0; i < 16; i++) {
		outp(VGA_PALETTE_WRITE, i);
		outp(VGA_PALETTE_DATA, ega_palette[i][0] >> 2);	/* VGA uses 6-bit DAC */
		outp(VGA_PALETTE_DATA, ega_palette[i][1] >> 2);
		outp(VGA_PALETTE_DATA, ega_palette[i][2] >> 2);
	}

	/* Fill remaining colors with gradient */
	for (i = 16; i < 256; i++) {
		outp(VGA_PALETTE_WRITE, i);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGALO;
	}

	/* Set VGA Mode 13h */
	set_vga_mode(VGA_MODE_13H);

	/* Get pointer to VGA memory */
#ifdef __WATCOMC__
	vga_state.vga_mem = (unsigned char FAR *)MK_FP(VGA_SEGMENT, 0);
#else
	vga_state.vga_mem = (unsigned char FAR *)((unsigned long)VGA_SEGMENT << 16);
#endif

	/* Set up EGA palette */
	set_vga_palette();

	vga_state.current_color = WHITE;
	vga_state.current_bkcolor = BLACK;
	vga_state.current_x = 0;
	vga_state.current_y = 0;
	vga_state.line_style = SOLID_LINE;
	vga_state.line_pattern = 0xFFFF;
	vga_state.fill_style = SOLID_FILL;
	vga_state.fill_color = WHITE;
	vga_state.text_justify_h = LEFT_TEXT;
	vga_state.text_justify_v = TOP_TEXT;
	vga_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!vga_state.initialized)
		return;

	/* Return to text mode */
	set_vga_mode(TEXT_MODE);

	memset(&vga_state, 0, sizeof(vga_state));
}

int
getmaxx(void)
{
	return VGA_WIDTH - 1;
}

int
getmaxy(void)
{
	return VGA_HEIGHT - 1;
}

void
setcolor(int color)
{
	vga_state.current_color = color & 0xFF;
}

int
getcolor(void)
{
	return vga_state.current_color;
}

void
setbkcolor(int color)
{
	vga_state.current_bkcolor = color & 0xFF;
}

int
getbkcolor(void)
{
	return vga_state.current_bkcolor;
}

void
cleardevice(void)
{
	unsigned int i;
	unsigned char color = (unsigned char)vga_state.current_bkcolor;

	/* Fill VGA memory with background color */
	for (i = 0; i < VGA_WIDTH * VGA_HEIGHT; i++)
		vga_state.vga_mem[i] = color;
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < VGA_WIDTH && y >= 0 && y < VGA_HEIGHT) {
		vga_state.vga_mem[y * VGA_WIDTH + x] = (unsigned char)color;
	}
}

unsigned int
getpixel(int x, int y)
{
	if (x < 0 || x >= VGA_WIDTH || y < 0 || y >= VGA_HEIGHT)
		return 0;

	return vga_state.vga_mem[y * VGA_WIDTH + x];
}

void
moveto(int x, int y)
{
	vga_state.current_x = x;
	vga_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	vga_state.current_x += dx;
	vga_state.current_y += dy;
}

int
getx(void)
{
	return vga_state.current_x;
}

int
gety(void)
{
	return vga_state.current_y;
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
		if (vga_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, vga_state.current_color);

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
	line(vga_state.current_x, vga_state.current_y, x, y);
	vga_state.current_x = x;
	vga_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = vga_state.current_x + dx;
	int y2 = vga_state.current_y + dy;
	line(vga_state.current_x, vga_state.current_y, x2, y2);
	vga_state.current_x = x2;
	vga_state.current_y = y2;
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
		putpixel(x + dx, y + dy, vga_state.current_color);
		putpixel(x - dx, y + dy, vga_state.current_color);
		putpixel(x + dx, y - dy, vga_state.current_color);
		putpixel(x - dx, y - dy, vga_state.current_color);
		putpixel(x + dy, y + dx, vga_state.current_color);
		putpixel(x - dy, y + dx, vga_state.current_color);
		putpixel(x + dy, y - dx, vga_state.current_color);
		putpixel(x - dy, y - dx, vga_state.current_color);

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
	vga_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		vga_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		vga_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		vga_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		vga_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		vga_state.line_pattern = pattern;
		break;
	default:
		vga_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	vga_state.fill_style = pattern;
	vga_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;
	unsigned char color = (unsigned char)vga_state.fill_color;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			if (x >= 0 && x < VGA_WIDTH && y >= 0 && y < VGA_HEIGHT)
				vga_state.vga_mem[y * VGA_WIDTH + x] = color;
		}
	}
}

void
settextjustify(int horiz, int vert)
{
	vga_state.text_justify_h = horiz;
	vga_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 8; /* Default 8x8 font */
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
	return vga_state.initialized ? grOk : grNoInitGraph;
}
