/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using SIXEL graphics (DEC VT340)
 *
 * Provides BGI graphics in terminals supporting SIXEL
 * Works on: xterm, mintty, mlterm, WezTerm, foot, and VT340-compatible terminals
 * Requires: Terminal with SIXEL support
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

/* Internal state */
static struct {
	unsigned char *pixels;	/* RGB pixel buffer */
	int width;
	int height;
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
} sixel_state;

/* EGA color palette (RGB) */
static const unsigned char ega_rgb[16][3] = {
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
output_sixel(void)
{
	int x, y, c, bit;
	unsigned char sixel_char;
	int color_map[256];
	int num_colors = 0;
	int i, j;

	/* Clear screen and move cursor to home */
	printf("\033[2J\033[H");

	/* Start SIXEL sequence */
	printf("\033Pq");

	/* Define color palette */
	for (i = 0; i < 16; i++) {
		/* #<color>;<mode>;<r>;<g>;<b> */
		printf("#%d;2;%d;%d;%d",
		       i,
		       (ega_rgb[i][0] * 100) / 255,
		       (ega_rgb[i][1] * 100) / 255,
		       (ega_rgb[i][2] * 100) / 255);
	}

	/* Output pixel data */
	/* SIXEL format: 6 pixels vertically per character */
	for (y = 0; y < sixel_state.height; y += 6) {
		for (c = 0; c < 16; c++) {
			/* Select color */
			printf("#%d", c);

			for (x = 0; x < sixel_state.width; x++) {
				sixel_char = 0;

				/* Pack 6 vertical pixels into one SIXEL character */
				for (bit = 0; bit < 6 && (y + bit) < sixel_state.height; bit++) {
					int offset = ((y + bit) * sixel_state.width + x) * 3;
					unsigned char r = sixel_state.pixels[offset];
					unsigned char g = sixel_state.pixels[offset + 1];
					unsigned char b = sixel_state.pixels[offset + 2];

					/* Find matching color */
					for (i = 0; i < 16; i++) {
						if (r == ega_rgb[i][0] &&
						    g == ega_rgb[i][1] &&
						    b == ega_rgb[i][2]) {
							if (i == c)
								sixel_char |= (1 << bit);
							break;
						}
					}
				}

				/* Output SIXEL character (63 + 6-bit value) */
				if (sixel_char != 0)
					putchar(sixel_char + 63);
			}

			/* Carriage return to start of line */
			putchar('$');
		}

		/* Line feed to next SIXEL row */
		putchar('-');
	}

	/* End SIXEL sequence */
	printf("\033\\");
	fflush(stdout);
}

void
initgraph(int *driver, int *mode, const char *path)
{
	int width = 640;
	int height = 480;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Determine resolution based on mode */
	switch (*mode) {
	case VGALO:
		width = 640;
		height = 200;
		break;
	case VGAMED:
		width = 640;
		height = 350;
		break;
	case VGAHI:
		width = 640;
		height = 480;
		break;
	case SVGA_800_600:
		width = 800;
		height = 600;
		break;
	default:
		/* Limit size for terminal */
		width = 640;
		height = 480;
	}

	/* Allocate pixel buffer (RGB) */
	sixel_state.pixels = (unsigned char *)malloc(width * height * 3);
	if (!sixel_state.pixels) {
		*driver = grNotDetected;
		return;
	}

	sixel_state.width = width;
	sixel_state.height = height;
	sixel_state.current_color = WHITE;
	sixel_state.current_bkcolor = BLACK;
	sixel_state.current_x = 0;
	sixel_state.current_y = 0;
	sixel_state.line_style = SOLID_LINE;
	sixel_state.line_pattern = 0xFFFF;
	sixel_state.fill_style = SOLID_FILL;
	sixel_state.fill_color = WHITE;
	sixel_state.text_justify_h = LEFT_TEXT;
	sixel_state.text_justify_v = TOP_TEXT;
	sixel_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!sixel_state.initialized)
		return;

	/* Output final image */
	output_sixel();

	if (sixel_state.pixels)
		free(sixel_state.pixels);

	/* Reset terminal */
	printf("\033[0m\033[2J\033[H");
	fflush(stdout);

	memset(&sixel_state, 0, sizeof(sixel_state));
}

int
getmaxx(void)
{
	return sixel_state.width - 1;
}

int
getmaxy(void)
{
	return sixel_state.height - 1;
}

void
setcolor(int color)
{
	sixel_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return sixel_state.current_color;
}

void
setbkcolor(int color)
{
	sixel_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return sixel_state.current_bkcolor;
}

void
cleardevice(void)
{
	int i;
	int total_pixels = sixel_state.width * sixel_state.height * 3;
	unsigned char r = ega_rgb[sixel_state.current_bkcolor][0];
	unsigned char g = ega_rgb[sixel_state.current_bkcolor][1];
	unsigned char b = ega_rgb[sixel_state.current_bkcolor][2];

	for (i = 0; i < total_pixels; i += 3) {
		sixel_state.pixels[i] = r;
		sixel_state.pixels[i + 1] = g;
		sixel_state.pixels[i + 2] = b;
	}

	output_sixel();
}

void
putpixel(int x, int y, int color)
{
	int offset;

	if (x >= 0 && x < sixel_state.width && y >= 0 && y < sixel_state.height) {
		offset = (y * sixel_state.width + x) * 3;
		sixel_state.pixels[offset] = ega_rgb[color & 0x0F][0];
		sixel_state.pixels[offset + 1] = ega_rgb[color & 0x0F][1];
		sixel_state.pixels[offset + 2] = ega_rgb[color & 0x0F][2];
	}
}

unsigned int
getpixel(int x, int y)
{
	int offset;
	unsigned char r, g, b;
	int i;

	if (x < 0 || x >= sixel_state.width || y < 0 || y >= sixel_state.height)
		return 0;

	offset = (y * sixel_state.width + x) * 3;
	r = sixel_state.pixels[offset];
	g = sixel_state.pixels[offset + 1];
	b = sixel_state.pixels[offset + 2];

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (r == ega_rgb[i][0] && g == ega_rgb[i][1] && b == ega_rgb[i][2])
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	sixel_state.current_x = x;
	sixel_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	sixel_state.current_x += dx;
	sixel_state.current_y += dy;
}

int
getx(void)
{
	return sixel_state.current_x;
}

int
gety(void)
{
	return sixel_state.current_y;
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
		if (sixel_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, sixel_state.current_color);

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

	output_sixel();
}

void
lineto(int x, int y)
{
	line(sixel_state.current_x, sixel_state.current_y, x, y);
	sixel_state.current_x = x;
	sixel_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = sixel_state.current_x + dx;
	int y2 = sixel_state.current_y + dy;
	line(sixel_state.current_x, sixel_state.current_y, x2, y2);
	sixel_state.current_x = x2;
	sixel_state.current_y = y2;
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
		putpixel(x + dx, y + dy, sixel_state.current_color);
		putpixel(x - dx, y + dy, sixel_state.current_color);
		putpixel(x + dx, y - dy, sixel_state.current_color);
		putpixel(x - dx, y - dy, sixel_state.current_color);
		putpixel(x + dy, y + dx, sixel_state.current_color);
		putpixel(x - dy, y + dx, sixel_state.current_color);
		putpixel(x + dy, y - dx, sixel_state.current_color);
		putpixel(x - dy, y - dx, sixel_state.current_color);

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}

	output_sixel();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	sixel_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		sixel_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		sixel_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		sixel_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		sixel_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		sixel_state.line_pattern = pattern;
		break;
	default:
		sixel_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	sixel_state.fill_style = pattern;
	sixel_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, sixel_state.fill_color);
		}
	}

	output_sixel();
}

void
settextjustify(int horiz, int vert)
{
	sixel_state.text_justify_h = horiz;
	sixel_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 8;
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
	return sixel_state.initialized ? grOk : grNoInitGraph;
}
