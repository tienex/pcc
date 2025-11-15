/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Unicode Block Elements
 *
 * Provides BGI graphics using Unicode block drawing characters
 * Works on: Modern terminals with UTF-8 support (xterm, gnome-terminal, etc.)
 * Resolution: 160x100 pixels (using half-block characters)
 * Features: 16 colors using ANSI escape sequences
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>

/* Terminal dimensions */
#define TERM_WIDTH	80
#define TERM_HEIGHT	50
#define PIXEL_WIDTH	(TERM_WIDTH * 2)	/* 160 pixels */
#define PIXEL_HEIGHT	(TERM_HEIGHT * 2)	/* 100 pixels */

/* Unicode block characters (UTF-8) */
#define BLOCK_EMPTY	" "			/* Empty */
#define BLOCK_LOWER	"\u2584"		/* ▄ Lower half block */
#define BLOCK_UPPER	"\u2580"		/* ▀ Upper half block */
#define BLOCK_FULL	"\u2588"		/* █ Full block */

/* ANSI color codes */
static const int ansi_colors[16] = {
	30,	/* BLACK */
	34,	/* BLUE */
	32,	/* GREEN */
	36,	/* CYAN */
	31,	/* RED */
	35,	/* MAGENTA */
	33,	/* BROWN/YELLOW */
	37,	/* LIGHTGRAY */
	90,	/* DARKGRAY */
	94,	/* LIGHTBLUE */
	92,	/* LIGHTGREEN */
	96,	/* LIGHTCYAN */
	91,	/* LIGHTRED */
	95,	/* LIGHTMAGENTA */
	93,	/* YELLOW */
	97	/* WHITE */
};

/* Internal state */
static struct {
	unsigned char *pixels;		/* Pixel buffer */
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
} unicode_state;

static void
update_display(void)
{
	int col, row;
	int upper_pixel, lower_pixel;
	const char *block_char;
	int fg, bg;

	/* Clear screen and move cursor to home */
	printf("\033[2J\033[H");

	/* Render pixel buffer to terminal using Unicode blocks */
	for (row = 0; row < TERM_HEIGHT; row++) {
		for (col = 0; col < TERM_WIDTH; col++) {
			/* Get the two vertical pixels for this character cell */
			upper_pixel = unicode_state.pixels[row * 2 * PIXEL_WIDTH + col * 2];
			lower_pixel = unicode_state.pixels[(row * 2 + 1) * PIXEL_WIDTH + col * 2];

			/* Determine block character and colors */
			if (upper_pixel == 0 && lower_pixel == 0) {
				/* Both background */
				block_char = BLOCK_EMPTY;
				fg = unicode_state.current_bkcolor;
				bg = unicode_state.current_bkcolor;
			} else if (upper_pixel != 0 && lower_pixel == 0) {
				/* Upper pixel foreground, lower background */
				block_char = BLOCK_UPPER;
				fg = upper_pixel;
				bg = unicode_state.current_bkcolor;
			} else if (upper_pixel == 0 && lower_pixel != 0) {
				/* Lower pixel foreground, upper background */
				block_char = BLOCK_LOWER;
				fg = lower_pixel;
				bg = unicode_state.current_bkcolor;
			} else if (upper_pixel == lower_pixel) {
				/* Both same color */
				block_char = BLOCK_FULL;
				fg = upper_pixel;
				bg = upper_pixel;
			} else {
				/* Different colors - use upper for fg, lower for bg */
				block_char = BLOCK_UPPER;
				fg = upper_pixel;
				bg = lower_pixel;
			}

			/* Output with ANSI colors */
			printf("\033[%d;%dm%s", ansi_colors[fg & 0x0F], ansi_colors[bg & 0x0F] + 10, block_char);
		}

		/* Reset colors and newline */
		printf("\033[0m\n");
	}

	fflush(stdout);
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGALO;
	}

	/* Set UTF-8 locale */
	setlocale(LC_ALL, "");

	/* Allocate pixel buffer */
	unicode_state.pixels = (unsigned char *)calloc(PIXEL_WIDTH * PIXEL_HEIGHT, 1);

	if (!unicode_state.pixels) {
		*driver = grNotDetected;
		return;
	}

	unicode_state.width = PIXEL_WIDTH;
	unicode_state.height = PIXEL_HEIGHT;
	unicode_state.current_color = WHITE;
	unicode_state.current_bkcolor = BLACK;
	unicode_state.current_x = 0;
	unicode_state.current_y = 0;
	unicode_state.line_style = SOLID_LINE;
	unicode_state.line_pattern = 0xFFFF;
	unicode_state.fill_style = SOLID_FILL;
	unicode_state.fill_color = WHITE;
	unicode_state.text_justify_h = LEFT_TEXT;
	unicode_state.text_justify_v = TOP_TEXT;
	unicode_state.initialized = 1;

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!unicode_state.initialized)
		return;

	/* Output final image */
	update_display();

	if (unicode_state.pixels)
		free(unicode_state.pixels);

	/* Reset terminal */
	printf("\033[0m\033[2J\033[H");
	fflush(stdout);

	memset(&unicode_state, 0, sizeof(unicode_state));
}

int
getmaxx(void)
{
	return unicode_state.width - 1;
}

int
getmaxy(void)
{
	return unicode_state.height - 1;
}

void
setcolor(int color)
{
	unicode_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return unicode_state.current_color;
}

void
setbkcolor(int color)
{
	unicode_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return unicode_state.current_bkcolor;
}

void
cleardevice(void)
{
	memset(unicode_state.pixels, 0, PIXEL_WIDTH * PIXEL_HEIGHT);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < unicode_state.width && y >= 0 && y < unicode_state.height) {
		unicode_state.pixels[y * unicode_state.width + x] = color & 0x0F;
	}
}

unsigned int
getpixel(int x, int y)
{
	if (x < 0 || x >= unicode_state.width || y < 0 || y >= unicode_state.height)
		return 0;

	return unicode_state.pixels[y * unicode_state.width + x];
}

void
moveto(int x, int y)
{
	unicode_state.current_x = x;
	unicode_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	unicode_state.current_x += dx;
	unicode_state.current_y += dy;
}

int
getx(void)
{
	return unicode_state.current_x;
}

int
gety(void)
{
	return unicode_state.current_y;
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
		if (unicode_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, unicode_state.current_color);

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

	update_display();
}

void
lineto(int x, int y)
{
	line(unicode_state.current_x, unicode_state.current_y, x, y);
	unicode_state.current_x = x;
	unicode_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = unicode_state.current_x + dx;
	int y2 = unicode_state.current_y + dy;
	line(unicode_state.current_x, unicode_state.current_y, x2, y2);
	unicode_state.current_x = x2;
	unicode_state.current_y = y2;
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
		putpixel(x + dx, y + dy, unicode_state.current_color);
		putpixel(x - dx, y + dy, unicode_state.current_color);
		putpixel(x + dx, y - dy, unicode_state.current_color);
		putpixel(x - dx, y - dy, unicode_state.current_color);
		putpixel(x + dy, y + dx, unicode_state.current_color);
		putpixel(x - dy, y + dx, unicode_state.current_color);
		putpixel(x + dy, y - dx, unicode_state.current_color);
		putpixel(x - dy, y - dx, unicode_state.current_color);

		if (d < 0) {
			d += 2 * dx + 3;
		} else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}

	update_display();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	unicode_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		unicode_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		unicode_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		unicode_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		unicode_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		unicode_state.line_pattern = pattern;
		break;
	default:
		unicode_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	unicode_state.fill_style = pattern;
	unicode_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, unicode_state.fill_color);
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	unicode_state.text_justify_h = horiz;
	unicode_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 2;
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * 2;
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
	return unicode_state.initialized ? grOk : grNoInitGraph;
}
