/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using CP437/CP850 Block Drawing Characters
 *
 * Provides BGI graphics using IBM PC code page block characters
 * Works on: DOS, OS/2, Windows console, Linux console with CP437/850 font
 * Resolution: 160x50 pixels (2x vertical pixels per character cell)
 * Features: 16 colors via ANSI escape codes, platform-independent stdio
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Terminal dimensions */
#define TEXT_WIDTH	80
#define TEXT_HEIGHT	25
#define PIXEL_WIDTH	(TEXT_WIDTH * 2)	/* 160 pixels */
#define PIXEL_HEIGHT	(TEXT_HEIGHT * 2)	/* 50 pixels */

/* CP437/CP850 block characters (same in both code pages) */
#define CHAR_EMPTY	' '	/* Empty (0x20) */
#define CHAR_LOWER	0xDC	/* ▄ Lower half block */
#define CHAR_UPPER	0xDF	/* ▀ Upper half block */
#define CHAR_FULL	0xDB	/* █ Full block */

/* ANSI color codes for 16 EGA colors */
static const int ansi_colors[16] = {
	30,	/* BLACK */
	34,	/* BLUE */
	32,	/* GREEN */
	36,	/* CYAN */
	31,	/* RED */
	35,	/* MAGENTA */
	33,	/* BROWN (yellow) */
	37,	/* LIGHTGRAY (white) */
	90,	/* DARKGRAY (bright black) */
	94,	/* LIGHTBLUE (bright blue) */
	92,	/* LIGHTGREEN (bright green) */
	96,	/* LIGHTCYAN (bright cyan) */
	91,	/* LIGHTRED (bright red) */
	95,	/* LIGHTMAGENTA (bright magenta) */
	93,	/* YELLOW (bright yellow) */
	97	/* WHITE (bright white) */
};

/* Internal state */
static struct {
	unsigned char *pixels;		/* Pixel buffer (color indices 0-15) */
	unsigned char *screen;		/* Character buffer */
	unsigned char *fg_colors;	/* Foreground colors */
	unsigned char *bg_colors;	/* Background colors */
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
} cp437_state;

static void
update_char(int col, int row)
{
	int upper_pixel, lower_pixel;
	unsigned char ch;
	int fg, bg;

	/* Get the two vertical pixels for this character cell */
	upper_pixel = cp437_state.pixels[row * 2 * PIXEL_WIDTH + col * 2];
	lower_pixel = cp437_state.pixels[(row * 2 + 1) * PIXEL_WIDTH + col * 2];

	/* Determine character and colors based on pixel combination */
	if (upper_pixel == 0 && lower_pixel == 0) {
		/* Both pixels are background */
		ch = CHAR_EMPTY;
		fg = cp437_state.current_bkcolor;
		bg = cp437_state.current_bkcolor;
	} else if (upper_pixel != 0 && lower_pixel == 0) {
		/* Upper pixel set, lower clear - use upper half block */
		ch = CHAR_UPPER;
		fg = upper_pixel;
		bg = cp437_state.current_bkcolor;
	} else if (upper_pixel == 0 && lower_pixel != 0) {
		/* Upper clear, lower pixel set - use lower half block */
		ch = CHAR_LOWER;
		fg = lower_pixel;
		bg = cp437_state.current_bkcolor;
	} else if (upper_pixel == lower_pixel) {
		/* Both pixels same color - use full block */
		ch = CHAR_FULL;
		fg = upper_pixel;
		bg = cp437_state.current_bkcolor;
	} else {
		/* Different colors - use upper half block with both colors */
		ch = CHAR_UPPER;
		fg = upper_pixel;
		bg = lower_pixel;
	}

	/* Store character and colors */
	cp437_state.screen[row * TEXT_WIDTH + col] = ch;
	cp437_state.fg_colors[row * TEXT_WIDTH + col] = fg & 0x0F;
	cp437_state.bg_colors[row * TEXT_WIDTH + col] = bg & 0x0F;
}

static void
update_display(void)
{
	int col, row;
	int offset;

	/* Update all character cells from pixel buffer */
	for (row = 0; row < TEXT_HEIGHT; row++) {
		for (col = 0; col < TEXT_WIDTH; col++) {
			update_char(col, row);
		}
	}

	/* Clear screen and move cursor to home */
	printf("\033[2J\033[H");

	/* Output screen with ANSI colors */
	for (row = 0; row < TEXT_HEIGHT; row++) {
		for (col = 0; col < TEXT_WIDTH; col++) {
			offset = row * TEXT_WIDTH + col;

			/* Set ANSI colors: ESC[foreground;backgroundm */
			printf("\033[%d;%dm",
			       ansi_colors[cp437_state.fg_colors[offset]],
			       ansi_colors[cp437_state.bg_colors[offset]] + 10);

			/* Output character */
			putchar(cp437_state.screen[offset]);
		}
		putchar('\n');
	}

	/* Reset colors */
	printf("\033[0m");
	fflush(stdout);
}

void
initgraph(int *driver, int *mode, const char *path)
{
	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGALO;
	}

	/* Allocate pixel buffer */
	cp437_state.pixels = (unsigned char *)calloc(PIXEL_WIDTH * PIXEL_HEIGHT, 1);
	if (!cp437_state.pixels) {
		*driver = grNotDetected;
		return;
	}

	/* Allocate screen buffers */
	cp437_state.screen = (unsigned char *)malloc(TEXT_WIDTH * TEXT_HEIGHT);
	cp437_state.fg_colors = (unsigned char *)malloc(TEXT_WIDTH * TEXT_HEIGHT);
	cp437_state.bg_colors = (unsigned char *)malloc(TEXT_WIDTH * TEXT_HEIGHT);

	if (!cp437_state.screen || !cp437_state.fg_colors || !cp437_state.bg_colors) {
		free(cp437_state.pixels);
		free(cp437_state.screen);
		free(cp437_state.fg_colors);
		free(cp437_state.bg_colors);
		*driver = grNotDetected;
		return;
	}

	/* Initialize screen to empty */
	memset(cp437_state.screen, CHAR_EMPTY, TEXT_WIDTH * TEXT_HEIGHT);
	memset(cp437_state.fg_colors, WHITE, TEXT_WIDTH * TEXT_HEIGHT);
	memset(cp437_state.bg_colors, BLACK, TEXT_WIDTH * TEXT_HEIGHT);

	cp437_state.width = PIXEL_WIDTH;
	cp437_state.height = PIXEL_HEIGHT;
	cp437_state.current_color = WHITE;
	cp437_state.current_bkcolor = BLACK;
	cp437_state.current_x = 0;
	cp437_state.current_y = 0;
	cp437_state.line_style = SOLID_LINE;
	cp437_state.line_pattern = 0xFFFF;
	cp437_state.fill_style = SOLID_FILL;
	cp437_state.fill_color = WHITE;
	cp437_state.text_justify_h = LEFT_TEXT;
	cp437_state.text_justify_v = TOP_TEXT;
	cp437_state.initialized = 1;

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!cp437_state.initialized)
		return;

	/* Output final image */
	update_display();

	/* Free buffers */
	if (cp437_state.pixels)
		free(cp437_state.pixels);
	if (cp437_state.screen)
		free(cp437_state.screen);
	if (cp437_state.fg_colors)
		free(cp437_state.fg_colors);
	if (cp437_state.bg_colors)
		free(cp437_state.bg_colors);

	/* Reset terminal */
	printf("\033[0m\033[2J\033[H");
	fflush(stdout);

	memset(&cp437_state, 0, sizeof(cp437_state));
}

int
getmaxx(void)
{
	return cp437_state.width - 1;
}

int
getmaxy(void)
{
	return cp437_state.height - 1;
}

void
setcolor(int color)
{
	cp437_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return cp437_state.current_color;
}

void
setbkcolor(int color)
{
	cp437_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return cp437_state.current_bkcolor;
}

void
cleardevice(void)
{
	memset(cp437_state.pixels, 0, PIXEL_WIDTH * PIXEL_HEIGHT);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < cp437_state.width && y >= 0 && y < cp437_state.height) {
		cp437_state.pixels[y * cp437_state.width + x] = color & 0x0F;
	}
}

unsigned int
getpixel(int x, int y)
{
	if (x < 0 || x >= cp437_state.width || y < 0 || y >= cp437_state.height)
		return 0;

	return cp437_state.pixels[y * cp437_state.width + x];
}

void
moveto(int x, int y)
{
	cp437_state.current_x = x;
	cp437_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	cp437_state.current_x += dx;
	cp437_state.current_y += dy;
}

int
getx(void)
{
	return cp437_state.current_x;
}

int
gety(void)
{
	return cp437_state.current_y;
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
		if (cp437_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, cp437_state.current_color);

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
	line(cp437_state.current_x, cp437_state.current_y, x, y);
	cp437_state.current_x = x;
	cp437_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = cp437_state.current_x + dx;
	int y2 = cp437_state.current_y + dy;
	line(cp437_state.current_x, cp437_state.current_y, x2, y2);
	cp437_state.current_x = x2;
	cp437_state.current_y = y2;
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
		putpixel(x + dx, y + dy, cp437_state.current_color);
		putpixel(x - dx, y + dy, cp437_state.current_color);
		putpixel(x + dx, y - dy, cp437_state.current_color);
		putpixel(x - dx, y - dy, cp437_state.current_color);
		putpixel(x + dy, y + dx, cp437_state.current_color);
		putpixel(x - dy, y + dx, cp437_state.current_color);
		putpixel(x + dy, y - dx, cp437_state.current_color);
		putpixel(x - dy, y - dx, cp437_state.current_color);

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
	cp437_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		cp437_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		cp437_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		cp437_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		cp437_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		cp437_state.line_pattern = pattern;
		break;
	default:
		cp437_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	cp437_state.fill_style = pattern;
	cp437_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, cp437_state.fill_color);
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	cp437_state.text_justify_h = horiz;
	cp437_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 2;	/* 2 pixels high in our text mode */
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
	return cp437_state.initialized ? grOk : grNoInitGraph;
}
