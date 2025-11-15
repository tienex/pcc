/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Unicode Braille Patterns
 *
 * Provides high-resolution BGI graphics using Unicode Braille characters
 * Works on: Modern terminals with UTF-8 support
 * Resolution: 160x100 pixels (2x4 dots per Braille character)
 * Features: Highest resolution text-mode graphics, monochrome
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <locale.h>

/* Terminal dimensions */
#define TERM_WIDTH	80
#define TERM_HEIGHT	25
#define DOTS_PER_CHAR_X	2
#define DOTS_PER_CHAR_Y	4
#define PIXEL_WIDTH	(TERM_WIDTH * DOTS_PER_CHAR_X)		/* 160 pixels */
#define PIXEL_HEIGHT	(TERM_HEIGHT * DOTS_PER_CHAR_Y)	/* 100 pixels */

/* Braille Unicode base: U+2800 */
#define BRAILLE_BASE	0x2800

/* Braille dot positions (Unicode standard):
 * 0 3
 * 1 4
 * 2 5
 * 6 7
 */
static const unsigned char braille_map[2][4] = {
	{0, 1, 2, 6},	/* Left column */
	{3, 4, 5, 7}	/* Right column */
};

/* Internal state */
static struct {
	unsigned char *pixels;		/* Pixel buffer (monochrome) */
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
} braille_state;

static wchar_t
pixels_to_braille(int col, int row)
{
	int x, y;
	unsigned int braille_char = BRAILLE_BASE;

	/* Encode 2x4 pixel block into Braille character */
	for (y = 0; y < 4; y++) {
		for (x = 0; x < 2; x++) {
			int pixel_x = col * 2 + x;
			int pixel_y = row * 4 + y;

			if (pixel_x < PIXEL_WIDTH && pixel_y < PIXEL_HEIGHT) {
				if (braille_state.pixels[pixel_y * PIXEL_WIDTH + pixel_x]) {
					braille_char |= (1 << braille_map[x][y]);
				}
			}
		}
	}

	return (wchar_t)braille_char;
}

static void
update_display(void)
{
	int col, row;
	wchar_t braille_char;
	char utf8[5];

	/* Clear screen and move cursor to home */
	printf("\033[2J\033[H");

	/* Render pixel buffer to terminal using Braille characters */
	for (row = 0; row < TERM_HEIGHT; row++) {
		for (col = 0; col < TERM_WIDTH; col++) {
			braille_char = pixels_to_braille(col, row);

			/* Convert wide char to UTF-8 */
			if (braille_char == BRAILLE_BASE) {
				/* Empty Braille = space */
				putchar(' ');
			} else {
				/* Convert to UTF-8 and output */
				int len = wctomb(utf8, braille_char);
				if (len > 0) {
					utf8[len] = '\0';
					printf("%s", utf8);
				} else {
					putchar(' ');
				}
			}
		}
		putchar('\n');
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

	/* Allocate pixel buffer (monochrome: 0 or 1) */
	braille_state.pixels = (unsigned char *)calloc(PIXEL_WIDTH * PIXEL_HEIGHT, 1);

	if (!braille_state.pixels) {
		*driver = grNotDetected;
		return;
	}

	braille_state.width = PIXEL_WIDTH;
	braille_state.height = PIXEL_HEIGHT;
	braille_state.current_color = WHITE;
	braille_state.current_bkcolor = BLACK;
	braille_state.current_x = 0;
	braille_state.current_y = 0;
	braille_state.line_style = SOLID_LINE;
	braille_state.line_pattern = 0xFFFF;
	braille_state.fill_style = SOLID_FILL;
	braille_state.fill_color = WHITE;
	braille_state.text_justify_h = LEFT_TEXT;
	braille_state.text_justify_v = TOP_TEXT;
	braille_state.initialized = 1;

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!braille_state.initialized)
		return;

	/* Output final image */
	update_display();

	if (braille_state.pixels)
		free(braille_state.pixels);

	/* Reset terminal */
	printf("\033[0m\033[2J\033[H");
	fflush(stdout);

	memset(&braille_state, 0, sizeof(braille_state));
}

int
getmaxx(void)
{
	return braille_state.width - 1;
}

int
getmaxy(void)
{
	return braille_state.height - 1;
}

void
setcolor(int color)
{
	braille_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return braille_state.current_color;
}

void
setbkcolor(int color)
{
	braille_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return braille_state.current_bkcolor;
}

void
cleardevice(void)
{
	memset(braille_state.pixels, 0, PIXEL_WIDTH * PIXEL_HEIGHT);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < braille_state.width && y >= 0 && y < braille_state.height) {
		/* Monochrome: any non-black color becomes white */
		braille_state.pixels[y * braille_state.width + x] = (color != BLACK) ? 1 : 0;
	}
}

unsigned int
getpixel(int x, int y)
{
	if (x < 0 || x >= braille_state.width || y < 0 || y >= braille_state.height)
		return 0;

	return braille_state.pixels[y * braille_state.width + x] ? WHITE : BLACK;
}

void
moveto(int x, int y)
{
	braille_state.current_x = x;
	braille_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	braille_state.current_x += dx;
	braille_state.current_y += dy;
}

int
getx(void)
{
	return braille_state.current_x;
}

int
gety(void)
{
	return braille_state.current_y;
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
		if (braille_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, braille_state.current_color);

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
	line(braille_state.current_x, braille_state.current_y, x, y);
	braille_state.current_x = x;
	braille_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = braille_state.current_x + dx;
	int y2 = braille_state.current_y + dy;
	line(braille_state.current_x, braille_state.current_y, x2, y2);
	braille_state.current_x = x2;
	braille_state.current_y = y2;
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
		putpixel(x + dx, y + dy, braille_state.current_color);
		putpixel(x - dx, y + dy, braille_state.current_color);
		putpixel(x + dx, y - dy, braille_state.current_color);
		putpixel(x - dx, y - dy, braille_state.current_color);
		putpixel(x + dy, y + dx, braille_state.current_color);
		putpixel(x - dy, y + dx, braille_state.current_color);
		putpixel(x + dy, y - dx, braille_state.current_color);
		putpixel(x - dy, y - dx, braille_state.current_color);

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
	braille_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		braille_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		braille_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		braille_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		braille_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		braille_state.line_pattern = pattern;
		break;
	default:
		braille_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	braille_state.fill_style = pattern;
	braille_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, braille_state.fill_color);
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	braille_state.text_justify_h = horiz;
	braille_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 4;
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
	return braille_state.initialized ? grOk : grNoInitGraph;
}
