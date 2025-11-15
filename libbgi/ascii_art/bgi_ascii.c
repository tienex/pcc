/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using ASCII Art (Luminance-Based Graphics)
 *
 * Provides BGI graphics using ASCII characters based on luminance
 * Works on: Any text terminal, telnet, SSH, serial console
 * Resolution: 80x24 (standard terminal size)
 * Features: Grayscale rendering using character luminance
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Terminal dimensions */
#define TERM_WIDTH	80
#define TERM_HEIGHT	24

/* ASCII luminance ramp (darkest to brightest) */
static const char luminance_chars[] = " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$";
#define NUM_LUMINANCE_LEVELS (sizeof(luminance_chars) - 1)

/* Internal state */
static struct {
	unsigned char *pixels;		/* Grayscale pixel buffer */
	char *screen;			/* ASCII character buffer */
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
} ascii_state;

/* EGA color to grayscale luminance conversion */
static unsigned char
color_to_luminance(int color)
{
	/* Convert EGA color to grayscale based on perceived luminance */
	static const unsigned char luminance_table[16] = {
		0,	/* BLACK */
		29,	/* BLUE (0.114 * 255) */
		150,	/* GREEN (0.587 * 255) */
		179,	/* CYAN */
		76,	/* RED (0.299 * 255) */
		105,	/* MAGENTA */
		226,	/* BROWN/YELLOW */
		170,	/* LIGHTGRAY */
		85,	/* DARKGRAY */
		114,	/* LIGHTBLUE */
		235,	/* LIGHTGREEN */
		255,	/* LIGHTCYAN */
		161,	/* LIGHTRED */
		190,	/* LIGHTMAGENTA */
		255,	/* YELLOW */
		255	/* WHITE */
	};

	return luminance_table[color & 0x0F];
}

static char
luminance_to_char(unsigned char luminance)
{
	int index = (luminance * (NUM_LUMINANCE_LEVELS - 1)) / 255;
	return luminance_chars[index];
}

static void
update_display(void)
{
	int x, y, px, py;
	unsigned long total;
	int count;
	unsigned char avg_luminance;

	/* Clear screen and move cursor to home */
	printf("\033[2J\033[H");

	/* Convert pixel buffer to ASCII art */
	for (y = 0; y < TERM_HEIGHT; y++) {
		for (x = 0; x < TERM_WIDTH; x++) {
			/* Average luminance of the pixels in this character cell */
			total = 0;
			count = 0;

			for (py = 0; py < (ascii_state.height / TERM_HEIGHT); py++) {
				for (px = 0; px < (ascii_state.width / TERM_WIDTH); px++) {
					int pixel_x = x * (ascii_state.width / TERM_WIDTH) + px;
					int pixel_y = y * (ascii_state.height / TERM_HEIGHT) + py;

					if (pixel_x < ascii_state.width && pixel_y < ascii_state.height) {
						total += ascii_state.pixels[pixel_y * ascii_state.width + pixel_x];
						count++;
					}
				}
			}

			avg_luminance = (count > 0) ? (total / count) : 0;
			ascii_state.screen[y * TERM_WIDTH + x] = luminance_to_char(avg_luminance);
		}
	}

	/* Output ASCII art */
	for (y = 0; y < TERM_HEIGHT; y++) {
		for (x = 0; x < TERM_WIDTH; x++) {
			putchar(ascii_state.screen[y * TERM_WIDTH + x]);
		}
		putchar('\n');
	}

	fflush(stdout);
}

void
initgraph(int *driver, int *mode, const char *path)
{
	int width = 160;	/* 2x terminal width for better resolution */
	int height = 48;	/* 2x terminal height */

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGALO;
	}

	/* Allocate buffers */
	ascii_state.pixels = (unsigned char *)calloc(width * height, 1);
	ascii_state.screen = (char *)malloc(TERM_WIDTH * TERM_HEIGHT);

	if (!ascii_state.pixels || !ascii_state.screen) {
		if (ascii_state.pixels) free(ascii_state.pixels);
		if (ascii_state.screen) free(ascii_state.screen);
		*driver = grNotDetected;
		return;
	}

	ascii_state.width = width;
	ascii_state.height = height;
	ascii_state.current_color = WHITE;
	ascii_state.current_bkcolor = BLACK;
	ascii_state.current_x = 0;
	ascii_state.current_y = 0;
	ascii_state.line_style = SOLID_LINE;
	ascii_state.line_pattern = 0xFFFF;
	ascii_state.fill_style = SOLID_FILL;
	ascii_state.fill_color = WHITE;
	ascii_state.text_justify_h = LEFT_TEXT;
	ascii_state.text_justify_v = TOP_TEXT;
	ascii_state.initialized = 1;

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!ascii_state.initialized)
		return;

	/* Output final image */
	update_display();

	if (ascii_state.pixels) free(ascii_state.pixels);
	if (ascii_state.screen) free(ascii_state.screen);

	/* Reset terminal */
	printf("\033[0m");
	fflush(stdout);

	memset(&ascii_state, 0, sizeof(ascii_state));
}

int
getmaxx(void)
{
	return ascii_state.width - 1;
}

int
getmaxy(void)
{
	return ascii_state.height - 1;
}

void
setcolor(int color)
{
	ascii_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return ascii_state.current_color;
}

void
setbkcolor(int color)
{
	ascii_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return ascii_state.current_bkcolor;
}

void
cleardevice(void)
{
	unsigned char bk_luminance = color_to_luminance(ascii_state.current_bkcolor);
	memset(ascii_state.pixels, bk_luminance, ascii_state.width * ascii_state.height);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < ascii_state.width && y >= 0 && y < ascii_state.height) {
		ascii_state.pixels[y * ascii_state.width + x] = color_to_luminance(color);
	}
}

unsigned int
getpixel(int x, int y)
{
	unsigned char luminance;
	int i;
	int best_match = BLACK;
	int min_diff = 255;

	if (x < 0 || x >= ascii_state.width || y < 0 || y >= ascii_state.height)
		return 0;

	luminance = ascii_state.pixels[y * ascii_state.width + x];

	/* Find closest EGA color */
	for (i = 0; i < 16; i++) {
		int diff = abs(color_to_luminance(i) - luminance);
		if (diff < min_diff) {
			min_diff = diff;
			best_match = i;
		}
	}

	return best_match;
}

void
moveto(int x, int y)
{
	ascii_state.current_x = x;
	ascii_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	ascii_state.current_x += dx;
	ascii_state.current_y += dy;
}

int
getx(void)
{
	return ascii_state.current_x;
}

int
gety(void)
{
	return ascii_state.current_y;
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
		if (ascii_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, ascii_state.current_color);

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
	line(ascii_state.current_x, ascii_state.current_y, x, y);
	ascii_state.current_x = x;
	ascii_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = ascii_state.current_x + dx;
	int y2 = ascii_state.current_y + dy;
	line(ascii_state.current_x, ascii_state.current_y, x2, y2);
	ascii_state.current_x = x2;
	ascii_state.current_y = y2;
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
		putpixel(x + dx, y + dy, ascii_state.current_color);
		putpixel(x - dx, y + dy, ascii_state.current_color);
		putpixel(x + dx, y - dy, ascii_state.current_color);
		putpixel(x - dx, y - dy, ascii_state.current_color);
		putpixel(x + dy, y + dx, ascii_state.current_color);
		putpixel(x - dy, y + dx, ascii_state.current_color);
		putpixel(x + dy, y - dx, ascii_state.current_color);
		putpixel(x - dy, y - dx, ascii_state.current_color);

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
	ascii_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		ascii_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		ascii_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		ascii_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		ascii_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		ascii_state.line_pattern = pattern;
		break;
	default:
		ascii_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	ascii_state.fill_style = pattern;
	ascii_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			putpixel(x, y, ascii_state.fill_color);
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	ascii_state.text_justify_h = horiz;
	ascii_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return ascii_state.height / TERM_HEIGHT;
}

int
textwidth(const char *textstring)
{
	return strlen(textstring) * (ascii_state.width / TERM_WIDTH);
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
	return ascii_state.initialized ? grOk : grNoInitGraph;
}
