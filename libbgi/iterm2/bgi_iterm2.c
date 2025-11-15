/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using iTerm2 inline images protocol
 *
 * Terminal graphics using iTerm2's inline image protocol
 * Works on: iTerm2 (macOS), WezTerm, Konsole (with support)
 * Features: 24-bit color, base64-encoded PNG output
 * Output: iTerm2 escape sequences to stdout
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

static struct {
	uint8_t *framebuffer;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	uint32_t colors[16];
	int initialized;
} iterm_state;

/* EGA palette in RGB888 format */
static const uint32_t ega_rgb888[16] = {
	0x000000,  /* BLACK */
	0x0000AA,  /* BLUE */
	0x00AA00,  /* GREEN */
	0x00AAAA,  /* CYAN */
	0xAA0000,  /* RED */
	0xAA00AA,  /* MAGENTA */
	0xAA5500,  /* BROWN */
	0xAAAAAA,  /* LIGHTGRAY */
	0x555555,  /* DARKGRAY */
	0x5555FF,  /* LIGHTBLUE */
	0x55FF55,  /* LIGHTGREEN */
	0x55FFFF,  /* LIGHTCYAN */
	0xFF5555,  /* LIGHTRED */
	0xFF55FF,  /* LIGHTMAGENTA */
	0xFFFF55,  /* YELLOW */
	0xFFFFFF   /* WHITE */
};

static const char base64_chars[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static void base64_encode(const uint8_t *input, size_t length, char *output) {
	size_t i, j;
	for (i = 0, j = 0; i < length;) {
		uint32_t octet_a = i < length ? input[i++] : 0;
		uint32_t octet_b = i < length ? input[i++] : 0;
		uint32_t octet_c = i < length ? input[i++] : 0;
		uint32_t triple = (octet_a << 16) + (octet_b << 8) + octet_c;

		output[j++] = base64_chars[(triple >> 18) & 0x3F];
		output[j++] = base64_chars[(triple >> 12) & 0x3F];
		output[j++] = base64_chars[(triple >> 6) & 0x3F];
		output[j++] = base64_chars[triple & 0x3F];
	}

	for (i = 0; i < (3 - length % 3) % 3; i++)
		output[j - 1 - i] = '=';
	output[j] = '\0';
}

static void display_framebuffer(void) {
	/* Simple PPM format converted to base64 for display */
	size_t ppm_size = 20 + iterm_state.width * iterm_state.height * 3;
	uint8_t *ppm_data = malloc(ppm_size);
	char *b64_data = malloc(ppm_size * 2);
	size_t offset = 0;
	int x, y;

	if (!ppm_data || !b64_data) {
		free(ppm_data);
		free(b64_data);
		return;
	}

	/* Create PPM header */
	offset = sprintf((char *)ppm_data, "P6\n%d %d\n255\n",
	                 iterm_state.width, iterm_state.height);

	/* Write pixel data (RGB) */
	for (y = 0; y < iterm_state.height; y++) {
		for (x = 0; x < iterm_state.width; x++) {
			uint8_t color_idx = iterm_state.framebuffer[y * iterm_state.width + x];
			uint32_t rgb = iterm_state.colors[color_idx & 0x0F];
			ppm_data[offset++] = (rgb >> 16) & 0xFF;  /* R */
			ppm_data[offset++] = (rgb >> 8) & 0xFF;   /* G */
			ppm_data[offset++] = rgb & 0xFF;          /* B */
		}
	}

	/* Encode to base64 */
	base64_encode(ppm_data, offset, b64_data);

	/* Output iTerm2 inline image escape sequence */
	printf("\x1B]1337;File=inline=1;width=%dpx;height=%dpx:",
	       iterm_state.width, iterm_state.height);
	printf("%s", b64_data);
	printf("\a\n");
	fflush(stdout);

	free(ppm_data);
	free(b64_data);
}

void initgraph(int *driver, int *mode, const char *path) {
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Allocate framebuffer */
	iterm_state.framebuffer = malloc(width * height);
	if (!iterm_state.framebuffer) {
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		iterm_state.colors[i] = ega_rgb888[i];

	iterm_state.width = width;
	iterm_state.height = height;
	iterm_state.current_color = WHITE;
	iterm_state.current_bkcolor = BLACK;
	iterm_state.current_x = 0;
	iterm_state.current_y = 0;
	iterm_state.line_style = SOLID_LINE;
	iterm_state.line_pattern = 0xFFFF;
	iterm_state.fill_style = SOLID_FILL;
	iterm_state.fill_color = WHITE;
	iterm_state.text_justify_h = LEFT_TEXT;
	iterm_state.text_justify_v = TOP_TEXT;
	iterm_state.initialized = 1;

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!iterm_state.initialized)
		return;

	/* Display final image */
	display_framebuffer();

	if (iterm_state.framebuffer)
		free(iterm_state.framebuffer);

	memset(&iterm_state, 0, sizeof(iterm_state));
}

int getmaxx(void) { return iterm_state.width - 1; }
int getmaxy(void) { return iterm_state.height - 1; }
void setcolor(int color) { iterm_state.current_color = color & 0x0F; }
int getcolor(void) { return iterm_state.current_color; }
void setbkcolor(int color) { iterm_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return iterm_state.current_bkcolor; }

void cleardevice(void) {
	memset(iterm_state.framebuffer, iterm_state.current_bkcolor,
	       iterm_state.width * iterm_state.height);
	display_framebuffer();
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < iterm_state.width && y >= 0 && y < iterm_state.height)
		iterm_state.framebuffer[y * iterm_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= iterm_state.width || y < 0 || y >= iterm_state.height)
		return 0;
	return iterm_state.framebuffer[y * iterm_state.width + x];
}

void moveto(int x, int y) { iterm_state.current_x = x; iterm_state.current_y = y; }
void moverel(int dx, int dy) { iterm_state.current_x += dx; iterm_state.current_y += dy; }
int getx(void) { return iterm_state.current_x; }
int gety(void) { return iterm_state.current_y; }

static int abs(int x) { return x < 0 ? -x : x; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	while (1) {
		if (iterm_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, iterm_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(iterm_state.current_x, iterm_state.current_y, x, y);
	iterm_state.current_x = x; iterm_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = iterm_state.current_x + dx, y2 = iterm_state.current_y + dy;
	line(iterm_state.current_x, iterm_state.current_y, x2, y2);
	iterm_state.current_x = x2; iterm_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;

	while (dx <= dy) {
		putpixel(x + dx, y + dy, iterm_state.current_color);
		putpixel(x - dx, y + dy, iterm_state.current_color);
		putpixel(x + dx, y - dy, iterm_state.current_color);
		putpixel(x - dx, y - dy, iterm_state.current_color);
		putpixel(x + dy, y + dx, iterm_state.current_color);
		putpixel(x - dy, y + dx, iterm_state.current_color);
		putpixel(x + dy, y - dx, iterm_state.current_color);
		putpixel(x - dy, y - dx, iterm_state.current_color);

		if (d < 0)
			d += 2 * dx + 3;
		else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	iterm_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: iterm_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: iterm_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: iterm_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: iterm_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: iterm_state.line_pattern = pattern; break;
	default: iterm_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	iterm_state.fill_style = pattern;
	iterm_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, iterm_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	iterm_state.text_justify_h = horiz;
	iterm_state.text_justify_v = vert;
}

int textheight(const char *textstring) { return 12; }
int textwidth(const char *textstring) { return strlen(textstring) * 8; }

const char *grapherrormsg(int errorcode) {
	switch (errorcode) {
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	case grNotDetected: return "Graphics hardware not detected";
	case grFileNotFound: return "Driver file not found";
	case grInvalidDriver: return "Invalid graphics driver";
	case grNoLoadMem: return "Insufficient memory to load driver";
	default: return "Unknown error";
	}
}

int graphresult(void) {
	return iterm_state.initialized ? grOk : grNoInitGraph;
}
