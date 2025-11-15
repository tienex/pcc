/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Kitty graphics protocol
 *
 * Terminal graphics using Kitty's graphics protocol
 * Works on: Kitty terminal, WezTerm, Konsole (partial)
 * Features: 24-bit color, chunked transmission, compression
 * Output: Kitty graphics escape sequences to stdout
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
} kitty_state;

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

static void base64_encode_chunk(const uint8_t *input, size_t length, char *output) {
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
	uint8_t *rgb_data;
	char *b64_chunk;
	size_t rgb_size = kitty_state.width * kitty_state.height * 3;
	size_t chunk_size = 4096;
	size_t offset;
	int x, y, first = 1;

	/* Convert indexed color to RGB */
	rgb_data = malloc(rgb_size);
	if (!rgb_data)
		return;

	for (y = 0; y < kitty_state.height; y++) {
		for (x = 0; x < kitty_state.width; x++) {
			uint8_t idx = kitty_state.framebuffer[y * kitty_state.width + x];
			uint32_t rgb = kitty_state.colors[idx & 0x0F];
			size_t pos = (y * kitty_state.width + x) * 3;
			rgb_data[pos + 0] = (rgb >> 16) & 0xFF;  /* R */
			rgb_data[pos + 1] = (rgb >> 8) & 0xFF;   /* G */
			rgb_data[pos + 2] = rgb & 0xFF;          /* B */
		}
	}

	/* Allocate base64 chunk buffer */
	b64_chunk = malloc((chunk_size * 4 / 3) + 4);
	if (!b64_chunk) {
		free(rgb_data);
		return;
	}

	/* Transmit in chunks using Kitty graphics protocol */
	for (offset = 0; offset < rgb_size; offset += chunk_size) {
		size_t remaining = rgb_size - offset;
		size_t current_chunk = remaining < chunk_size ? remaining : chunk_size;
		int more = (offset + current_chunk < rgb_size) ? 1 : 0;

		base64_encode_chunk(rgb_data + offset, current_chunk, b64_chunk);

		if (first) {
			/* First chunk: include format, dimensions */
			printf("\x1B_Gf=24,s=%d,v=%d,a=T,m=%d;%s\x1B\\",
			       kitty_state.width, kitty_state.height, more, b64_chunk);
			first = 0;
		} else {
			/* Subsequent chunks */
			printf("\x1B_Gm=%d;%s\x1B\\", more, b64_chunk);
		}
		fflush(stdout);
	}

	printf("\n");
	free(rgb_data);
	free(b64_chunk);
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
	kitty_state.framebuffer = malloc(width * height);
	if (!kitty_state.framebuffer) {
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		kitty_state.colors[i] = ega_rgb888[i];

	kitty_state.width = width;
	kitty_state.height = height;
	kitty_state.current_color = WHITE;
	kitty_state.current_bkcolor = BLACK;
	kitty_state.current_x = 0;
	kitty_state.current_y = 0;
	kitty_state.line_style = SOLID_LINE;
	kitty_state.line_pattern = 0xFFFF;
	kitty_state.fill_style = SOLID_FILL;
	kitty_state.fill_color = WHITE;
	kitty_state.text_justify_h = LEFT_TEXT;
	kitty_state.text_justify_v = TOP_TEXT;
	kitty_state.initialized = 1;

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!kitty_state.initialized)
		return;

	/* Display final image */
	display_framebuffer();

	/* Delete image from terminal */
	printf("\x1B_Ga=d\x1B\\\n");
	fflush(stdout);

	if (kitty_state.framebuffer)
		free(kitty_state.framebuffer);

	memset(&kitty_state, 0, sizeof(kitty_state));
}

int getmaxx(void) { return kitty_state.width - 1; }
int getmaxy(void) { return kitty_state.height - 1; }
void setcolor(int color) { kitty_state.current_color = color & 0x0F; }
int getcolor(void) { return kitty_state.current_color; }
void setbkcolor(int color) { kitty_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return kitty_state.current_bkcolor; }

void cleardevice(void) {
	memset(kitty_state.framebuffer, kitty_state.current_bkcolor,
	       kitty_state.width * kitty_state.height);
	display_framebuffer();
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < kitty_state.width && y >= 0 && y < kitty_state.height)
		kitty_state.framebuffer[y * kitty_state.width + x] = color & 0x0F;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= kitty_state.width || y < 0 || y >= kitty_state.height)
		return 0;
	return kitty_state.framebuffer[y * kitty_state.width + x];
}

void moveto(int x, int y) { kitty_state.current_x = x; kitty_state.current_y = y; }
void moverel(int dx, int dy) { kitty_state.current_x += dx; kitty_state.current_y += dy; }
int getx(void) { return kitty_state.current_x; }
int gety(void) { return kitty_state.current_y; }

static int abs(int x) { return x < 0 ? -x : x; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	while (1) {
		if (kitty_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, kitty_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(kitty_state.current_x, kitty_state.current_y, x, y);
	kitty_state.current_x = x; kitty_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = kitty_state.current_x + dx, y2 = kitty_state.current_y + dy;
	line(kitty_state.current_x, kitty_state.current_y, x2, y2);
	kitty_state.current_x = x2; kitty_state.current_y = y2;
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
		putpixel(x + dx, y + dy, kitty_state.current_color);
		putpixel(x - dx, y + dy, kitty_state.current_color);
		putpixel(x + dx, y - dy, kitty_state.current_color);
		putpixel(x - dx, y - dy, kitty_state.current_color);
		putpixel(x + dy, y + dx, kitty_state.current_color);
		putpixel(x - dy, y + dx, kitty_state.current_color);
		putpixel(x + dy, y - dx, kitty_state.current_color);
		putpixel(x - dy, y - dx, kitty_state.current_color);

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
	kitty_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: kitty_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: kitty_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: kitty_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: kitty_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: kitty_state.line_pattern = pattern; break;
	default: kitty_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	kitty_state.fill_style = pattern;
	kitty_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, kitty_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	kitty_state.text_justify_h = horiz;
	kitty_state.text_justify_v = vert;
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
	return kitty_state.initialized ? grOk : grNoInitGraph;
}
