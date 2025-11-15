/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for FreeBSD Framebuffer
 *
 * Provides BGI graphics on FreeBSD console framebuffer
 * Works on: FreeBSD with VESA or syscons/vt framebuffer
 * Requires: /dev/ttyv0, VESA framebuffer support
 */

#include "../graphics.h"
#include <sys/consio.h>
#include <sys/fbio.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include <stdlib.h>

static struct {
	int fb_fd;
	struct fbtype fb_type;
	unsigned char *framebuffer;
	int width, height, stride;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	uint32_t colors[16];
	int initialized;
} fbsd_state;

static const uint32_t ega_palette[16] = {
	0xFF000000, 0xFF0000AA, 0xFF00AA00, 0xFF00AAAA,
	0xFFAA0000, 0xFFAA00AA, 0xFFAA5500, 0xFFAAAAAA,
	0xFF555555, 0xFF5555FF, 0xFF55FF55, 0xFF55FFFF,
	0xFFFF5555, 0xFFFF55FF, 0xFFFFFF55, 0xFFFFFFFF
};

void
initgraph(int *driver, int *mode, const char *path)
{
	struct fbtype type;
	int width = 640, height = 480, i;
	size_t fb_size;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	fbsd_state.fb_fd = open("/dev/ttyv0", O_RDWR);
	if (fbsd_state.fb_fd < 0) {
		*driver = grNotDetected;
		return;
	}

	if (ioctl(fbsd_state.fb_fd, FBIOGTYPE, &type) < 0) {
		close(fbsd_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	fbsd_state.fb_type = type;
	fbsd_state.width = type.fb_width;
	fbsd_state.height = type.fb_height;
	fbsd_state.stride = type.fb_width * (type.fb_depth / 8);

	fb_size = fbsd_state.stride * fbsd_state.height;
	fbsd_state.framebuffer = (unsigned char *)mmap(NULL, fb_size,
	                                                PROT_READ | PROT_WRITE,
	                                                MAP_SHARED, fbsd_state.fb_fd, 0);

	if (fbsd_state.framebuffer == MAP_FAILED) {
		close(fbsd_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	for (i = 0; i < 16; i++)
		fbsd_state.colors[i] = ega_palette[i];

	fbsd_state.current_color = WHITE;
	fbsd_state.current_bkcolor = BLACK;
	fbsd_state.current_x = 0;
	fbsd_state.current_y = 0;
	fbsd_state.line_style = SOLID_LINE;
	fbsd_state.line_pattern = 0xFFFF;
	fbsd_state.fill_style = SOLID_FILL;
	fbsd_state.fill_color = WHITE;
	fbsd_state.text_justify_h = LEFT_TEXT;
	fbsd_state.text_justify_v = TOP_TEXT;
	fbsd_state.initialized = 1;

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!fbsd_state.initialized) return;
	if (fbsd_state.framebuffer != MAP_FAILED)
		munmap(fbsd_state.framebuffer, fbsd_state.stride * fbsd_state.height);
	if (fbsd_state.fb_fd >= 0)
		close(fbsd_state.fb_fd);
	memset(&fbsd_state, 0, sizeof(fbsd_state));
	fbsd_state.fb_fd = -1;
}

int getmaxx(void) { return fbsd_state.width - 1; }
int getmaxy(void) { return fbsd_state.height - 1; }
void setcolor(int color) { fbsd_state.current_color = color & 0x0F; }
int getcolor(void) { return fbsd_state.current_color; }
void setbkcolor(int color) { fbsd_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return fbsd_state.current_bkcolor; }

void cleardevice(void) {
	uint32_t *fb = (uint32_t *)fbsd_state.framebuffer;
	uint32_t bg = fbsd_state.colors[fbsd_state.current_bkcolor];
	int i;
	for (i = 0; i < fbsd_state.width * fbsd_state.height; i++)
		fb[i] = bg;
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < fbsd_state.width && y >= 0 && y < fbsd_state.height) {
		uint32_t *fb = (uint32_t *)fbsd_state.framebuffer;
		fb[y * fbsd_state.width + x] = fbsd_state.colors[color & 0x0F];
	}
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= fbsd_state.width || y < 0 || y >= fbsd_state.height)
		return 0;
	uint32_t *fb = (uint32_t *)fbsd_state.framebuffer;
	uint32_t pixel = fb[y * fbsd_state.width + x];
	int i;
	for (i = 0; i < 16; i++)
		if (fbsd_state.colors[i] == pixel)
			return i;
	return 0;
}

void moveto(int x, int y) { fbsd_state.current_x = x; fbsd_state.current_y = y; }
void moverel(int dx, int dy) { fbsd_state.current_x += dx; fbsd_state.current_y += dy; }
int getx(void) { return fbsd_state.current_x; }
int gety(void) { return fbsd_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	while (1) {
		if (fbsd_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, fbsd_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
}

void lineto(int x, int y) {
	line(fbsd_state.current_x, fbsd_state.current_y, x, y);
	fbsd_state.current_x = x; fbsd_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = fbsd_state.current_x + dx, y2 = fbsd_state.current_y + dy;
	line(fbsd_state.current_x, fbsd_state.current_y, x2, y2);
	fbsd_state.current_x = x2; fbsd_state.current_y = y2;
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
		putpixel(x + dx, y + dy, fbsd_state.current_color);
		putpixel(x - dx, y + dy, fbsd_state.current_color);
		putpixel(x + dx, y - dy, fbsd_state.current_color);
		putpixel(x - dx, y - dy, fbsd_state.current_color);
		putpixel(x + dy, y + dx, fbsd_state.current_color);
		putpixel(x - dy, y + dx, fbsd_state.current_color);
		putpixel(x + dy, y - dx, fbsd_state.current_color);
		putpixel(x - dy, y - dx, fbsd_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	fbsd_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: fbsd_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: fbsd_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: fbsd_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: fbsd_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: fbsd_state.line_pattern = pattern; break;
	default: fbsd_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	fbsd_state.fill_style = pattern;
	fbsd_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, fbsd_state.fill_color);
}

void settextjustify(int horiz, int vert) {
	fbsd_state.text_justify_h = horiz;
	fbsd_state.text_justify_v = vert;
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
	return fbsd_state.initialized ? grOk : grNoInitGraph;
}
