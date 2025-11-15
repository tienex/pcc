/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Linux Framebuffer (/dev/fb0)
 *
 * Provides BGI graphics on Linux console without X11
 * Works on: Linux kernel with framebuffer device support
 * Requires: /dev/fb0 device, root or video group access
 */

#include "../graphics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/ioctl.h>
#include <linux/fb.h>
#include <math.h>

/* Internal state */
static struct {
	int fb_fd;
	char *fb_mem;
	struct fb_var_screeninfo vinfo;
	struct fb_fix_screeninfo finfo;
	long screensize;
	uint32_t *pixels;		/* Our pixel buffer */
	int width;
	int height;
	int bpp;
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
} fbdev_state;

/* BGI to ARGB8888 color mapping (EGA palette) */
static const uint32_t ega_colors[16] = {
	0xFF000000,	/* BLACK */
	0xFF0000AA,	/* BLUE */
	0xFF00AA00,	/* GREEN */
	0xFF00AAAA,	/* CYAN */
	0xFFAA0000,	/* RED */
	0xFFAA00AA,	/* MAGENTA */
	0xFFAA5500,	/* BROWN */
	0xFFAAAAAA,	/* LIGHTGRAY */
	0xFF555555,	/* DARKGRAY */
	0xFF5555FF,	/* LIGHTBLUE */
	0xFF55FF55,	/* LIGHTGREEN */
	0xFF55FFFF,	/* LIGHTCYAN */
	0xFFFF5555,	/* LIGHTRED */
	0xFFFF55FF,	/* LIGHTMAGENTA */
	0xFFFFFF55,	/* YELLOW */
	0xFFFFFFFF	/* WHITE */
};

static uint32_t
get_fb_color(int bgi_color)
{
	if (bgi_color >= 0 && bgi_color < 16)
		return ega_colors[bgi_color];
	return ega_colors[WHITE];
}

void
initgraph(int *driver, int *mode, const char *path)
{
	const char *fbdev = "/dev/fb0";

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Open framebuffer device */
	fbdev_state.fb_fd = open(fbdev, O_RDWR);
	if (fbdev_state.fb_fd == -1) {
		*driver = grNotDetected;
		return;
	}

	/* Get fixed screen information */
	if (ioctl(fbdev_state.fb_fd, FBIOGET_FSCREENINFO, &fbdev_state.finfo)) {
		close(fbdev_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	/* Get variable screen information */
	if (ioctl(fbdev_state.fb_fd, FBIOGET_VSCREENINFO, &fbdev_state.vinfo)) {
		close(fbdev_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	/* Store screen parameters */
	fbdev_state.width = fbdev_state.vinfo.xres;
	fbdev_state.height = fbdev_state.vinfo.yres;
	fbdev_state.bpp = fbdev_state.vinfo.bits_per_pixel;

	/* Calculate screen size */
	fbdev_state.screensize = fbdev_state.finfo.smem_len;

	/* Map framebuffer to memory */
	fbdev_state.fb_mem = (char *)mmap(0, fbdev_state.screensize,
	                                   PROT_READ | PROT_WRITE,
	                                   MAP_SHARED, fbdev_state.fb_fd, 0);

	if (fbdev_state.fb_mem == MAP_FAILED) {
		close(fbdev_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	/* Allocate our pixel buffer */
	fbdev_state.pixels = (uint32_t *)malloc(fbdev_state.width * fbdev_state.height * sizeof(uint32_t));
	if (!fbdev_state.pixels) {
		munmap(fbdev_state.fb_mem, fbdev_state.screensize);
		close(fbdev_state.fb_fd);
		*driver = grNotDetected;
		return;
	}

	fbdev_state.current_color = WHITE;
	fbdev_state.current_bkcolor = BLACK;
	fbdev_state.current_x = 0;
	fbdev_state.current_y = 0;
	fbdev_state.line_style = SOLID_LINE;
	fbdev_state.line_pattern = 0xFFFF;
	fbdev_state.fill_style = SOLID_FILL;
	fbdev_state.fill_color = WHITE;
	fbdev_state.text_justify_h = LEFT_TEXT;
	fbdev_state.text_justify_v = TOP_TEXT;
	fbdev_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!fbdev_state.initialized)
		return;

	if (fbdev_state.pixels)
		free(fbdev_state.pixels);

	if (fbdev_state.fb_mem != MAP_FAILED)
		munmap(fbdev_state.fb_mem, fbdev_state.screensize);

	if (fbdev_state.fb_fd >= 0)
		close(fbdev_state.fb_fd);

	memset(&fbdev_state, 0, sizeof(fbdev_state));
	fbdev_state.fb_fd = -1;
}

int
getmaxx(void)
{
	return fbdev_state.width - 1;
}

int
getmaxy(void)
{
	return fbdev_state.height - 1;
}

void
setcolor(int color)
{
	fbdev_state.current_color = color;
}

int
getcolor(void)
{
	return fbdev_state.current_color;
}

void
setbkcolor(int color)
{
	fbdev_state.current_bkcolor = color;
}

int
getbkcolor(void)
{
	return fbdev_state.current_bkcolor;
}

static void
update_display(void)
{
	int x, y;
	uint32_t *fb_pixels;
	int bytes_per_pixel = fbdev_state.bpp / 8;
	long location;

	/* Copy our pixel buffer to framebuffer */
	if (fbdev_state.bpp == 32) {
		/* Direct 32-bit copy */
		fb_pixels = (uint32_t *)fbdev_state.fb_mem;
		for (y = 0; y < fbdev_state.height; y++) {
			for (x = 0; x < fbdev_state.width; x++) {
				location = y * fbdev_state.finfo.line_length / 4 + x;
				fb_pixels[location] = fbdev_state.pixels[y * fbdev_state.width + x];
			}
		}
	} else if (fbdev_state.bpp == 16) {
		/* Convert 32-bit ARGB to 16-bit RGB565 */
		uint16_t *fb_pixels16 = (uint16_t *)fbdev_state.fb_mem;
		for (y = 0; y < fbdev_state.height; y++) {
			for (x = 0; x < fbdev_state.width; x++) {
				uint32_t color = fbdev_state.pixels[y * fbdev_state.width + x];
				uint8_t r = (color >> 16) & 0xFF;
				uint8_t g = (color >> 8) & 0xFF;
				uint8_t b = color & 0xFF;
				uint16_t rgb565 = ((r >> 3) << 11) | ((g >> 2) << 5) | (b >> 3);
				location = y * fbdev_state.finfo.line_length / 2 + x;
				fb_pixels16[location] = rgb565;
			}
		}
	}
}

void
cleardevice(void)
{
	uint32_t bk_color = get_fb_color(fbdev_state.current_bkcolor);
	int i;

	for (i = 0; i < fbdev_state.width * fbdev_state.height; i++)
		fbdev_state.pixels[i] = bk_color;

	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < fbdev_state.width && y >= 0 && y < fbdev_state.height) {
		fbdev_state.pixels[y * fbdev_state.width + x] = get_fb_color(color);
	}
}

unsigned int
getpixel(int x, int y)
{
	uint32_t pixel;
	int i;

	if (x < 0 || x >= fbdev_state.width || y < 0 || y >= fbdev_state.height)
		return 0;

	pixel = fbdev_state.pixels[y * fbdev_state.width + x];

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (ega_colors[i] == pixel)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	fbdev_state.current_x = x;
	fbdev_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	fbdev_state.current_x += dx;
	fbdev_state.current_y += dy;
}

int
getx(void)
{
	return fbdev_state.current_x;
}

int
gety(void)
{
	return fbdev_state.current_y;
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
		if (fbdev_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, fbdev_state.current_color);

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
	line(fbdev_state.current_x, fbdev_state.current_y, x, y);
	fbdev_state.current_x = x;
	fbdev_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = fbdev_state.current_x + dx;
	int y2 = fbdev_state.current_y + dy;
	line(fbdev_state.current_x, fbdev_state.current_y, x2, y2);
	fbdev_state.current_x = x2;
	fbdev_state.current_y = y2;
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
		putpixel(x + dx, y + dy, fbdev_state.current_color);
		putpixel(x - dx, y + dy, fbdev_state.current_color);
		putpixel(x + dx, y - dy, fbdev_state.current_color);
		putpixel(x - dx, y - dy, fbdev_state.current_color);
		putpixel(x + dy, y + dx, fbdev_state.current_color);
		putpixel(x - dy, y + dx, fbdev_state.current_color);
		putpixel(x + dy, y - dx, fbdev_state.current_color);
		putpixel(x - dy, y - dx, fbdev_state.current_color);

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
	fbdev_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		fbdev_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		fbdev_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		fbdev_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		fbdev_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		fbdev_state.line_pattern = pattern;
		break;
	default:
		fbdev_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	fbdev_state.fill_style = pattern;
	fbdev_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;
	uint32_t color = get_fb_color(fbdev_state.fill_color);

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			if (x >= 0 && x < fbdev_state.width && y >= 0 && y < fbdev_state.height)
				fbdev_state.pixels[y * fbdev_state.width + x] = color;
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	fbdev_state.text_justify_h = horiz;
	fbdev_state.text_justify_v = vert;
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
	return fbdev_state.initialized ? grOk : grNoInitGraph;
}
