/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using SDL 1.2 (Simple DirectMedia Layer 1.x)
 *
 * Provides BGI graphics on legacy platforms via SDL 1.2
 * Works on: Older Linux distributions, legacy systems
 */

#include "../graphics.h"
#include <SDL/SDL.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	SDL_Surface *screen;
	uint32_t *pixels;		/* Pixel buffer */
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
} sdl1_state;

/* BGI to SDL color mapping (EGA palette) */
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
get_sdl_color(int bgi_color)
{
	if (bgi_color >= 0 && bgi_color < 16)
		return ega_colors[bgi_color];
	return ega_colors[WHITE];
}

void
initgraph(int *driver, int *mode, const char *path)
{
	int width = 640;
	int height = 480;
	int bpp = 32;

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
	case SVGA_1024_768:
		width = 1024;
		height = 768;
		break;
	default:
		width = 640;
		height = 480;
	}

	/* Initialize SDL */
	if (SDL_Init(SDL_INIT_VIDEO) < 0) {
		*driver = grNotDetected;
		return;
	}

	/* Set video mode */
	sdl1_state.screen = SDL_SetVideoMode(width, height, bpp, SDL_SWSURFACE);

	if (!sdl1_state.screen) {
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	/* Set window caption */
	SDL_WM_SetCaption("BGI Graphics", NULL);

	/* Allocate pixel buffer */
	sdl1_state.pixels = (uint32_t *)malloc(width * height * sizeof(uint32_t));
	if (!sdl1_state.pixels) {
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	sdl1_state.width = width;
	sdl1_state.height = height;
	sdl1_state.current_color = WHITE;
	sdl1_state.current_bkcolor = BLACK;
	sdl1_state.current_x = 0;
	sdl1_state.current_y = 0;
	sdl1_state.line_style = SOLID_LINE;
	sdl1_state.line_pattern = 0xFFFF;
	sdl1_state.fill_style = SOLID_FILL;
	sdl1_state.fill_color = WHITE;
	sdl1_state.text_justify_h = LEFT_TEXT;
	sdl1_state.text_justify_v = TOP_TEXT;
	sdl1_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!sdl1_state.initialized)
		return;

	if (sdl1_state.pixels)
		free(sdl1_state.pixels);

	SDL_Quit();

	memset(&sdl1_state, 0, sizeof(sdl1_state));
}

int
getmaxx(void)
{
	return sdl1_state.width - 1;
}

int
getmaxy(void)
{
	return sdl1_state.height - 1;
}

void
setcolor(int color)
{
	sdl1_state.current_color = color;
}

int
getcolor(void)
{
	return sdl1_state.current_color;
}

void
setbkcolor(int color)
{
	sdl1_state.current_bkcolor = color;
}

int
getbkcolor(void)
{
	return sdl1_state.current_bkcolor;
}

static void
update_display(void)
{
	int x, y;
	uint32_t *screen_pixels;

	/* Lock surface if needed */
	if (SDL_MUSTLOCK(sdl1_state.screen)) {
		if (SDL_LockSurface(sdl1_state.screen) < 0)
			return;
	}

	/* Copy our pixel buffer to screen surface */
	screen_pixels = (uint32_t *)sdl1_state.screen->pixels;
	for (y = 0; y < sdl1_state.height; y++) {
		for (x = 0; x < sdl1_state.width; x++) {
			screen_pixels[y * (sdl1_state.screen->pitch / 4) + x] =
				sdl1_state.pixels[y * sdl1_state.width + x];
		}
	}

	/* Unlock surface if needed */
	if (SDL_MUSTLOCK(sdl1_state.screen)) {
		SDL_UnlockSurface(sdl1_state.screen);
	}

	/* Flip buffer */
	SDL_Flip(sdl1_state.screen);

	/* Process events to keep window responsive */
	SDL_Event event;
	while (SDL_PollEvent(&event)) {
		if (event.type == SDL_QUIT) {
			closegraph();
			exit(0);
		}
	}
}

void
cleardevice(void)
{
	uint32_t bk_color = get_sdl_color(sdl1_state.current_bkcolor);
	int i;

	for (i = 0; i < sdl1_state.width * sdl1_state.height; i++)
		sdl1_state.pixels[i] = bk_color;

	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < sdl1_state.width && y >= 0 && y < sdl1_state.height) {
		sdl1_state.pixels[y * sdl1_state.width + x] = get_sdl_color(color);
	}
}

unsigned int
getpixel(int x, int y)
{
	uint32_t pixel;
	int i;

	if (x < 0 || x >= sdl1_state.width || y < 0 || y >= sdl1_state.height)
		return 0;

	pixel = sdl1_state.pixels[y * sdl1_state.width + x];

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
	sdl1_state.current_x = x;
	sdl1_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	sdl1_state.current_x += dx;
	sdl1_state.current_y += dy;
}

int
getx(void)
{
	return sdl1_state.current_x;
}

int
gety(void)
{
	return sdl1_state.current_y;
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
		if (sdl1_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, sdl1_state.current_color);

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
	line(sdl1_state.current_x, sdl1_state.current_y, x, y);
	sdl1_state.current_x = x;
	sdl1_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = sdl1_state.current_x + dx;
	int y2 = sdl1_state.current_y + dy;
	line(sdl1_state.current_x, sdl1_state.current_y, x2, y2);
	sdl1_state.current_x = x2;
	sdl1_state.current_y = y2;
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
		putpixel(x + dx, y + dy, sdl1_state.current_color);
		putpixel(x - dx, y + dy, sdl1_state.current_color);
		putpixel(x + dx, y - dy, sdl1_state.current_color);
		putpixel(x - dx, y - dy, sdl1_state.current_color);
		putpixel(x + dy, y + dx, sdl1_state.current_color);
		putpixel(x - dy, y + dx, sdl1_state.current_color);
		putpixel(x + dy, y - dx, sdl1_state.current_color);
		putpixel(x - dy, y - dx, sdl1_state.current_color);

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
	sdl1_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		sdl1_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		sdl1_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		sdl1_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		sdl1_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		sdl1_state.line_pattern = pattern;
		break;
	default:
		sdl1_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	sdl1_state.fill_style = pattern;
	sdl1_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;
	uint32_t color = get_sdl_color(sdl1_state.fill_color);

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			if (x >= 0 && x < sdl1_state.width && y >= 0 && y < sdl1_state.height)
				sdl1_state.pixels[y * sdl1_state.width + x] = color;
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	sdl1_state.text_justify_h = horiz;
	sdl1_state.text_justify_v = vert;
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
	return sdl1_state.initialized ? grOk : grNoInitGraph;
}
