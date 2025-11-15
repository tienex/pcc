/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using SDL2 (Simple DirectMedia Layer 2.x)
 *
 * Provides BGI graphics on modern platforms via SDL2
 * Works on: Linux, *BSD, Windows, macOS, Android, iOS, etc.
 */

#include "../graphics.h"
#include <SDL2/SDL.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	SDL_Window *window;
	SDL_Renderer *renderer;
	SDL_Texture *texture;
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
} sdl2_state;

/* BGI to SDL2 color mapping (EGA palette) */
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

	/* Create window */
	sdl2_state.window = SDL_CreateWindow("BGI Graphics",
	                                      SDL_WINDOWPOS_CENTERED,
	                                      SDL_WINDOWPOS_CENTERED,
	                                      width, height,
	                                      SDL_WINDOW_SHOWN);

	if (!sdl2_state.window) {
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	/* Create renderer */
	sdl2_state.renderer = SDL_CreateRenderer(sdl2_state.window, -1,
	                                          SDL_RENDERER_ACCELERATED |
	                                          SDL_RENDERER_PRESENTVSYNC);

	if (!sdl2_state.renderer) {
		SDL_DestroyWindow(sdl2_state.window);
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	/* Create texture for pixel buffer */
	sdl2_state.texture = SDL_CreateTexture(sdl2_state.renderer,
	                                        SDL_PIXELFORMAT_ARGB8888,
	                                        SDL_TEXTUREACCESS_STREAMING,
	                                        width, height);

	if (!sdl2_state.texture) {
		SDL_DestroyRenderer(sdl2_state.renderer);
		SDL_DestroyWindow(sdl2_state.window);
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	/* Allocate pixel buffer */
	sdl2_state.pixels = (uint32_t *)malloc(width * height * sizeof(uint32_t));
	if (!sdl2_state.pixels) {
		SDL_DestroyTexture(sdl2_state.texture);
		SDL_DestroyRenderer(sdl2_state.renderer);
		SDL_DestroyWindow(sdl2_state.window);
		SDL_Quit();
		*driver = grNotDetected;
		return;
	}

	sdl2_state.width = width;
	sdl2_state.height = height;
	sdl2_state.current_color = WHITE;
	sdl2_state.current_bkcolor = BLACK;
	sdl2_state.current_x = 0;
	sdl2_state.current_y = 0;
	sdl2_state.line_style = SOLID_LINE;
	sdl2_state.line_pattern = 0xFFFF;
	sdl2_state.fill_style = SOLID_FILL;
	sdl2_state.fill_color = WHITE;
	sdl2_state.text_justify_h = LEFT_TEXT;
	sdl2_state.text_justify_v = TOP_TEXT;
	sdl2_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!sdl2_state.initialized)
		return;

	if (sdl2_state.pixels)
		free(sdl2_state.pixels);
	if (sdl2_state.texture)
		SDL_DestroyTexture(sdl2_state.texture);
	if (sdl2_state.renderer)
		SDL_DestroyRenderer(sdl2_state.renderer);
	if (sdl2_state.window)
		SDL_DestroyWindow(sdl2_state.window);

	SDL_Quit();

	memset(&sdl2_state, 0, sizeof(sdl2_state));
}

int
getmaxx(void)
{
	return sdl2_state.width - 1;
}

int
getmaxy(void)
{
	return sdl2_state.height - 1;
}

void
setcolor(int color)
{
	sdl2_state.current_color = color;
}

int
getcolor(void)
{
	return sdl2_state.current_color;
}

void
setbkcolor(int color)
{
	sdl2_state.current_bkcolor = color;
}

int
getbkcolor(void)
{
	return sdl2_state.current_bkcolor;
}

static void
update_display(void)
{
	/* Update texture from pixel buffer */
	SDL_UpdateTexture(sdl2_state.texture, NULL, sdl2_state.pixels,
	                  sdl2_state.width * sizeof(uint32_t));

	/* Render texture */
	SDL_RenderClear(sdl2_state.renderer);
	SDL_RenderCopy(sdl2_state.renderer, sdl2_state.texture, NULL, NULL);
	SDL_RenderPresent(sdl2_state.renderer);

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
	uint32_t bk_color = get_sdl_color(sdl2_state.current_bkcolor);
	int i;

	for (i = 0; i < sdl2_state.width * sdl2_state.height; i++)
		sdl2_state.pixels[i] = bk_color;

	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < sdl2_state.width && y >= 0 && y < sdl2_state.height) {
		sdl2_state.pixels[y * sdl2_state.width + x] = get_sdl_color(color);
	}
}

unsigned int
getpixel(int x, int y)
{
	uint32_t pixel;
	int i;

	if (x < 0 || x >= sdl2_state.width || y < 0 || y >= sdl2_state.height)
		return 0;

	pixel = sdl2_state.pixels[y * sdl2_state.width + x];

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
	sdl2_state.current_x = x;
	sdl2_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	sdl2_state.current_x += dx;
	sdl2_state.current_y += dy;
}

int
getx(void)
{
	return sdl2_state.current_x;
}

int
gety(void)
{
	return sdl2_state.current_y;
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
		if (sdl2_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, sdl2_state.current_color);

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
	line(sdl2_state.current_x, sdl2_state.current_y, x, y);
	sdl2_state.current_x = x;
	sdl2_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = sdl2_state.current_x + dx;
	int y2 = sdl2_state.current_y + dy;
	line(sdl2_state.current_x, sdl2_state.current_y, x2, y2);
	sdl2_state.current_x = x2;
	sdl2_state.current_y = y2;
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
		putpixel(x + dx, y + dy, sdl2_state.current_color);
		putpixel(x - dx, y + dy, sdl2_state.current_color);
		putpixel(x + dx, y - dy, sdl2_state.current_color);
		putpixel(x - dx, y - dy, sdl2_state.current_color);
		putpixel(x + dy, y + dx, sdl2_state.current_color);
		putpixel(x - dy, y + dx, sdl2_state.current_color);
		putpixel(x + dy, y - dx, sdl2_state.current_color);
		putpixel(x - dy, y - dx, sdl2_state.current_color);

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
	sdl2_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		sdl2_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		sdl2_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		sdl2_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		sdl2_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		sdl2_state.line_pattern = pattern;
		break;
	default:
		sdl2_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	sdl2_state.fill_style = pattern;
	sdl2_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	int x, y;
	uint32_t color = get_sdl_color(sdl2_state.fill_color);

	for (y = top; y <= bottom; y++) {
		for (x = left; x <= right; x++) {
			if (x >= 0 && x < sdl2_state.width && y >= 0 && y < sdl2_state.height)
				sdl2_state.pixels[y * sdl2_state.width + x] = color;
		}
	}

	update_display();
}

void
settextjustify(int horiz, int vert)
{
	sdl2_state.text_justify_h = horiz;
	sdl2_state.text_justify_v = vert;
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
	return sdl2_state.initialized ? grOk : grNoInitGraph;
}
