/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for 3Dfx Voodoo Graphics using Glide API
 *
 * Provides hardware-accelerated BGI graphics on 3Dfx Voodoo cards
 * Works on: DOS, Windows 95/98 with 3Dfx Voodoo 1/2/3/4/5/Banshee/Rush
 * Requires: Glide 2.x or Glide 3.x API
 * Features: Hardware 3D acceleration, texture mapping (for fills)
 */

#include "../graphics.h"
#include <glide.h>
#include <stdlib.h>
#include <string.h>

static struct {
	GrContext_t context;
	FxU32 *framebuffer;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	GrColor_t colors[16];
	int initialized;
} glide_state;

/* EGA palette in 565 RGB format (Voodoo's native format) */
static const GrColor_t ega_565[16] = {
	0x0000,	/* BLACK: 00000 000000 00000 */
	0x0015,	/* BLUE: 00000 000000 10101 */
	0x0540,	/* GREEN: 00000 101010 00000 */
	0x0555,	/* CYAN: 00000 101010 10101 */
	0xA800,	/* RED: 10101 000000 00000 */
	0xA815,	/* MAGENTA: 10101 000000 10101 */
	0xA540,	/* BROWN: 10101 101000 00000 */
	0xAD55,	/* LIGHTGRAY: 10101 101010 10101 */
	0x52AA,	/* DARKGRAY: 01010 010101 01010 */
	0x52BF,	/* LIGHTBLUE: 01010 010111 11111 */
	0x57EA,	/* LIGHTGREEN: 01010 111111 01010 */
	0x57FF,	/* LIGHTCYAN: 01010 111111 11111 */
	0xFAAA,	/* LIGHTRED: 11111 010101 01010 */
	0xFABF,	/* LIGHTMAGENTA: 11111 010101 11111 */
	0xFFEA,	/* YELLOW: 11111 111111 01010 */
	0xFFFF	/* WHITE: 11111 111111 11111 */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	GrScreenResolution_t resolution;
	GrScreenRefresh_t refresh = GR_REFRESH_60Hz;
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: resolution = GR_RESOLUTION_640x200; width = 640; height = 200; break;
	case VGAMED: resolution = GR_RESOLUTION_640x350; width = 640; height = 350; break;
	case VGAHI: resolution = GR_RESOLUTION_640x480; width = 640; height = 480; break;
	case SVGA_800_600: resolution = GR_RESOLUTION_800x600; width = 800; height = 600; break;
	default: resolution = GR_RESOLUTION_640x480; width = 640; height = 480;
	}

	/* Initialize Glide */
	grGlideInit();

	/* Select first Voodoo device */
	if (!grSstQueryHardware(&glide_state.context)) {
		*driver = grNotDetected;
		return;
	}

	grSstSelect(0);

	/* Open graphics mode */
	if (!grSstWinOpen(0, resolution, refresh,
	                   GR_COLORFORMAT_ARGB, GR_ORIGIN_UPPER_LEFT,
	                   2, 1)) {
		grGlideShutdown();
		*driver = grNotDetected;
		return;
	}

	/* Disable 3D features for 2D rendering */
	grDisable(GR_FOG);
	grDisable(GR_ALPHA_BLEND);
	grDisable(GR_DEPTH_BUFFER_MODE);
	grColorCombine(GR_COMBINE_FUNCTION_LOCAL, GR_COMBINE_FACTOR_NONE,
	                GR_COMBINE_LOCAL_CONSTANT, GR_COMBINE_OTHER_NONE, FXFALSE);

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		glide_state.colors[i] = ega_565[i];

	glide_state.width = width;
	glide_state.height = height;
	glide_state.current_color = WHITE;
	glide_state.current_bkcolor = BLACK;
	glide_state.current_x = 0;
	glide_state.current_y = 0;
	glide_state.line_style = SOLID_LINE;
	glide_state.line_pattern = 0xFFFF;
	glide_state.fill_style = SOLID_FILL;
	glide_state.fill_color = WHITE;
	glide_state.text_justify_h = LEFT_TEXT;
	glide_state.text_justify_v = TOP_TEXT;
	glide_state.initialized = 1;

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!glide_state.initialized) return;
	grSstWinClose();
	grGlideShutdown();
	memset(&glide_state, 0, sizeof(glide_state));
}

int getmaxx(void) { return glide_state.width - 1; }
int getmaxy(void) { return glide_state.height - 1; }
void setcolor(int color) {
	glide_state.current_color = color & 0x0F;
	grConstantColorValue(glide_state.colors[glide_state.current_color]);
}
int getcolor(void) { return glide_state.current_color; }
void setbkcolor(int color) { glide_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return glide_state.current_bkcolor; }

void cleardevice(void) {
	grBufferClear(glide_state.colors[glide_state.current_bkcolor], 0, 0);
	grBufferSwap(1);
}

void putpixel(int x, int y, int color) {
	GrVertex v;
	v.x = (float)x;
	v.y = (float)y;
	grConstantColorValue(glide_state.colors[color & 0x0F]);
	grDrawPoint(&v);
}

unsigned int getpixel(int x, int y) {
	/* Glide doesn't support reading back pixels easily */
	return 0;
}

void moveto(int x, int y) { glide_state.current_x = x; glide_state.current_y = y; }
void moverel(int dx, int dy) { glide_state.current_x += dx; glide_state.current_y += dy; }
int getx(void) { return glide_state.current_x; }
int gety(void) { return glide_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	GrVertex v[2];
	v[0].x = (float)x1;
	v[0].y = (float)y1;
	v[1].x = (float)x2;
	v[1].y = (float)y2;
	grDrawLine(&v[0], &v[1]);
	grBufferSwap(1);
}

void lineto(int x, int y) {
	line(glide_state.current_x, glide_state.current_y, x, y);
	glide_state.current_x = x;
	glide_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = glide_state.current_x + dx, y2 = glide_state.current_y + dy;
	line(glide_state.current_x, glide_state.current_y, x2, y2);
	glide_state.current_x = x2;
	glide_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	/* Circle using line segments */
	int segments = 64, i;
	float angle, x1, y1, x2, y2;
	
	for (i = 0; i < segments; i++) {
		angle = 2.0f * 3.14159265f * i / segments;
		x1 = x + radius * cosf(angle);
		y1 = y + radius * sinf(angle);
		angle = 2.0f * 3.14159265f * (i + 1) / segments;
		x2 = x + radius * cosf(angle);
		y2 = y + radius * sinf(angle);
		line((int)x1, (int)y1, (int)x2, (int)y2);
	}
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	glide_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: glide_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: glide_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: glide_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: glide_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: glide_state.line_pattern = pattern; break;
	default: glide_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	glide_state.fill_style = pattern;
	glide_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	GrVertex v[4];
	grConstantColorValue(glide_state.colors[glide_state.fill_color & 0x0F]);
	
	v[0].x = (float)left;  v[0].y = (float)top;
	v[1].x = (float)right; v[1].y = (float)top;
	v[2].x = (float)right; v[2].y = (float)bottom;
	v[3].x = (float)left;  v[3].y = (float)bottom;
	
	/* Draw as two triangles */
	grDrawTriangle(&v[0], &v[1], &v[2]);
	grDrawTriangle(&v[0], &v[2], &v[3]);
	
	grBufferSwap(1);
	grConstantColorValue(glide_state.colors[glide_state.current_color]);
}

void settextjustify(int horiz, int vert) {
	glide_state.text_justify_h = horiz;
	glide_state.text_justify_v = vert;
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
	return glide_state.initialized ? grOk : grNoInitGraph;
}
