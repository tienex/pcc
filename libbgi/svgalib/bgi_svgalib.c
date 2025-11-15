/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using SVGAlib (Legacy Linux)
 *
 * Legacy Linux framebuffer graphics using SVGAlib
 * Works on: Linux 2.x with SVGAlib
 * Requires: libvga (SVGAlib)
 * Features: Direct video hardware access, multiple resolutions
 * Note: Requires root or setuid for direct hardware access
 */

#include "../graphics.h"
#include <vga.h>
#include <vgagl.h>
#include <stdlib.h>
#include <string.h>

static struct {
	GraphicsContext *gc;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	int initialized;
} svga_state;

/* EGA palette for SVGAlib */
static const int ega_svga_colors[16] = {
	0, 1, 2, 3, 4, 5, 20, 7, 56, 57, 58, 59, 60, 61, 62, 63
};

void initgraph(int *driver, int *mode, const char *path) {
	int vga_mode = G640x480x16;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	/* Map BGI mode to SVGAlib mode */
	switch (*mode) {
	case VGALO:
		vga_mode = G640x200x16;
		svga_state.width = 640;
		svga_state.height = 200;
		break;
	case VGAMED:
		vga_mode = G640x350x16;
		svga_state.width = 640;
		svga_state.height = 350;
		break;
	case VGAHI:
		vga_mode = G640x480x16;
		svga_state.width = 640;
		svga_state.height = 480;
		break;
	case SVGA_800_600:
		vga_mode = G800x600x16;
		svga_state.width = 800;
		svga_state.height = 600;
		break;
	case SVGA_1024_768:
		vga_mode = G1024x768x16;
		svga_state.width = 1024;
		svga_state.height = 768;
		break;
	default:
		vga_mode = G640x480x16;
		svga_state.width = 640;
		svga_state.height = 480;
	}

	/* Initialize SVGAlib */
	vga_init();

	if (!vga_hasmode(vga_mode)) {
		/* Try to find an alternative mode */
		if (vga_hasmode(G640x480x16))
			vga_mode = G640x480x16;
		else if (vga_hasmode(G640x480x256))
			vga_mode = G640x480x256;
		else {
			*driver = grNotDetected;
			return;
		}
	}

	if (vga_setmode(vga_mode) < 0) {
		*driver = grNotDetected;
		return;
	}

	/* Initialize graphics context */
	gl_setcontextvga(vga_mode);
	svga_state.gc = gl_allocatecontext();
	if (!svga_state.gc) {
		vga_setmode(TEXT);
		*driver = grNotDetected;
		return;
	}

	gl_getcontext(svga_state.gc);
	gl_setcontext(svga_state.gc);

	svga_state.current_color = WHITE;
	svga_state.current_bkcolor = BLACK;
	svga_state.current_x = 0;
	svga_state.current_y = 0;
	svga_state.line_style = SOLID_LINE;
	svga_state.line_pattern = 0xFFFF;
	svga_state.fill_style = SOLID_FILL;
	svga_state.fill_color = WHITE;
	svga_state.text_justify_h = LEFT_TEXT;
	svga_state.text_justify_v = TOP_TEXT;
	svga_state.initialized = 1;

	/* Set drawing color */
	gl_setwritemode(WRITEMODE_OVERWRITE);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!svga_state.initialized)
		return;

	if (svga_state.gc)
		gl_freecontext(svga_state.gc);

	vga_setmode(TEXT);
	memset(&svga_state, 0, sizeof(svga_state));
}

int getmaxx(void) { return svga_state.width - 1; }
int getmaxy(void) { return svga_state.height - 1; }

void setcolor(int color) {
	svga_state.current_color = color & 0x0F;
	gl_setcolor(ega_svga_colors[svga_state.current_color]);
}

int getcolor(void) { return svga_state.current_color; }

void setbkcolor(int color) {
	svga_state.current_bkcolor = color & 0x0F;
}

int getbkcolor(void) { return svga_state.current_bkcolor; }

void cleardevice(void) {
	gl_clearscreen(ega_svga_colors[svga_state.current_bkcolor]);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < svga_state.width && y >= 0 && y < svga_state.height)
		gl_setpixel(x, y, ega_svga_colors[color & 0x0F]);
}

unsigned int getpixel(int x, int y) {
	int svga_color, i;

	if (x < 0 || x >= svga_state.width || y < 0 || y >= svga_state.height)
		return 0;

	svga_color = gl_getpixel(x, y);

	/* Map SVGAlib color back to EGA color */
	for (i = 0; i < 16; i++)
		if (ega_svga_colors[i] == svga_color)
			return i;

	return 0;
}

void moveto(int x, int y) {
	svga_state.current_x = x;
	svga_state.current_y = y;
}

void moverel(int dx, int dy) {
	svga_state.current_x += dx;
	svga_state.current_y += dy;
}

int getx(void) { return svga_state.current_x; }
int gety(void) { return svga_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	gl_line(x1, y1, x2, y2, ega_svga_colors[svga_state.current_color]);
}

void lineto(int x, int y) {
	line(svga_state.current_x, svga_state.current_y, x, y);
	svga_state.current_x = x;
	svga_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = svga_state.current_x + dx;
	int y2 = svga_state.current_y + dy;
	line(svga_state.current_x, svga_state.current_y, x2, y2);
	svga_state.current_x = x2;
	svga_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	gl_rect(left, top, right, bottom, ega_svga_colors[svga_state.current_color]);
}

void circle(int x, int y, int radius) {
	gl_circle(x, y, radius, ega_svga_colors[svga_state.current_color]);
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	svga_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: svga_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: svga_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: svga_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: svga_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: svga_state.line_pattern = pattern; break;
	default: svga_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	svga_state.fill_style = pattern;
	svga_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	gl_fillbox(left, top, right - left + 1, bottom - top + 1,
	           ega_svga_colors[svga_state.fill_color & 0x0F]);
}

void settextjustify(int horiz, int vert) {
	svga_state.text_justify_h = horiz;
	svga_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	return 8;  /* SVGAlib default font height */
}

int textwidth(const char *textstring) {
	return strlen(textstring) * 8;  /* SVGAlib default font width */
}

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
	return svga_state.initialized ? grOk : grNoInitGraph;
}
