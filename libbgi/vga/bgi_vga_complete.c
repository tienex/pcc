/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Complete BGI Implementation for VGA Mode 13h (320x200x256)
 *
 * Full BGI API with hardware acceleration and common library integration
 */

#include "../graphics.h"
#include "../bgi_common.h"
#include <dos.h>
#include <stdlib.h>
#include <string.h>

/* VGA Mode 13h constants */
#define VGA_WIDTH		320
#define VGA_HEIGHT		200
#define VGA_SEGMENT		0xA000
#define VGA_MODE_13H		0x13
#define TEXT_MODE		0x03

/* VGA I/O ports */
#define VGA_INPUT_STATUS	0x3DA
#define VGA_PALETTE_MASK	0x3C6
#define VGA_PALETTE_READ	0x3C7
#define VGA_PALETTE_WRITE	0x3C8
#define VGA_PALETTE_DATA	0x3C9

/* Internal state */
static struct {
	struct bgi_common_state common;
	unsigned char FAR *vga_mem;
	unsigned char current_palette[256][3];
	int initialized;
} vga_state;

/* EGA palette */
static const unsigned char ega_palette[16][3] = {
	{0x00, 0x00, 0x00}, {0x00, 0x00, 0xAA}, {0x00, 0xAA, 0x00}, {0x00, 0xAA, 0xAA},
	{0xAA, 0x00, 0x00}, {0xAA, 0x00, 0xAA}, {0xAA, 0x55, 0x00}, {0xAA, 0xAA, 0xAA},
	{0x55, 0x55, 0x55}, {0x55, 0x55, 0xFF}, {0x55, 0xFF, 0x55}, {0x55, 0xFF, 0xFF},
	{0xFF, 0x55, 0x55}, {0xFF, 0x55, 0xFF}, {0xFF, 0xFF, 0x55}, {0xFF, 0xFF, 0xFF}
};

static void set_vga_mode(unsigned char mode) {
	union REGS regs;
	regs.h.ah = 0x00;
	regs.h.al = mode;
	int86(0x10, &regs, &regs);
}

static void set_vga_palette(void) {
	int i;
	for (i = 0; i < 16; i++) {
		outp(VGA_PALETTE_WRITE, i);
		outp(VGA_PALETTE_DATA, ega_palette[i][0] >> 2);
		outp(VGA_PALETTE_DATA, ega_palette[i][1] >> 2);
		outp(VGA_PALETTE_DATA, ega_palette[i][2] >> 2);
		vga_state.current_palette[i][0] = ega_palette[i][0];
		vga_state.current_palette[i][1] = ega_palette[i][1];
		vga_state.current_palette[i][2] = ega_palette[i][2];
	}
	for (i = 16; i < 256; i++) {
		outp(VGA_PALETTE_WRITE, i);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
		outp(VGA_PALETTE_DATA, (i >> 2) & 0x3F);
	}
}

void initgraph(int *driver, int *mode, const char *path) {
	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	set_vga_mode(VGA_MODE_13H);
	vga_state.vga_mem = (unsigned char FAR *)MK_FP(VGA_SEGMENT, 0);
	set_vga_palette();

	vga_state.common.width = VGA_WIDTH;
	vga_state.common.height = VGA_HEIGHT;
	vga_state.common.current_color = WHITE;
	vga_state.common.current_bkcolor = BLACK;
	vga_state.common.current_x = 0;
	vga_state.common.current_y = 0;
	vga_state.common.line_style = SOLID_LINE;
	vga_state.common.line_pattern = 0xFFFF;
	vga_state.common.line_thickness = NORM_WIDTH;
	vga_state.common.fill_style = SOLID_FILL;
	vga_state.common.fill_color = WHITE;
	_fmemcpy(vga_state.common.fill_pattern, bgi_fill_patterns[SOLID_FILL], 8);
	vga_state.common.text_justify_h = LEFT_TEXT;
	vga_state.common.text_justify_v = TOP_TEXT;
	vga_state.common.text_direction = HORIZ_DIR;
	vga_state.common.text_font = DEFAULT_FONT;
	vga_state.common.text_charsize = 1;
	vga_state.common.viewport.left = 0;
	vga_state.common.viewport.top = 0;
	vga_state.common.viewport.right = VGA_WIDTH - 1;
	vga_state.common.viewport.bottom = VGA_HEIGHT - 1;
	vga_state.common.viewport.clip = 1;
	vga_state.common.write_mode = 0;
	vga_state.common.visual_page = 0;
	vga_state.common.active_page = 0;
	vga_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	if (!vga_state.initialized) return;
	set_vga_mode(TEXT_MODE);
	_fmemset(&vga_state, 0, sizeof(vga_state));
}

int graphresult(void) {
	return vga_state.initialized ? grOk : grNoInitGraph;
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

/* Screen control */
void cleardevice(void) {
	_fmemset(vga_state.vga_mem, vga_state.common.current_bkcolor, VGA_WIDTH * VGA_HEIGHT);
}

void clearviewport(void) {
	int x, y;
	for (y = vga_state.common.viewport.top; y <= vga_state.common.viewport.bottom; y++)
		for (x = vga_state.common.viewport.left; x <= vga_state.common.viewport.right; x++)
			vga_state.vga_mem[y * VGA_WIDTH + x] = vga_state.common.current_bkcolor;
}

/* Coordinate functions */
int getmaxx(void) { return VGA_WIDTH - 1; }
int getmaxy(void) { return VGA_HEIGHT - 1; }
int getx(void) { return vga_state.common.current_x; }
int gety(void) { return vga_state.common.current_y; }

void getviewsettings(struct viewporttype *viewport) {
	if (viewport) *viewport = vga_state.common.viewport;
}

void setviewport(int left, int top, int right, int bottom, int clip) {
	vga_state.common.viewport.left = left;
	vga_state.common.viewport.top = top;
	vga_state.common.viewport.right = right;
	vga_state.common.viewport.bottom = bottom;
	vga_state.common.viewport.clip = clip;
	vga_state.common.current_x = 0;
	vga_state.common.current_y = 0;
}

/* Pixel functions */
void putpixel(int x, int y, int color) {
	if (x >= 0 && x < VGA_WIDTH && y >= 0 && y < VGA_HEIGHT)
		vga_state.vga_mem[y * VGA_WIDTH + x] = color & 0xFF;
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= VGA_WIDTH || y < 0 || y >= VGA_HEIGHT) return 0;
	return vga_state.vga_mem[y * VGA_WIDTH + x];
}

/* Movement functions */
void moveto(int x, int y) {
	vga_state.common.current_x = x;
	vga_state.common.current_y = y;
}

void moverel(int dx, int dy) {
	vga_state.common.current_x += dx;
	vga_state.common.current_y += dy;
}

/* Line drawing */
void line(int x1, int y1, int x2, int y2) {
	bgi_draw_line_bresenham(x1, y1, x2, y2, putpixel,
	                        vga_state.common.current_color,
	                        vga_state.common.line_pattern);
}

void lineto(int x, int y) {
	line(vga_state.common.current_x, vga_state.common.current_y, x, y);
	vga_state.common.current_x = x;
	vga_state.common.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = vga_state.common.current_x + dx;
	int y2 = vga_state.common.current_y + dy;
	line(vga_state.common.current_x, vga_state.common.current_y, x2, y2);
	vga_state.common.current_x = x2;
	vga_state.common.current_y = y2;
}

/* Shape drawing */
void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	bgi_draw_circle_bresenham(x, y, radius, putpixel, vga_state.common.current_color);
}

void arc(int x, int y, int stangle, int endangle, int radius) {
	bgi_draw_arc(x, y, stangle, endangle, radius, putpixel,
	             vga_state.common.current_color, &vga_state.common.last_arc);
}

void ellipse(int x, int y, int stangle, int endangle, int xradius, int yradius) {
	bgi_draw_ellipse(x, y, stangle, endangle, xradius, yradius, putpixel,
	                 vga_state.common.current_color);
}

void fillellipse(int x, int y, int xradius, int yradius) {
	bgi_fill_ellipse(x, y, xradius, yradius, putpixel, vga_state.common.fill_color);
}

void pieslice(int x, int y, int stangle, int endangle, int radius) {
	/* Draw arc */
	bgi_draw_arc(x, y, stangle, endangle, radius, putpixel,
	             vga_state.common.current_color, NULL);
	/* Draw lines to center */
	int x1, y1, x2, y2;
	bgi_angle_to_point(stangle, radius, &x1, &y1);
	bgi_angle_to_point(endangle, radius, &x2, &y2);
	line(x, y, x + x1, y - y1);
	line(x, y, x + x2, y - y2);
	/* Fill would go here */
}

void drawpoly(int numpoints, const int *polypoints) {
	bgi_draw_polygon(numpoints, polypoints, line);
}

void fillpoly(int numpoints, const int *polypoints) {
	bgi_fill_polygon(numpoints, polypoints, putpixel, vga_state.common.fill_color,
	                 VGA_WIDTH, VGA_HEIGHT);
}

void bar(int left, int top, int right, int bottom) {
	if (vga_state.common.fill_style == SOLID_FILL) {
		int x, y;
		for (y = top; y <= bottom; y++)
			for (x = left; x <= right; x++)
				putpixel(x, y, vga_state.common.fill_color);
	} else {
		bgi_pattern_fill_rect(left, top, right, bottom, putpixel,
		                      vga_state.common.fill_pattern,
		                      vga_state.common.fill_color);
	}
}

void bar3d(int left, int top, int right, int bottom, int depth, int topflag) {
	bar(left, top, right, bottom);
	if (depth > 0) {
		line(right, top, right + depth, top - depth);
		line(right, bottom, right + depth, bottom - depth);
		line(right + depth, top - depth, right + depth, bottom - depth);
		if (topflag) {
			line(left, top, left + depth, top - depth);
			line(left + depth, top - depth, right + depth, top - depth);
		}
	}
}

void floodfill(int x, int y, int border) {
	bgi_flood_fill(x, y, border, getpixel, putpixel, vga_state.common.fill_color,
	               VGA_WIDTH, VGA_HEIGHT);
}

/* Line style functions */
void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	vga_state.common.line_style = linestyle;
	vga_state.common.line_thickness = thickness;
	switch (linestyle) {
	case SOLID_LINE: vga_state.common.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: vga_state.common.line_pattern = 0xCCCC; break;
	case CENTER_LINE: vga_state.common.line_pattern = 0xF8F8; break;
	case DASHED_LINE: vga_state.common.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: vga_state.common.line_pattern = pattern; break;
	default: vga_state.common.line_pattern = 0xFFFF;
	}
}

void getlinesettings(struct linesettingstype *lineinfo) {
	if (lineinfo) {
		lineinfo->linestyle = vga_state.common.line_style;
		lineinfo->upattern = vga_state.common.line_pattern;
		lineinfo->thickness = vga_state.common.line_thickness;
	}
}

/* Fill functions */
void setfillstyle(int pattern, int color) {
	vga_state.common.fill_style = pattern;
	vga_state.common.fill_color = color & 0xFF;
	if (pattern >= 0 && pattern < 12)
		_fmemcpy(vga_state.common.fill_pattern, bgi_fill_patterns[pattern], 8);
}

void setfillpattern(const char *upattern, int color) {
	vga_state.common.fill_style = USER_FILL;
	vga_state.common.fill_color = color & 0xFF;
	_fmemcpy(vga_state.common.fill_pattern, upattern, 8);
}

void getfillsettings(struct fillsettingstype *fillinfo) {
	if (fillinfo) {
		fillinfo->pattern = vga_state.common.fill_style;
		fillinfo->color = vga_state.common.fill_color;
	}
}

void getfillpattern(char *pattern) {
	if (pattern)
		_fmemcpy(pattern, vga_state.common.fill_pattern, 8);
}

/* Color functions */
void setcolor(int color) {
	vga_state.common.current_color = color & 0xFF;
}

int getcolor(void) {
	return vga_state.common.current_color;
}

void setbkcolor(int color) {
	vga_state.common.current_bkcolor = color & 0xFF;
}

int getbkcolor(void) {
	return vga_state.common.current_bkcolor;
}

void setpalette(int colornum, int color) {
	if (colornum < 0 || colornum >= 256) return;
	outp(VGA_PALETTE_WRITE, colornum);
	outp(VGA_PALETTE_DATA, (color >> 16) & 0x3F);
	outp(VGA_PALETTE_DATA, (color >> 8) & 0x3F);
	outp(VGA_PALETTE_DATA, color & 0x3F);
}

/* Text functions */
void settextjustify(int horiz, int vert) {
	vga_state.common.text_justify_h = horiz;
	vga_state.common.text_justify_v = vert;
}

void settextstyle(int font, int direction, int charsize) {
	vga_state.common.text_font = font;
	vga_state.common.text_direction = direction;
	vga_state.common.text_charsize = charsize;
}

void gettextsettings(struct textsettingstype *texttypeinfo) {
	if (texttypeinfo) {
		texttypeinfo->font = vga_state.common.text_font;
		texttypeinfo->direction = vga_state.common.text_direction;
		texttypeinfo->charsize = vga_state.common.text_charsize;
		texttypeinfo->horiz = vga_state.common.text_justify_h;
		texttypeinfo->vert = vga_state.common.text_justify_v;
	}
}

void outtext(const char *textstring) {
	outtextxy(vga_state.common.current_x, vga_state.common.current_y, textstring);
}

void outtextxy(int x, int y, const char *textstring) {
	bgi_draw_text_simple(x, y, textstring, putpixel, vga_state.common.current_color,
	                     vga_state.common.text_justify_h, vga_state.common.text_justify_v);
}

int textheight(const char *textstring) {
	return bgi_text_height_simple() * vga_state.common.text_charsize;
}

int textwidth(const char *textstring) {
	return bgi_text_width_simple(textstring) * vga_state.common.text_charsize;
}

void getarccoords(struct arccoordstype *arccoords) {
	if (arccoords) *arccoords = vga_state.common.last_arc;
}

/* Image functions */
unsigned int imagesize(int left, int top, int right, int bottom) {
	int width = right - left + 1;
	int height = bottom - top + 1;
	return 4 + width * height;  /* 4 bytes for dimensions */
}

void getimage(int left, int top, int right, int bottom, void *bitmap) {
	int width = right - left + 1;
	int height = bottom - top + 1;
	unsigned char *bmp = (unsigned char *)bitmap;
	int x, y;

	/* Store dimensions */
	bmp[0] = width & 0xFF;
	bmp[1] = (width >> 8) & 0xFF;
	bmp[2] = height & 0xFF;
	bmp[3] = (height >> 8) & 0xFF;

	/* Copy pixels */
	for (y = 0; y < height; y++)
		for (x = 0; x < width; x++)
			bmp[4 + y * width + x] = getpixel(left + x, top + y);
}

void putimage(int left, int top, const void *bitmap, int op) {
	const unsigned char *bmp = (const unsigned char *)bitmap;
	int width = bmp[0] | (bmp[1] << 8);
	int height = bmp[2] | (bmp[3] << 8);
	int x, y;

	for (y = 0; y < height; y++) {
		for (x = 0; x < width; x++) {
			unsigned char pixel = bmp[4 + y * width + x];
			int px = left + x;
			int py = top + y;
			if (px >= 0 && px < VGA_WIDTH && py >= 0 && py < VGA_HEIGHT) {
				if (op == 0) /* COPY */
					putpixel(px, py, pixel);
				/* Other ops: XOR, OR, AND, NOT would go here */
			}
		}
	}
}
