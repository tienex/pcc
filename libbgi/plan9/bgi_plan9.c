/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Plan 9 from Bell Labs
 *
 * Provides BGI graphics on Plan 9 using /dev/draw
 * Works on: Plan 9, 9front, plan9port
 * Requires: rio window system, /dev/draw device
 */

#include "../graphics.h"
#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <cursor.h>

/* Internal state */
static struct {
	Image *screen;
	Image *buffer;		/* Double buffering */
	Display *display;
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
	Image *colors[16];	/* EGA color images */
	int initialized;
} plan9_state;

/* EGA color palette (RGB) */
static const unsigned long ega_rgb[16] = {
	0x000000FF,	/* BLACK */
	0x0000AAFF,	/* BLUE */
	0x00AA00FF,	/* GREEN */
	0x00AAAAFF,	/* CYAN */
	0xAA0000FF,	/* RED */
	0xAA00AAFF,	/* MAGENTA */
	0xAA5500FF,	/* BROWN */
	0xAAAAAAFF,	/* LIGHTGRAY */
	0x555555FF,	/* DARKGRAY */
	0x5555FFFF,	/* LIGHTBLUE */
	0x55FF55FF,	/* LIGHTGREEN */
	0x55FFFFFF,	/* LIGHTCYAN */
	0xFF5555FF,	/* LIGHTRED */
	0xFF55FFFF,	/* LIGHTMAGENTA */
	0xFFFF55FF,	/* YELLOW */
	0xFFFFFFFF	/* WHITE */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	Rectangle r;
	int width = 640;
	int height = 480;
	int i;

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

	/* Initialize display */
	if (initdraw(nil, nil, "BGI Graphics") < 0) {
		*driver = grNotDetected;
		return;
	}

	plan9_state.display = display;
	plan9_state.screen = screen;

	/* Create off-screen buffer for double buffering */
	r = Rect(0, 0, width, height);
	plan9_state.buffer = allocimage(plan9_state.display, r, screen->chan, 0, DNofill);
	if (!plan9_state.buffer) {
		closedisplay(plan9_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Allocate color images */
	r = Rect(0, 0, 1, 1);
	for (i = 0; i < 16; i++) {
		plan9_state.colors[i] = allocimage(plan9_state.display, r, RGBA32, 1, ega_rgb[i]);
		if (!plan9_state.colors[i]) {
			/* Clean up on failure */
			while (--i >= 0)
				freeimage(plan9_state.colors[i]);
			freeimage(plan9_state.buffer);
			closedisplay(plan9_state.display);
			*driver = grNotDetected;
			return;
		}
	}

	plan9_state.width = width;
	plan9_state.height = height;
	plan9_state.current_color = WHITE;
	plan9_state.current_bkcolor = BLACK;
	plan9_state.current_x = 0;
	plan9_state.current_y = 0;
	plan9_state.line_style = SOLID_LINE;
	plan9_state.line_pattern = 0xFFFF;
	plan9_state.fill_style = SOLID_FILL;
	plan9_state.fill_color = WHITE;
	plan9_state.text_justify_h = LEFT_TEXT;
	plan9_state.text_justify_v = TOP_TEXT;
	plan9_state.initialized = 1;

	/* Initialize event handling */
	einit(Emouse | Ekeyboard);

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	int i;

	if (!plan9_state.initialized)
		return;

	/* Free color images */
	for (i = 0; i < 16; i++) {
		if (plan9_state.colors[i])
			freeimage(plan9_state.colors[i]);
	}

	/* Free buffer */
	if (plan9_state.buffer)
		freeimage(plan9_state.buffer);

	/* Close display */
	closedisplay(plan9_state.display);

	memset(&plan9_state, 0, sizeof(plan9_state));
}

int
getmaxx(void)
{
	return plan9_state.width - 1;
}

int
getmaxy(void)
{
	return plan9_state.height - 1;
}

void
setcolor(int color)
{
	plan9_state.current_color = color & 0x0F;
}

int
getcolor(void)
{
	return plan9_state.current_color;
}

void
setbkcolor(int color)
{
	plan9_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return plan9_state.current_bkcolor;
}

static void
update_display(void)
{
	Rectangle r;

	/* Copy buffer to screen */
	r = Rect(0, 0, plan9_state.width, plan9_state.height);
	draw(plan9_state.screen, r, plan9_state.buffer, nil, ZP);
	flushimage(plan9_state.display, 1);
}

void
cleardevice(void)
{
	Rectangle r;

	r = Rect(0, 0, plan9_state.width, plan9_state.height);
	draw(plan9_state.buffer, r, plan9_state.colors[plan9_state.current_bkcolor], nil, ZP);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	Rectangle r;

	if (x >= 0 && x < plan9_state.width && y >= 0 && y < plan9_state.height) {
		r = Rect(x, y, x + 1, y + 1);
		draw(plan9_state.buffer, r, plan9_state.colors[color & 0x0F], nil, ZP);
	}
}

unsigned int
getpixel(int x, int y)
{
	Rectangle r;
	uchar buf[4];
	unsigned long pixel;
	int i;

	if (x < 0 || x >= plan9_state.width || y < 0 || y >= plan9_state.height)
		return 0;

	r = Rect(x, y, x + 1, y + 1);
	if (unloadimage(plan9_state.buffer, r, buf, sizeof(buf)) < 0)
		return 0;

	/* Convert to RGBA */
	pixel = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (ega_rgb[i] == pixel)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	plan9_state.current_x = x;
	plan9_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	plan9_state.current_x += dx;
	plan9_state.current_y += dy;
}

int
getx(void)
{
	return plan9_state.current_x;
}

int
gety(void)
{
	return plan9_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	Point p1, p2;

	if (plan9_state.line_style == SOLID_LINE) {
		p1 = Pt(x1, y1);
		p2 = Pt(x2, y2);
		line(plan9_state.buffer, p1, p2, Endsquare, Endsquare, 0,
		     plan9_state.colors[plan9_state.current_color], ZP);
	} else {
		/* Bresenham's line algorithm for styled lines */
		int dx = abs(x2 - x1);
		int dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1;
		int sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy;
		int e2;
		int pattern_pos = 0;

		while (1) {
			if (plan9_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, plan9_state.current_color);

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
	}

	update_display();
}

void
lineto(int x, int y)
{
	line(plan9_state.current_x, plan9_state.current_y, x, y);
	plan9_state.current_x = x;
	plan9_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = plan9_state.current_x + dx;
	int y2 = plan9_state.current_y + dy;
	line(plan9_state.current_x, plan9_state.current_y, x2, y2);
	plan9_state.current_x = x2;
	plan9_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	Rectangle r;
	Point p[4];

	/* Draw as four lines */
	p[0] = Pt(left, top);
	p[1] = Pt(right, top);
	p[2] = Pt(right, bottom);
	p[3] = Pt(left, bottom);

	line(plan9_state.buffer, p[0], p[1], Endsquare, Endsquare, 0,
	     plan9_state.colors[plan9_state.current_color], ZP);
	line(plan9_state.buffer, p[1], p[2], Endsquare, Endsquare, 0,
	     plan9_state.colors[plan9_state.current_color], ZP);
	line(plan9_state.buffer, p[2], p[3], Endsquare, Endsquare, 0,
	     plan9_state.colors[plan9_state.current_color], ZP);
	line(plan9_state.buffer, p[3], p[0], Endsquare, Endsquare, 0,
	     plan9_state.colors[plan9_state.current_color], ZP);

	update_display();
}

void
circle(int x, int y, int radius)
{
	Point center;

	center = Pt(x, y);
	ellipse(plan9_state.buffer, center, radius, radius, 0,
	        plan9_state.colors[plan9_state.current_color], ZP);
	update_display();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	plan9_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		plan9_state.line_pattern = 0xFFFF;
		break;
	case DOTTED_LINE:
		plan9_state.line_pattern = 0xCCCC;
		break;
	case CENTER_LINE:
		plan9_state.line_pattern = 0xF8F8;
		break;
	case DASHED_LINE:
		plan9_state.line_pattern = 0xF0F0;
		break;
	case USERBIT_LINE:
		plan9_state.line_pattern = pattern;
		break;
	default:
		plan9_state.line_pattern = 0xFFFF;
	}
}

void
setfillstyle(int pattern, int color)
{
	plan9_state.fill_style = pattern;
	plan9_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	Rectangle r;

	r = Rect(left, top, right + 1, bottom + 1);
	draw(plan9_state.buffer, r, plan9_state.colors[plan9_state.fill_color & 0x0F], nil, ZP);
	update_display();
}

void
settextjustify(int horiz, int vert)
{
	plan9_state.text_justify_h = horiz;
	plan9_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return plan9_state.display->defaultfont->height;
}

int
textwidth(const char *textstring)
{
	return stringwidth(plan9_state.display->defaultfont, textstring);
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
	return plan9_state.initialized ? grOk : grNoInitGraph;
}
