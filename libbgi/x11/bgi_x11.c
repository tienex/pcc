/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using X11 (X Window System)
 *
 * Provides BGI graphics on Unix/Linux with X11
 * Works on: Linux, *BSD, Solaris, AIX, HP-UX with X Window System
 * Requires: libX11
 */

#include "../graphics.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	Display *display;
	Window window;
	GC gc;
	Pixmap pixmap;		/* Double buffering */
	int screen;
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
	XColor colors[16];	/* EGA palette */
	Colormap colormap;
	int initialized;
} x11_state;

/* EGA color values (RGB) */
static const unsigned short ega_rgb[16][3] = {
	{0x0000, 0x0000, 0x0000},	/* BLACK */
	{0x0000, 0x0000, 0xAAAA},	/* BLUE */
	{0x0000, 0xAAAA, 0x0000},	/* GREEN */
	{0x0000, 0xAAAA, 0xAAAA},	/* CYAN */
	{0xAAAA, 0x0000, 0x0000},	/* RED */
	{0xAAAA, 0x0000, 0xAAAA},	/* MAGENTA */
	{0xAAAA, 0x5555, 0x0000},	/* BROWN */
	{0xAAAA, 0xAAAA, 0xAAAA},	/* LIGHTGRAY */
	{0x5555, 0x5555, 0x5555},	/* DARKGRAY */
	{0x5555, 0x5555, 0xFFFF},	/* LIGHTBLUE */
	{0x5555, 0xFFFF, 0x5555},	/* LIGHTGREEN */
	{0x5555, 0xFFFF, 0xFFFF},	/* LIGHTCYAN */
	{0xFFFF, 0x5555, 0x5555},	/* LIGHTRED */
	{0xFFFF, 0x5555, 0xFFFF},	/* LIGHTMAGENTA */
	{0xFFFF, 0xFFFF, 0x5555},	/* YELLOW */
	{0xFFFF, 0xFFFF, 0xFFFF}	/* WHITE */
};

static void
setup_colors(void)
{
	int i;

	x11_state.colormap = DefaultColormap(x11_state.display, x11_state.screen);

	for (i = 0; i < 16; i++) {
		x11_state.colors[i].red = ega_rgb[i][0];
		x11_state.colors[i].green = ega_rgb[i][1];
		x11_state.colors[i].blue = ega_rgb[i][2];
		x11_state.colors[i].flags = DoRed | DoGreen | DoBlue;

		if (!XAllocColor(x11_state.display, x11_state.colormap, &x11_state.colors[i])) {
			/* Fall back to nearest color */
			x11_state.colors[i].pixel = i;
		}
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	int width = 640;
	int height = 480;
	XSetWindowAttributes attrs;

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

	/* Open X display */
	x11_state.display = XOpenDisplay(NULL);
	if (!x11_state.display) {
		*driver = grNotDetected;
		return;
	}

	x11_state.screen = DefaultScreen(x11_state.display);

	/* Setup color palette */
	setup_colors();

	/* Create window */
	attrs.background_pixel = x11_state.colors[BLACK].pixel;
	attrs.border_pixel = x11_state.colors[WHITE].pixel;
	attrs.backing_store = Always;

	x11_state.window = XCreateWindow(x11_state.display,
	                                  RootWindow(x11_state.display, x11_state.screen),
	                                  0, 0, width, height, 0,
	                                  DefaultDepth(x11_state.display, x11_state.screen),
	                                  InputOutput,
	                                  DefaultVisual(x11_state.display, x11_state.screen),
	                                  CWBackPixel | CWBorderPixel | CWBackingStore,
	                                  &attrs);

	if (!x11_state.window) {
		XCloseDisplay(x11_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Create pixmap for double buffering */
	x11_state.pixmap = XCreatePixmap(x11_state.display, x11_state.window,
	                                  width, height,
	                                  DefaultDepth(x11_state.display, x11_state.screen));

	/* Create graphics context */
	x11_state.gc = XCreateGC(x11_state.display, x11_state.window, 0, NULL);

	/* Set window properties */
	XStoreName(x11_state.display, x11_state.window, "BGI Graphics");
	XMapWindow(x11_state.display, x11_state.window);
	XFlush(x11_state.display);

	x11_state.width = width;
	x11_state.height = height;
	x11_state.current_color = WHITE;
	x11_state.current_bkcolor = BLACK;
	x11_state.current_x = 0;
	x11_state.current_y = 0;
	x11_state.line_style = SOLID_LINE;
	x11_state.line_pattern = 0xFFFF;
	x11_state.fill_style = SOLID_FILL;
	x11_state.fill_color = WHITE;
	x11_state.text_justify_h = LEFT_TEXT;
	x11_state.text_justify_v = TOP_TEXT;
	x11_state.initialized = 1;

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!x11_state.initialized)
		return;

	if (x11_state.pixmap)
		XFreePixmap(x11_state.display, x11_state.pixmap);

	if (x11_state.gc)
		XFreeGC(x11_state.display, x11_state.gc);

	if (x11_state.window)
		XDestroyWindow(x11_state.display, x11_state.window);

	if (x11_state.display)
		XCloseDisplay(x11_state.display);

	memset(&x11_state, 0, sizeof(x11_state));
}

int
getmaxx(void)
{
	return x11_state.width - 1;
}

int
getmaxy(void)
{
	return x11_state.height - 1;
}

void
setcolor(int color)
{
	x11_state.current_color = color & 0x0F;
	XSetForeground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.current_color].pixel);
}

int
getcolor(void)
{
	return x11_state.current_color;
}

void
setbkcolor(int color)
{
	x11_state.current_bkcolor = color & 0x0F;
	XSetBackground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.current_bkcolor].pixel);
}

int
getbkcolor(void)
{
	return x11_state.current_bkcolor;
}

static void
update_display(void)
{
	/* Copy pixmap to window */
	XCopyArea(x11_state.display, x11_state.pixmap, x11_state.window, x11_state.gc,
	          0, 0, x11_state.width, x11_state.height, 0, 0);
	XFlush(x11_state.display);

	/* Process any pending events */
	while (XPending(x11_state.display)) {
		XEvent event;
		XNextEvent(x11_state.display, &event);
		if (event.type == ClientMessage) {
			/* Window close request */
			closegraph();
			exit(0);
		}
	}
}

void
cleardevice(void)
{
	XSetForeground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.current_bkcolor].pixel);
	XFillRectangle(x11_state.display, x11_state.pixmap, x11_state.gc,
	               0, 0, x11_state.width, x11_state.height);
	XSetForeground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.current_color].pixel);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < x11_state.width && y >= 0 && y < x11_state.height) {
		XSetForeground(x11_state.display, x11_state.gc,
		               x11_state.colors[color & 0x0F].pixel);
		XDrawPoint(x11_state.display, x11_state.pixmap, x11_state.gc, x, y);
		XSetForeground(x11_state.display, x11_state.gc,
		               x11_state.colors[x11_state.current_color].pixel);
	}
}

unsigned int
getpixel(int x, int y)
{
	XImage *image;
	unsigned long pixel;
	int i;

	if (x < 0 || x >= x11_state.width || y < 0 || y >= x11_state.height)
		return 0;

	image = XGetImage(x11_state.display, x11_state.pixmap, x, y, 1, 1, AllPlanes, ZPixmap);
	if (!image)
		return 0;

	pixel = XGetPixel(image, 0, 0);
	XDestroyImage(image);

	/* Find matching color */
	for (i = 0; i < 16; i++) {
		if (x11_state.colors[i].pixel == pixel)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	x11_state.current_x = x;
	x11_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	x11_state.current_x += dx;
	x11_state.current_y += dy;
}

int
getx(void)
{
	return x11_state.current_x;
}

int
gety(void)
{
	return x11_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	if (x11_state.line_style == SOLID_LINE) {
		XDrawLine(x11_state.display, x11_state.pixmap, x11_state.gc, x1, y1, x2, y2);
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
			if (x11_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, x11_state.current_color);

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
	line(x11_state.current_x, x11_state.current_y, x, y);
	x11_state.current_x = x;
	x11_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = x11_state.current_x + dx;
	int y2 = x11_state.current_y + dy;
	line(x11_state.current_x, x11_state.current_y, x2, y2);
	x11_state.current_x = x2;
	x11_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	XDrawRectangle(x11_state.display, x11_state.pixmap, x11_state.gc,
	               left, top, right - left, bottom - top);
	update_display();
}

void
circle(int x, int y, int radius)
{
	XDrawArc(x11_state.display, x11_state.pixmap, x11_state.gc,
	         x - radius, y - radius, radius * 2, radius * 2, 0, 360 * 64);
	update_display();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	x11_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		x11_state.line_pattern = 0xFFFF;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineSolid, CapRound, JoinRound);
		break;
	case DOTTED_LINE:
		x11_state.line_pattern = 0xCCCC;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineOnOffDash, CapRound, JoinRound);
		break;
	case CENTER_LINE:
		x11_state.line_pattern = 0xF8F8;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineDoubleDash, CapRound, JoinRound);
		break;
	case DASHED_LINE:
		x11_state.line_pattern = 0xF0F0;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineOnOffDash, CapRound, JoinRound);
		break;
	case USERBIT_LINE:
		x11_state.line_pattern = pattern;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineOnOffDash, CapRound, JoinRound);
		break;
	default:
		x11_state.line_pattern = 0xFFFF;
		XSetLineAttributes(x11_state.display, x11_state.gc, thickness,
		                   LineSolid, CapRound, JoinRound);
	}
}

void
setfillstyle(int pattern, int color)
{
	x11_state.fill_style = pattern;
	x11_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	XSetForeground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.fill_color & 0x0F].pixel);
	XFillRectangle(x11_state.display, x11_state.pixmap, x11_state.gc,
	               left, top, right - left + 1, bottom - top + 1);
	XSetForeground(x11_state.display, x11_state.gc,
	               x11_state.colors[x11_state.current_color].pixel);
	update_display();
}

void
settextjustify(int horiz, int vert)
{
	x11_state.text_justify_h = horiz;
	x11_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 8;
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
	return x11_state.initialized ? grOk : grNoInitGraph;
}
