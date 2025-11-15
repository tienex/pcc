/*
 * Copyright (c) 2025 PCC Project
 * Borland Graphics Interface (BGI) - Unix/X11 Implementation
 */

#if !defined(_WIN32) && !defined(__DOS__) && !defined(__MSDOS__) && !defined(_MSDOS)

#include "../bgi.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

static struct {
	int initialized;
	int width;
	int height;
	int color;
	int fillcolor;
	int linestyle;
	int fillstyle;
	int textjustify_h;
	int textjustify_v;
	Display *display;
	Window window;
	GC gc;
	Pixmap pixmap;  /* Double buffering */
	int x, y;
} bgi_state = {0};

static const unsigned long bgi_colors_x11[] = {
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

void initgraph(int *driver, int *mode, const char *path) {
	(void)driver; (void)mode; (void)path;

	bgi_state.width = 640;
	bgi_state.height = 480;

	/* Open connection to X server */
	bgi_state.display = XOpenDisplay(NULL);
	if (!bgi_state.display) {
		fprintf(stderr, "Cannot open X display\n");
		return;
	}

	int screen = DefaultScreen(bgi_state.display);

	/* Create window */
	bgi_state.window = XCreateSimpleWindow(
		bgi_state.display,
		RootWindow(bgi_state.display, screen),
		0, 0, bgi_state.width, bgi_state.height, 1,
		BlackPixel(bgi_state.display, screen),
		BlackPixel(bgi_state.display, screen)
	);

	/* Set window properties */
	XStoreName(bgi_state.display, bgi_state.window, "BGI Graphics");
	XSelectInput(bgi_state.display, bgi_state.window, ExposureMask | KeyPressMask);
	XMapWindow(bgi_state.display, bgi_state.window);

	/* Create graphics context */
	bgi_state.gc = XCreateGC(bgi_state.display, bgi_state.window, 0, NULL);

	/* Create pixmap for double buffering */
	bgi_state.pixmap = XCreatePixmap(bgi_state.display, bgi_state.window,
	                                  bgi_state.width, bgi_state.height,
	                                  DefaultDepth(bgi_state.display, screen));

	/* Clear pixmap to black */
	XSetForeground(bgi_state.display, bgi_state.gc, BlackPixel(bgi_state.display, screen));
	XFillRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	               0, 0, bgi_state.width, bgi_state.height);

	bgi_state.color = WHITE;
	bgi_state.fillcolor = WHITE;
	bgi_state.x = bgi_state.y = 0;
	bgi_state.initialized = 1;

	/* Wait for window to be mapped */
	XFlush(bgi_state.display);
}

void closegraph(void) {
	if (bgi_state.initialized) {
		XFreePixmap(bgi_state.display, bgi_state.pixmap);
		XFreeGC(bgi_state.display, bgi_state.gc);
		XDestroyWindow(bgi_state.display, bgi_state.window);
		XCloseDisplay(bgi_state.display);
		bgi_state.initialized = 0;
	}
}

void cleardevice(void) {
	if (!bgi_state.initialized) return;

	int screen = DefaultScreen(bgi_state.display);
	XSetForeground(bgi_state.display, bgi_state.gc, BlackPixel(bgi_state.display, screen));
	XFillRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	               0, 0, bgi_state.width, bgi_state.height);
	XCopyArea(bgi_state.display, bgi_state.pixmap, bgi_state.window, bgi_state.gc,
	          0, 0, bgi_state.width, bgi_state.height, 0, 0);
	XFlush(bgi_state.display);
}

void setcolor(int color) {
	bgi_state.color = color;
	if (bgi_state.initialized) {
		XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[color & 15]);
	}
}

void setfillstyle(int pattern, int color) {
	bgi_state.fillstyle = pattern;
	bgi_state.fillcolor = color;
}

void putpixel(int x, int y, int color) {
	if (!bgi_state.initialized) return;
	XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[color & 15]);
	XDrawPoint(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x, y);
	XDrawPoint(bgi_state.display, bgi_state.window, bgi_state.gc, x, y);
	XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[bgi_state.color & 15]);
}

int getpixel(int x, int y) {
	if (!bgi_state.initialized) return 0;

	XImage *image = XGetImage(bgi_state.display, bgi_state.pixmap, x, y, 1, 1, AllPlanes, ZPixmap);
	if (image) {
		unsigned long pixel = XGetPixel(image, 0, 0);
		XDestroyImage(image);

		for (int i = 0; i < 16; i++) {
			if (bgi_colors_x11[i] == pixel) return i;
		}
	}
	return 0;
}

void line(int x1, int y1, int x2, int y2) {
	if (!bgi_state.initialized) return;

	XDrawLine(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x1, y1, x2, y2);
	XDrawLine(bgi_state.display, bgi_state.window, bgi_state.gc, x1, y1, x2, y2);

	bgi_state.x = x2;
	bgi_state.y = y2;
	XFlush(bgi_state.display);
}

void lineto(int x, int y) {
	line(bgi_state.x, bgi_state.y, x, y);
}

void moveto(int x, int y) {
	bgi_state.x = x;
	bgi_state.y = y;
}

void circle(int x, int y, int radius) {
	if (!bgi_state.initialized) return;

	int diameter = radius * 2;
	XDrawArc(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	         x - radius, y - radius, diameter, diameter, 0, 360 * 64);
	XDrawArc(bgi_state.display, bgi_state.window, bgi_state.gc,
	         x - radius, y - radius, diameter, diameter, 0, 360 * 64);
	XFlush(bgi_state.display);
}

void rectangle(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized) return;

	int width = right - left;
	int height = bottom - top;

	XDrawRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc, left, top, width, height);
	XDrawRectangle(bgi_state.display, bgi_state.window, bgi_state.gc, left, top, width, height);
	XFlush(bgi_state.display);
}

void bar(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized) return;

	int width = right - left;
	int height = bottom - top;

	unsigned long old_fg;
	XGCValues values;
	XGetGCValues(bgi_state.display, bgi_state.gc, GCForeground, &values);
	old_fg = values.foreground;

	XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[bgi_state.fillcolor & 15]);
	XFillRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc, left, top, width, height);
	XFillRectangle(bgi_state.display, bgi_state.window, bgi_state.gc, left, top, width, height);

	XSetForeground(bgi_state.display, bgi_state.gc, old_fg);
	XFlush(bgi_state.display);
}

void outtextxy(int x, int y, const char *text) {
	if (!bgi_state.initialized) return;

	XDrawString(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x, y, text, strlen(text));
	XDrawString(bgi_state.display, bgi_state.window, bgi_state.gc, x, y, text, strlen(text));
	XFlush(bgi_state.display);
}

void outtext(const char *text) {
	outtextxy(bgi_state.x, bgi_state.y, text);
}

void setbkcolor(int color) {
	if (bgi_state.initialized) {
		XSetBackground(bgi_state.display, bgi_state.gc, bgi_colors_x11[color & 15]);
	}
}

int getbkcolor(void) {
	if (bgi_state.initialized) {
		XGCValues values;
		XGetGCValues(bgi_state.display, bgi_state.gc, GCBackground, &values);
		for (int i = 0; i < 16; i++) {
			if (bgi_colors_x11[i] == values.background) return i;
		}
	}
	return 0;
}

int getcolor(void) {
	return bgi_state.color;
}

int getmaxx(void) {
	return bgi_state.width - 1;
}

int getmaxy(void) {
	return bgi_state.height - 1;
}

int getx(void) {
	return bgi_state.x;
}

int gety(void) {
	return bgi_state.y;
}

#endif /* Unix/X11 */
