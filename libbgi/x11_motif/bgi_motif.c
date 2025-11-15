/*
 * Copyright (c) 2025 PCC Project
 * Borland Graphics Interface (BGI) - Motif/X11 Implementation
 *
 * Uses Motif widgets for a more polished GUI appearance
 */

#include "../bgi.h"
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/Frame.h>
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
	Widget toplevel;
	Widget main_w;
	Widget frame;
	Widget drawing_area;
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

/* Exposure callback */
static void expose_callback(Widget w, XtPointer client_data, XtPointer call_data)
{
	(void)w; (void)client_data; (void)call_data;

	if (bgi_state.pixmap) {
		XCopyArea(bgi_state.display, bgi_state.pixmap, bgi_state.window,
		          bgi_state.gc, 0, 0, bgi_state.width, bgi_state.height, 0, 0);
	}
}

void initgraph(int *driver, int *mode, const char *path)
{
	(void)driver; (void)mode; (void)path;

	int argc = 0;
	char **argv = NULL;
	Arg args[10];
	int n;

	bgi_state.width = 640;
	bgi_state.height = 480;

	/* Initialize Motif */
	bgi_state.toplevel = XtVaAppInitialize(NULL, "BGIGraphics",
	                                        NULL, 0, &argc, argv, NULL, NULL);
	bgi_state.display = XtDisplay(bgi_state.toplevel);

	/* Create main window */
	bgi_state.main_w = XmCreateMainWindow(bgi_state.toplevel, "main", NULL, 0);
	XtManageChild(bgi_state.main_w);

	/* Create frame */
	n = 0;
	XtSetArg(args[n], XmNshadowType, XmSHADOW_IN); n++;
	bgi_state.frame = XmCreateFrame(bgi_state.main_w, "frame", args, n);
	XtManageChild(bgi_state.frame);

	/* Create drawing area */
	n = 0;
	XtSetArg(args[n], XmNwidth, bgi_state.width); n++;
	XtSetArg(args[n], XmNheight, bgi_state.height); n++;
	XtSetArg(args[n], XmNbackground, BlackPixel(bgi_state.display,
	                                             DefaultScreen(bgi_state.display))); n++;
	bgi_state.drawing_area = XmCreateDrawingArea(bgi_state.frame, "canvas", args, n);
	XtManageChild(bgi_state.drawing_area);

	/* Add expose callback */
	XtAddCallback(bgi_state.drawing_area, XmNexposeCallback, expose_callback, NULL);

	/* Realize widgets */
	XtRealizeWidget(bgi_state.toplevel);

	/* Get window */
	bgi_state.window = XtWindow(bgi_state.drawing_area);

	/* Create graphics context */
	bgi_state.gc = XCreateGC(bgi_state.display, bgi_state.window, 0, NULL);

	/* Create pixmap for double buffering */
	bgi_state.pixmap = XCreatePixmap(bgi_state.display, bgi_state.window,
	                                  bgi_state.width, bgi_state.height,
	                                  DefaultDepth(bgi_state.display,
	                                               DefaultScreen(bgi_state.display)));

	/* Clear pixmap to black */
	XSetForeground(bgi_state.display, bgi_state.gc,
	               BlackPixel(bgi_state.display, DefaultScreen(bgi_state.display)));
	XFillRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	               0, 0, bgi_state.width, bgi_state.height);

	bgi_state.color = WHITE;
	bgi_state.fillcolor = WHITE;
	bgi_state.x = bgi_state.y = 0;
	bgi_state.initialized = 1;

	/* Process initial events */
	XFlush(bgi_state.display);
	while (XtAppPending(XtWidgetToApplicationContext(bgi_state.toplevel))) {
		XEvent event;
		XtAppNextEvent(XtWidgetToApplicationContext(bgi_state.toplevel), &event);
		XtDispatchEvent(&event);
	}
}

void closegraph(void)
{
	if (!bgi_state.initialized)
		return;

	if (bgi_state.pixmap)
		XFreePixmap(bgi_state.display, bgi_state.pixmap);
	if (bgi_state.gc)
		XFreeGC(bgi_state.display, bgi_state.gc);
	if (bgi_state.toplevel)
		XtDestroyWidget(bgi_state.toplevel);

	bgi_state.initialized = 0;
}

void cleardevice(void)
{
	if (!bgi_state.initialized)
		return;

	XSetForeground(bgi_state.display, bgi_state.gc,
	               BlackPixel(bgi_state.display, DefaultScreen(bgi_state.display)));
	XFillRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	               0, 0, bgi_state.width, bgi_state.height);
	XCopyArea(bgi_state.display, bgi_state.pixmap, bgi_state.window,
	          bgi_state.gc, 0, 0, bgi_state.width, bgi_state.height, 0, 0);
	XFlush(bgi_state.display);
}

void setcolor(int color)
{
	if (!bgi_state.initialized)
		return;

	bgi_state.color = color;
	if (color >= 0 && color < 16) {
		XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[color]);
	}
}

void putpixel(int x, int y, int color)
{
	if (!bgi_state.initialized)
		return;

	if (color >= 0 && color < 16) {
		XSetForeground(bgi_state.display, bgi_state.gc, bgi_colors_x11[color]);
	}
	XDrawPoint(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x, y);
	XDrawPoint(bgi_state.display, bgi_state.window, bgi_state.gc, x, y);
	XFlush(bgi_state.display);
}

void line(int x1, int y1, int x2, int y2)
{
	if (!bgi_state.initialized)
		return;

	XDrawLine(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x1, y1, x2, y2);
	XDrawLine(bgi_state.display, bgi_state.window, bgi_state.gc, x1, y1, x2, y2);
	XFlush(bgi_state.display);
}

void rectangle(int left, int top, int right, int bottom)
{
	if (!bgi_state.initialized)
		return;

	int width = right - left;
	int height = bottom - top;

	XDrawRectangle(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	               left, top, width, height);
	XDrawRectangle(bgi_state.display, bgi_state.window, bgi_state.gc,
	               left, top, width, height);
	XFlush(bgi_state.display);
}

void circle(int x, int y, int radius)
{
	if (!bgi_state.initialized)
		return;

	XDrawArc(bgi_state.display, bgi_state.pixmap, bgi_state.gc,
	         x - radius, y - radius, radius * 2, radius * 2, 0, 360 * 64);
	XDrawArc(bgi_state.display, bgi_state.window, bgi_state.gc,
	         x - radius, y - radius, radius * 2, radius * 2, 0, 360 * 64);
	XFlush(bgi_state.display);
}

void outtextxy(int x, int y, const char *text)
{
	if (!bgi_state.initialized || !text)
		return;

	XDrawString(bgi_state.display, bgi_state.pixmap, bgi_state.gc, x, y, text, strlen(text));
	XDrawString(bgi_state.display, bgi_state.window, bgi_state.gc, x, y, text, strlen(text));
	XFlush(bgi_state.display);
}

int getmaxx(void) { return bgi_state.width - 1; }
int getmaxy(void) { return bgi_state.height - 1; }
void moveto(int x, int y) { bgi_state.x = x; bgi_state.y = y; }
int getx(void) { return bgi_state.x; }
int gety(void) { return bgi_state.y; }
void lineto(int x, int y) { line(bgi_state.x, bgi_state.y, x, y); bgi_state.x = x; bgi_state.y = y; }
