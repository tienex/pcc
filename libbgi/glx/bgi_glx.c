/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using GLX (OpenGL Extension to X11)
 *
 * Provides hardware-accelerated BGI graphics on Unix/Linux with X11
 * Works on: Linux, BSD, Unix with X11 and OpenGL
 * Requires: libX11, libGL, GLX extension
 * Features: GPU acceleration, high performance
 */

#include "../graphics.h"
#include <X11/Xlib.h>
#include <GL/gl.h>
#include <GL/glx.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct {
	Display *display;
	Window window;
	GLXContext glx_context;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	GLfloat colors[16][3];
	int initialized;
} glx_state;

static const GLfloat ega_rgb[16][3] = {
	{0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.67f}, {0.0f, 0.67f, 0.0f},
	{0.0f, 0.67f, 0.67f}, {0.67f, 0.0f, 0.0f}, {0.67f, 0.0f, 0.67f},
	{0.67f, 0.33f, 0.0f}, {0.67f, 0.67f, 0.67f}, {0.33f, 0.33f, 0.33f},
	{0.33f, 0.33f, 1.0f}, {0.33f, 1.0f, 0.33f}, {0.33f, 1.0f, 1.0f},
	{1.0f, 0.33f, 0.33f}, {1.0f, 0.33f, 1.0f}, {1.0f, 1.0f, 0.33f},
	{1.0f, 1.0f, 1.0f}
};

void
initgraph(int *driver, int *mode, const char *path)
{
	int width = 640, height = 480, i;
	int glx_attr[] = {GLX_RGBA, GLX_DOUBLEBUFFER, None};
	XVisualInfo *vi;
	Colormap cmap;
	XSetWindowAttributes swa;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	glx_state.display = XOpenDisplay(NULL);
	if (!glx_state.display) { *driver = grNotDetected; return; }

	vi = glXChooseVisual(glx_state.display, 0, glx_attr);
	if (!vi) {
		XCloseDisplay(glx_state.display);
		*driver = grNotDetected;
		return;
	}

	cmap = XCreateColormap(glx_state.display,
	                        RootWindow(glx_state.display, vi->screen),
	                        vi->visual, AllocNone);
	swa.colormap = cmap;
	swa.event_mask = ExposureMask | KeyPressMask;

	glx_state.window = XCreateWindow(glx_state.display,
	                                  RootWindow(glx_state.display, vi->screen),
	                                  0, 0, width, height, 0, vi->depth,
	                                  InputOutput, vi->visual,
	                                  CWColormap | CWEventMask, &swa);

	XMapWindow(glx_state.display, glx_state.window);
	XStoreName(glx_state.display, glx_state.window, "BGI Graphics (GLX)");

	glx_state.glx_context = glXCreateContext(glx_state.display, vi, NULL, GL_TRUE);
	XFree(vi);

	if (!glx_state.glx_context) {
		XDestroyWindow(glx_state.display, glx_state.window);
		XCloseDisplay(glx_state.display);
		*driver = grNotDetected;
		return;
	}

	glXMakeCurrent(glx_state.display, glx_state.window, glx_state.glx_context);

	for (i = 0; i < 16; i++)
		memcpy(glx_state.colors[i], ega_rgb[i], sizeof(GLfloat) * 3);

	glx_state.width = width;
	glx_state.height = height;
	glx_state.current_color = WHITE;
	glx_state.current_bkcolor = BLACK;
	glx_state.current_x = 0;
	glx_state.current_y = 0;
	glx_state.line_style = SOLID_LINE;
	glx_state.line_pattern = 0xFFFF;
	glx_state.fill_style = SOLID_FILL;
	glx_state.fill_color = WHITE;
	glx_state.text_justify_h = LEFT_TEXT;
	glx_state.text_justify_v = TOP_TEXT;
	glx_state.initialized = 1;

	glViewport(0, 0, width, height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, width, height, 0, -1, 1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!glx_state.initialized) return;
	glXMakeCurrent(glx_state.display, None, NULL);
	glXDestroyContext(glx_state.display, glx_state.glx_context);
	XDestroyWindow(glx_state.display, glx_state.window);
	XCloseDisplay(glx_state.display);
	memset(&glx_state, 0, sizeof(glx_state));
}

int getmaxx(void) { return glx_state.width - 1; }
int getmaxy(void) { return glx_state.height - 1; }
void setcolor(int color) {
	glx_state.current_color = color & 0x0F;
	glColor3fv(glx_state.colors[glx_state.current_color]);
}
int getcolor(void) { return glx_state.current_color; }
void setbkcolor(int color) { glx_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return glx_state.current_bkcolor; }

void cleardevice(void) {
	glClearColor(glx_state.colors[glx_state.current_bkcolor][0],
	             glx_state.colors[glx_state.current_bkcolor][1],
	             glx_state.colors[glx_state.current_bkcolor][2], 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	glXSwapBuffers(glx_state.display, glx_state.window);
}

void putpixel(int x, int y, int color) {
	glColor3fv(glx_state.colors[color & 0x0F]);
	glBegin(GL_POINTS); glVertex2i(x, y); glEnd();
	glFlush();
}

unsigned int getpixel(int x, int y) { return 0; }
void moveto(int x, int y) { glx_state.current_x = x; glx_state.current_y = y; }
void moverel(int dx, int dy) { glx_state.current_x += dx; glx_state.current_y += dy; }
int getx(void) { return glx_state.current_x; }
int gety(void) { return glx_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	glBegin(GL_LINES);
	glVertex2i(x1, y1); glVertex2i(x2, y2);
	glEnd(); glFlush();
}

void lineto(int x, int y) {
	line(glx_state.current_x, glx_state.current_y, x, y);
	glx_state.current_x = x; glx_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = glx_state.current_x + dx, y2 = glx_state.current_y + dy;
	line(glx_state.current_x, glx_state.current_y, x2, y2);
	glx_state.current_x = x2; glx_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	glBegin(GL_LINE_LOOP);
	glVertex2i(left, top); glVertex2i(right, top);
	glVertex2i(right, bottom); glVertex2i(left, bottom);
	glEnd(); glFlush();
}

void circle(int x, int y, int radius) {
	int i, segments = 64;
	glBegin(GL_LINE_LOOP);
	for (i = 0; i < segments; i++) {
		float angle = 2.0f * M_PI * i / segments;
		glVertex2f(x + radius * cosf(angle), y + radius * sinf(angle));
	}
	glEnd(); glFlush();
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	glx_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: glx_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: glx_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: glx_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: glx_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: glx_state.line_pattern = pattern; break;
	default: glx_state.line_pattern = 0xFFFF;
	}
	if (thickness > 0) glLineWidth(thickness);
}

void setfillstyle(int pattern, int color) {
	glx_state.fill_style = pattern;
	glx_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	glColor3fv(glx_state.colors[glx_state.fill_color & 0x0F]);
	glBegin(GL_QUADS);
	glVertex2i(left, top); glVertex2i(right, top);
	glVertex2i(right, bottom); glVertex2i(left, bottom);
	glEnd(); glFlush();
	glColor3fv(glx_state.colors[glx_state.current_color]);
}

void settextjustify(int horiz, int vert) {
	glx_state.text_justify_h = horiz;
	glx_state.text_justify_v = vert;
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
	return glx_state.initialized ? grOk : grNoInitGraph;
}
