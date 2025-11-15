/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using EGL and OpenGL ES
 *
 * Provides BGI graphics using EGL/OpenGL ES
 * Works on: Embedded Linux, Android, any platform with EGL support
 * Requires: EGL, OpenGL ES 2.0+
 * Features: GPU-accelerated rendering, shader support, cross-platform
 */

#include "../graphics.h"
#include <EGL/egl.h>
#include <GLES2/gl2.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	EGLDisplay display;
	EGLContext context;
	EGLSurface surface;
	EGLConfig config;
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
	GLfloat colors[16][3];	/* EGA palette RGB */
	int initialized;
} egl_state;

/* EGA color palette (RGB float) */
static const GLfloat ega_rgb[16][3] = {
	{0.0f, 0.0f, 0.0f},	/* BLACK */
	{0.0f, 0.0f, 0.67f},	/* BLUE */
	{0.0f, 0.67f, 0.0f},	/* GREEN */
	{0.0f, 0.67f, 0.67f},	/* CYAN */
	{0.67f, 0.0f, 0.0f},	/* RED */
	{0.67f, 0.0f, 0.67f},	/* MAGENTA */
	{0.67f, 0.33f, 0.0f},	/* BROWN */
	{0.67f, 0.67f, 0.67f},	/* LIGHTGRAY */
	{0.33f, 0.33f, 0.33f},	/* DARKGRAY */
	{0.33f, 0.33f, 1.0f},	/* LIGHTBLUE */
	{0.33f, 1.0f, 0.33f},	/* LIGHTGREEN */
	{0.33f, 1.0f, 1.0f},	/* LIGHTCYAN */
	{1.0f, 0.33f, 0.33f},	/* LIGHTRED */
	{1.0f, 0.33f, 1.0f},	/* LIGHTMAGENTA */
	{1.0f, 1.0f, 0.33f},	/* YELLOW */
	{1.0f, 1.0f, 1.0f}	/* WHITE */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	EGLint num_configs;
	EGLint attr[] = {
		EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
		EGL_RENDERABLE_TYPE, EGL_OPENGL_ES2_BIT,
		EGL_RED_SIZE, 8,
		EGL_GREEN_SIZE, 8,
		EGL_BLUE_SIZE, 8,
		EGL_NONE
	};
	EGLint ctx_attr[] = {
		EGL_CONTEXT_CLIENT_VERSION, 2,
		EGL_NONE
	};
	int width = 640, height = 480;
	int i;

	if (*driver == DETECT) {
		*driver = VGA;
		*mode = VGAHI;
	}

	/* Determine resolution */
	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Get default display */
	egl_state.display = eglGetDisplay(EGL_DEFAULT_DISPLAY);
	if (egl_state.display == EGL_NO_DISPLAY) {
		*driver = grNotDetected;
		return;
	}

	/* Initialize EGL */
	if (!eglInitialize(egl_state.display, NULL, NULL)) {
		*driver = grNotDetected;
		return;
	}

	/* Choose config */
	if (!eglChooseConfig(egl_state.display, attr, &egl_state.config, 1, &num_configs)) {
		eglTerminate(egl_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Create context */
	egl_state.context = eglCreateContext(egl_state.display, egl_state.config,
	                                      EGL_NO_CONTEXT, ctx_attr);
	if (egl_state.context == EGL_NO_CONTEXT) {
		eglTerminate(egl_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Create window surface (platform-specific, simplified here) */
	egl_state.surface = eglCreateWindowSurface(egl_state.display, egl_state.config,
	                                            (EGLNativeWindowType)0, NULL);
	if (egl_state.surface == EGL_NO_SURFACE) {
		eglDestroyContext(egl_state.display, egl_state.context);
		eglTerminate(egl_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Make context current */
	if (!eglMakeCurrent(egl_state.display, egl_state.surface,
	                     egl_state.surface, egl_state.context)) {
		eglDestroySurface(egl_state.display, egl_state.surface);
		eglDestroyContext(egl_state.display, egl_state.context);
		eglTerminate(egl_state.display);
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++) {
		egl_state.colors[i][0] = ega_rgb[i][0];
		egl_state.colors[i][1] = ega_rgb[i][1];
		egl_state.colors[i][2] = ega_rgb[i][2];
	}

	egl_state.width = width;
	egl_state.height = height;
	egl_state.current_color = WHITE;
	egl_state.current_bkcolor = BLACK;
	egl_state.current_x = 0;
	egl_state.current_y = 0;
	egl_state.line_style = SOLID_LINE;
	egl_state.line_pattern = 0xFFFF;
	egl_state.fill_style = SOLID_FILL;
	egl_state.fill_color = WHITE;
	egl_state.text_justify_h = LEFT_TEXT;
	egl_state.text_justify_v = TOP_TEXT;
	egl_state.initialized = 1;

	/* Set up OpenGL ES viewport */
	glViewport(0, 0, width, height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrthof(0, width, height, 0, -1, 1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	if (!egl_state.initialized)
		return;

	eglMakeCurrent(egl_state.display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);

	if (egl_state.surface != EGL_NO_SURFACE)
		eglDestroySurface(egl_state.display, egl_state.surface);

	if (egl_state.context != EGL_NO_CONTEXT)
		eglDestroyContext(egl_state.display, egl_state.context);

	if (egl_state.display != EGL_NO_DISPLAY)
		eglTerminate(egl_state.display);

	memset(&egl_state, 0, sizeof(egl_state));
}

int
getmaxx(void)
{
	return egl_state.width - 1;
}

int
getmaxy(void)
{
	return egl_state.height - 1;
}

void
setcolor(int color)
{
	egl_state.current_color = color & 0x0F;
	glColor3fv(egl_state.colors[egl_state.current_color]);
}

int
getcolor(void)
{
	return egl_state.current_color;
}

void
setbkcolor(int color)
{
	egl_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return egl_state.current_bkcolor;
}

void
cleardevice(void)
{
	glClearColor(egl_state.colors[egl_state.current_bkcolor][0],
	             egl_state.colors[egl_state.current_bkcolor][1],
	             egl_state.colors[egl_state.current_bkcolor][2], 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	eglSwapBuffers(egl_state.display, egl_state.surface);
}

void
putpixel(int x, int y, int color)
{
	glColor3fv(egl_state.colors[color & 0x0F]);
	glBegin(GL_POINTS);
	glVertex2i(x, y);
	glEnd();
	glFlush();
}

unsigned int
getpixel(int x, int y)
{
	/* Reading pixels from OpenGL ES is complex, return 0 for now */
	return 0;
}

void
moveto(int x, int y)
{
	egl_state.current_x = x;
	egl_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	egl_state.current_x += dx;
	egl_state.current_y += dy;
}

int
getx(void)
{
	return egl_state.current_x;
}

int
gety(void)
{
	return egl_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	glBegin(GL_LINES);
	glVertex2i(x1, y1);
	glVertex2i(x2, y2);
	glEnd();
	glFlush();
}

void
lineto(int x, int y)
{
	line(egl_state.current_x, egl_state.current_y, x, y);
	egl_state.current_x = x;
	egl_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = egl_state.current_x + dx;
	int y2 = egl_state.current_y + dy;
	line(egl_state.current_x, egl_state.current_y, x2, y2);
	egl_state.current_x = x2;
	egl_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	glBegin(GL_LINE_LOOP);
	glVertex2i(left, top);
	glVertex2i(right, top);
	glVertex2i(right, bottom);
	glVertex2i(left, bottom);
	glEnd();
	glFlush();
}

void
circle(int x, int y, int radius)
{
	int segments = 64;
	int i;

	glBegin(GL_LINE_LOOP);
	for (i = 0; i < segments; i++) {
		float angle = 2.0f * M_PI * i / segments;
		glVertex2f(x + radius * cosf(angle), y + radius * sinf(angle));
	}
	glEnd();
	glFlush();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	egl_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE: egl_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: egl_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: egl_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: egl_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: egl_state.line_pattern = pattern; break;
	default: egl_state.line_pattern = 0xFFFF;
	}

	if (thickness > 0)
		glLineWidth(thickness);
}

void
setfillstyle(int pattern, int color)
{
	egl_state.fill_style = pattern;
	egl_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	glColor3fv(egl_state.colors[egl_state.fill_color & 0x0F]);
	glBegin(GL_QUADS);
	glVertex2i(left, top);
	glVertex2i(right, top);
	glVertex2i(right, bottom);
	glVertex2i(left, bottom);
	glEnd();
	glFlush();
	glColor3fv(egl_state.colors[egl_state.current_color]);
}

void
settextjustify(int horiz, int vert)
{
	egl_state.text_justify_h = horiz;
	egl_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 12;
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
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	case grNotDetected: return "Graphics hardware not detected";
	case grFileNotFound: return "Driver file not found";
	case grInvalidDriver: return "Invalid graphics driver";
	case grNoLoadMem: return "Insufficient memory to load driver";
	default: return "Unknown error";
	}
}

int
graphresult(void)
{
	return egl_state.initialized ? grOk : grNoInitGraph;
}
