/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using WGL (Windows OpenGL)
 *
 * Provides hardware-accelerated BGI graphics on Windows
 * Works on: Windows 95/98/ME/NT/2000/XP/Vista/7/8/10/11
 * Requires: OpenGL
 * Features: GPU acceleration, high performance
 */

#include "../graphics.h"
#include <windows.h>
#include <GL/gl.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct {
	HWND hwnd;
	HDC hdc;
	HGLRC hglrc;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	GLfloat colors[16][3];
	int initialized;
} wgl_state;

static const GLfloat ega_rgb[16][3] = {
	{0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.67f}, {0.0f, 0.67f, 0.0f},
	{0.0f, 0.67f, 0.67f}, {0.67f, 0.0f, 0.0f}, {0.67f, 0.0f, 0.67f},
	{0.67f, 0.33f, 0.0f}, {0.67f, 0.67f, 0.67f}, {0.33f, 0.33f, 0.33f},
	{0.33f, 0.33f, 1.0f}, {0.33f, 1.0f, 0.33f}, {0.33f, 1.0f, 1.0f},
	{1.0f, 0.33f, 0.33f}, {1.0f, 0.33f, 1.0f}, {1.0f, 1.0f, 0.33f},
	{1.0f, 1.0f, 1.0f}
};

static LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch (msg) {
	case WM_CLOSE: PostQuitMessage(0); return 0;
	case WM_DESTROY: return 0;
	default: return DefWindowProc(hwnd, msg, wParam, lParam);
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	WNDCLASS wc = {0};
	PIXELFORMATDESCRIPTOR pfd;
	int pixel_format, width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	wc.lpfnWndProc = WndProc;
	wc.hInstance = GetModuleHandle(NULL);
	wc.lpszClassName = "BGI_WGL_Class";
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	RegisterClass(&wc);

	wgl_state.hwnd = CreateWindow("BGI_WGL_Class", "BGI Graphics (WGL)",
	                               WS_OVERLAPPEDWINDOW | WS_VISIBLE,
	                               CW_USEDEFAULT, CW_USEDEFAULT, width, height,
	                               NULL, NULL, wc.hInstance, NULL);
	if (!wgl_state.hwnd) { *driver = grNotDetected; return; }

	wgl_state.hdc = GetDC(wgl_state.hwnd);
	if (!wgl_state.hdc) {
		DestroyWindow(wgl_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	memset(&pfd, 0, sizeof(pfd));
	pfd.nSize = sizeof(pfd);
	pfd.nVersion = 1;
	pfd.dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
	pfd.iPixelType = PFD_TYPE_RGBA;
	pfd.cColorBits = 24;
	pfd.cDepthBits = 16;
	pfd.iLayerType = PFD_MAIN_PLANE;

	pixel_format = ChoosePixelFormat(wgl_state.hdc, &pfd);
	if (!pixel_format || !SetPixelFormat(wgl_state.hdc, pixel_format, &pfd)) {
		ReleaseDC(wgl_state.hwnd, wgl_state.hdc);
		DestroyWindow(wgl_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	wgl_state.hglrc = wglCreateContext(wgl_state.hdc);
	if (!wgl_state.hglrc) {
		ReleaseDC(wgl_state.hwnd, wgl_state.hdc);
		DestroyWindow(wgl_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	wglMakeCurrent(wgl_state.hdc, wgl_state.hglrc);

	for (i = 0; i < 16; i++)
		memcpy(wgl_state.colors[i], ega_rgb[i], sizeof(GLfloat) * 3);

	wgl_state.width = width;
	wgl_state.height = height;
	wgl_state.current_color = WHITE;
	wgl_state.current_bkcolor = BLACK;
	wgl_state.current_x = 0;
	wgl_state.current_y = 0;
	wgl_state.line_style = SOLID_LINE;
	wgl_state.line_pattern = 0xFFFF;
	wgl_state.fill_style = SOLID_FILL;
	wgl_state.fill_color = WHITE;
	wgl_state.text_justify_h = LEFT_TEXT;
	wgl_state.text_justify_v = TOP_TEXT;
	wgl_state.initialized = 1;

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
	if (!wgl_state.initialized) return;
	wglMakeCurrent(NULL, NULL);
	wglDeleteContext(wgl_state.hglrc);
	ReleaseDC(wgl_state.hwnd, wgl_state.hdc);
	DestroyWindow(wgl_state.hwnd);
	memset(&wgl_state, 0, sizeof(wgl_state));
}

int getmaxx(void) { return wgl_state.width - 1; }
int getmaxy(void) { return wgl_state.height - 1; }
void setcolor(int color) {
	wgl_state.current_color = color & 0x0F;
	glColor3fv(wgl_state.colors[wgl_state.current_color]);
}
int getcolor(void) { return wgl_state.current_color; }
void setbkcolor(int color) { wgl_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return wgl_state.current_bkcolor; }

void cleardevice(void) {
	glClearColor(wgl_state.colors[wgl_state.current_bkcolor][0],
	             wgl_state.colors[wgl_state.current_bkcolor][1],
	             wgl_state.colors[wgl_state.current_bkcolor][2], 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	SwapBuffers(wgl_state.hdc);
}

void putpixel(int x, int y, int color) {
	glColor3fv(wgl_state.colors[color & 0x0F]);
	glBegin(GL_POINTS); glVertex2i(x, y); glEnd();
	glFlush();
}

unsigned int getpixel(int x, int y) { return 0; }
void moveto(int x, int y) { wgl_state.current_x = x; wgl_state.current_y = y; }
void moverel(int dx, int dy) { wgl_state.current_x += dx; wgl_state.current_y += dy; }
int getx(void) { return wgl_state.current_x; }
int gety(void) { return wgl_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	glBegin(GL_LINES);
	glVertex2i(x1, y1); glVertex2i(x2, y2);
	glEnd(); glFlush();
}

void lineto(int x, int y) {
	line(wgl_state.current_x, wgl_state.current_y, x, y);
	wgl_state.current_x = x; wgl_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = wgl_state.current_x + dx, y2 = wgl_state.current_y + dy;
	line(wgl_state.current_x, wgl_state.current_y, x2, y2);
	wgl_state.current_x = x2; wgl_state.current_y = y2;
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
		float angle = 2.0f * 3.14159265f * i / segments;
		glVertex2f(x + radius * cosf(angle), y + radius * sinf(angle));
	}
	glEnd(); glFlush();
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	wgl_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: wgl_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: wgl_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: wgl_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: wgl_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: wgl_state.line_pattern = pattern; break;
	default: wgl_state.line_pattern = 0xFFFF;
	}
	if (thickness > 0) glLineWidth((GLfloat)thickness);
}

void setfillstyle(int pattern, int color) {
	wgl_state.fill_style = pattern;
	wgl_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	glColor3fv(wgl_state.colors[wgl_state.fill_color & 0x0F]);
	glBegin(GL_QUADS);
	glVertex2i(left, top); glVertex2i(right, top);
	glVertex2i(right, bottom); glVertex2i(left, bottom);
	glEnd(); glFlush();
	glColor3fv(wgl_state.colors[wgl_state.current_color]);
}

void settextjustify(int horiz, int vert) {
	wgl_state.text_justify_h = horiz;
	wgl_state.text_justify_v = vert;
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
	return wgl_state.initialized ? grOk : grNoInitGraph;
}
