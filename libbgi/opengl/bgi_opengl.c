/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using OpenGL (generic cross-platform)
 *
 * Generic OpenGL 2.x/3.x/4.x backend
 * Works on: Any platform with OpenGL support
 * Requires: OpenGL context (created externally)
 * Features: GPU acceleration, programmable pipeline
 */

#include "../graphics.h"
#include <GL/gl.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct {
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	GLfloat colors[16][3];
	int initialized;
} gl_state;

static const GLfloat ega_rgb[16][3] = {
	{0.0f, 0.0f, 0.0f}, {0.0f, 0.0f, 0.67f}, {0.0f, 0.67f, 0.0f},
	{0.0f, 0.67f, 0.67f}, {0.67f, 0.0f, 0.0f}, {0.67f, 0.0f, 0.67f},
	{0.67f, 0.33f, 0.0f}, {0.67f, 0.67f, 0.67f}, {0.33f, 0.33f, 0.33f},
	{0.33f, 0.33f, 1.0f}, {0.33f, 1.0f, 0.33f}, {0.33f, 1.0f, 1.0f},
	{1.0f, 0.33f, 0.33f}, {1.0f, 0.33f, 1.0f}, {1.0f, 1.0f, 0.33f},
	{1.0f, 1.0f, 1.0f}
};

void initgraph(int *driver, int *mode, const char *path) {
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	gl_state.width = width;
	gl_state.height = height;

	for (i = 0; i < 16; i++)
		memcpy(gl_state.colors[i], ega_rgb[i], sizeof(GLfloat) * 3);

	gl_state.current_color = WHITE;
	gl_state.current_bkcolor = BLACK;
	gl_state.current_x = 0;
	gl_state.current_y = 0;
	gl_state.line_style = SOLID_LINE;
	gl_state.line_pattern = 0xFFFF;
	gl_state.fill_style = SOLID_FILL;
	gl_state.fill_color = WHITE;
	gl_state.text_justify_h = LEFT_TEXT;
	gl_state.text_justify_v = TOP_TEXT;
	gl_state.initialized = 1;

	glViewport(0, 0, width, height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, width, height, 0, -1, 1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	memset(&gl_state, 0, sizeof(gl_state));
}

int getmaxx(void) { return gl_state.width - 1; }
int getmaxy(void) { return gl_state.height - 1; }

void setcolor(int color) {
	gl_state.current_color = color & 0x0F;
	glColor3fv(gl_state.colors[gl_state.current_color]);
}

int getcolor(void) { return gl_state.current_color; }
void setbkcolor(int color) { gl_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return gl_state.current_bkcolor; }

void cleardevice(void) {
	glClearColor(gl_state.colors[gl_state.current_bkcolor][0],
	             gl_state.colors[gl_state.current_bkcolor][1],
	             gl_state.colors[gl_state.current_bkcolor][2], 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
}

void putpixel(int x, int y, int color) {
	glColor3fv(gl_state.colors[color & 0x0F]);
	glBegin(GL_POINTS);
	glVertex2i(x, y);
	glEnd();
}

unsigned int getpixel(int x, int y) { return 0; }
void moveto(int x, int y) { gl_state.current_x = x; gl_state.current_y = y; }
void moverel(int dx, int dy) { gl_state.current_x += dx; gl_state.current_y += dy; }
int getx(void) { return gl_state.current_x; }
int gety(void) { return gl_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	glBegin(GL_LINES);
	glVertex2i(x1, y1);
	glVertex2i(x2, y2);
	glEnd();
}

void lineto(int x, int y) {
	line(gl_state.current_x, gl_state.current_y, x, y);
	gl_state.current_x = x; gl_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = gl_state.current_x + dx, y2 = gl_state.current_y + dy;
	line(gl_state.current_x, gl_state.current_y, x2, y2);
	gl_state.current_x = x2; gl_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	glBegin(GL_LINE_LOOP);
	glVertex2i(left, top);
	glVertex2i(right, top);
	glVertex2i(right, bottom);
	glVertex2i(left, bottom);
	glEnd();
}

void circle(int x, int y, int radius) {
	int i, segments = 64;
	glBegin(GL_LINE_LOOP);
	for (i = 0; i < segments; i++) {
		float angle = 2.0f * M_PI * i / segments;
		glVertex2f(x + radius * cosf(angle), y + radius * sinf(angle));
	}
	glEnd();
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	gl_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: gl_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: gl_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: gl_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: gl_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: gl_state.line_pattern = pattern; break;
	default: gl_state.line_pattern = 0xFFFF;
	}
	if (thickness > 0) glLineWidth(thickness);
}

void setfillstyle(int pattern, int color) {
	gl_state.fill_style = pattern;
	gl_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	glColor3fv(gl_state.colors[gl_state.fill_color & 0x0F]);
	glBegin(GL_QUADS);
	glVertex2i(left, top);
	glVertex2i(right, top);
	glVertex2i(right, bottom);
	glVertex2i(left, bottom);
	glEnd();
	glColor3fv(gl_state.colors[gl_state.current_color]);
}

void settextjustify(int horiz, int vert) {
	gl_state.text_justify_h = horiz;
	gl_state.text_justify_v = vert;
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
	return gl_state.initialized ? grOk : grNoInitGraph;
}
