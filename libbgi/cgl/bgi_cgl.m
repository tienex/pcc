/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using CGL (macOS OpenGL)
 *
 * Provides hardware-accelerated BGI graphics on macOS
 * Works on: macOS 10.0+ (deprecated on modern macOS, prefer Metal)
 * Requires: Cocoa, OpenGL framework
 * Features: GPU acceleration
 */

#include "../graphics.h"
#import <Cocoa/Cocoa.h>
#import <OpenGL/gl.h>
#import <OpenGL/OpenGL.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

@interface BGI_CGLView : NSOpenGLView
@end

@implementation BGI_CGLView
- (void)drawRect:(NSRect)dirtyRect {
	[[self openGLContext] makeCurrentContext];
	glFlush();
}
@end

static struct {
	NSWindow *window;
	BGI_CGLView *view;
	NSOpenGLContext *context;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	GLfloat colors[16][3];
	int initialized;
} cgl_state;

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
	NSRect frame;
	int width = 640, height = 480, i;
	NSOpenGLPixelFormatAttribute attrs[] = {
		NSOpenGLPFADoubleBuffer,
		NSOpenGLPFAColorSize, 24,
		NSOpenGLPFADepthSize, 16,
		0
	};
	NSOpenGLPixelFormat *pixelFormat;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	[NSApplication sharedApplication];

	frame = NSMakeRect(0, 0, width, height);
	cgl_state.window = [[NSWindow alloc]
	                     initWithContentRect:frame
	                     styleMask:(NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask)
	                     backing:NSBackingStoreBuffered
	                     defer:NO];

	if (!cgl_state.window) { *driver = grNotDetected; return; }

	[cgl_state.window setTitle:@"BGI Graphics (CGL)"];
	[cgl_state.window center];

	pixelFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
	if (!pixelFormat) {
		[cgl_state.window release];
		*driver = grNotDetected;
		return;
	}

	cgl_state.view = [[BGI_CGLView alloc] initWithFrame:frame pixelFormat:pixelFormat];
	[pixelFormat release];

	if (!cgl_state.view) {
		[cgl_state.window release];
		*driver = grNotDetected;
		return;
	}

	[cgl_state.window setContentView:cgl_state.view];
	cgl_state.context = [cgl_state.view openGLContext];
	[cgl_state.context makeCurrentContext];

	for (i = 0; i < 16; i++)
		memcpy(cgl_state.colors[i], ega_rgb[i], sizeof(GLfloat) * 3);

	cgl_state.width = width;
	cgl_state.height = height;
	cgl_state.current_color = WHITE;
	cgl_state.current_bkcolor = BLACK;
	cgl_state.current_x = 0;
	cgl_state.current_y = 0;
	cgl_state.line_style = SOLID_LINE;
	cgl_state.line_pattern = 0xFFFF;
	cgl_state.fill_style = SOLID_FILL;
	cgl_state.fill_color = WHITE;
	cgl_state.text_justify_h = LEFT_TEXT;
	cgl_state.text_justify_v = TOP_TEXT;
	cgl_state.initialized = 1;

	glViewport(0, 0, width, height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, width, height, 0, -1, 1);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	[cgl_state.window makeKeyAndOrderFront:nil];

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!cgl_state.initialized) return;
	[NSOpenGLContext clearCurrentContext];
	if (cgl_state.window) {
		[cgl_state.window close];
		[cgl_state.window release];
	}
	if (cgl_state.view)
		[cgl_state.view release];
	memset(&cgl_state, 0, sizeof(cgl_state));
}

int getmaxx(void) { return cgl_state.width - 1; }
int getmaxy(void) { return cgl_state.height - 1; }
void setcolor(int color) {
	cgl_state.current_color = color & 0x0F;
	glColor3fv(cgl_state.colors[cgl_state.current_color]);
}
int getcolor(void) { return cgl_state.current_color; }
void setbkcolor(int color) { cgl_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return cgl_state.current_bkcolor; }

void cleardevice(void) {
	glClearColor(cgl_state.colors[cgl_state.current_bkcolor][0],
	             cgl_state.colors[cgl_state.current_bkcolor][1],
	             cgl_state.colors[cgl_state.current_bkcolor][2], 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	[cgl_state.context flushBuffer];
}

void putpixel(int x, int y, int color) {
	glColor3fv(cgl_state.colors[color & 0x0F]);
	glBegin(GL_POINTS); glVertex2i(x, y); glEnd();
	glFlush();
}

unsigned int getpixel(int x, int y) { return 0; }
void moveto(int x, int y) { cgl_state.current_x = x; cgl_state.current_y = y; }
void moverel(int dx, int dy) { cgl_state.current_x += dx; cgl_state.current_y += dy; }
int getx(void) { return cgl_state.current_x; }
int gety(void) { return cgl_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	glBegin(GL_LINES);
	glVertex2i(x1, y1); glVertex2i(x2, y2);
	glEnd(); glFlush();
}

void lineto(int x, int y) {
	line(cgl_state.current_x, cgl_state.current_y, x, y);
	cgl_state.current_x = x; cgl_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = cgl_state.current_x + dx, y2 = cgl_state.current_y + dy;
	line(cgl_state.current_x, cgl_state.current_y, x2, y2);
	cgl_state.current_x = x2; cgl_state.current_y = y2;
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
	cgl_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: cgl_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: cgl_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: cgl_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: cgl_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: cgl_state.line_pattern = pattern; break;
	default: cgl_state.line_pattern = 0xFFFF;
	}
	if (thickness > 0) glLineWidth((GLfloat)thickness);
}

void setfillstyle(int pattern, int color) {
	cgl_state.fill_style = pattern;
	cgl_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	glColor3fv(cgl_state.colors[cgl_state.fill_color & 0x0F]);
	glBegin(GL_QUADS);
	glVertex2i(left, top); glVertex2i(right, top);
	glVertex2i(right, bottom); glVertex2i(left, bottom);
	glEnd(); glFlush();
	glColor3fv(cgl_state.colors[cgl_state.current_color]);
}

void settextjustify(int horiz, int vert) {
	cgl_state.text_justify_h = horiz;
	cgl_state.text_justify_v = vert;
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
	return cgl_state.initialized ? grOk : grNoInitGraph;
}
