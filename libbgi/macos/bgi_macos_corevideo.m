/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for macOS using Core Video Framework
 *
 * Provides BGI graphics on macOS using Core Video for display
 * Works on: macOS 10.4+ (Tiger and later)
 * Requires: Core Video framework, Cocoa
 * Features: Hardware-accelerated video display, CVDisplayLink timing
 */

#include "../graphics.h"
#import <Cocoa/Cocoa.h>
#import <CoreVideo/CoreVideo.h>
#import <CoreGraphics/CoreGraphics.h>
#include <stdlib.h>
#include <string.h>

@interface BGI_CVView : NSView
{
	CVPixelBufferRef pixelBuffer;
	CIContext *ciContext;
}
- (void)updateWithPixelBuffer:(CVPixelBufferRef)buffer;
@end

@implementation BGI_CVView

- (id)initWithFrame:(NSRect)frameRect
{
	self = [super initWithFrame:frameRect];
	if (self) {
		pixelBuffer = NULL;
		ciContext = [[CIContext contextWithOptions:nil] retain];
	}
	return self;
}

- (void)updateWithPixelBuffer:(CVPixelBufferRef)buffer
{
	if (pixelBuffer) CVPixelBufferRelease(pixelBuffer);
	pixelBuffer = CVPixelBufferRetain(buffer);
	[self setNeedsDisplay:YES];
}

- (void)drawRect:(NSRect)dirtyRect
{
	if (!pixelBuffer) return;
	
	CGContextRef context = [[NSGraphicsContext currentContext] CGContext];
	CIImage *ciImage = [CIImage imageWithCVPixelBuffer:pixelBuffer];
	CGImageRef cgImage = [ciContext createCGImage:ciImage fromRect:[ciImage extent]];
	
	CGContextDrawImage(context, self.bounds, cgImage);
	CGImageRelease(cgImage);
}

- (void)dealloc
{
	if (pixelBuffer) CVPixelBufferRelease(pixelBuffer);
	[ciContext release];
	[super dealloc];
}

@end

static struct {
	NSWindow *window;
	BGI_CVView *view;
	CVPixelBufferRef pixelBuffer;
	CVDisplayLinkRef displayLink;
	void *baseAddress;
	int width, height, bytesPerRow;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	uint32_t colors[16];
	int initialized;
} cv_state;

static const uint32_t ega_palette[16] = {
	0xFF000000, 0xFF0000AA, 0xFF00AA00, 0xFF00AAAA,
	0xFFAA0000, 0xFFAA00AA, 0xFFAA5500, 0xFFAAAAAA,
	0xFF555555, 0xFF5555FF, 0xFF55FF55, 0xFF55FFFF,
	0xFFFF5555, 0xFFFF55FF, 0xFFFFFF55, 0xFFFFFFFF
};

static CVReturn displayLinkCallback(CVDisplayLinkRef displayLink, const CVTimeStamp *now,
                                     const CVTimeStamp *outputTime, CVOptionFlags flagsIn,
                                     CVOptionFlags *flagsOut, void *displayLinkContext)
{
	return kCVReturnSuccess;
}

void
initgraph(int *driver, int *mode, const char *path)
{
	NSRect frame;
	int width = 640, height = 480, i;
	NSDictionary *pixelAttributes;

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
	cv_state.window = [[NSWindow alloc]
	                    initWithContentRect:frame
	                    styleMask:(NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask)
	                    backing:NSBackingStoreBuffered
	                    defer:NO];

	if (!cv_state.window) { *driver = grNotDetected; return; }

	[cv_state.window setTitle:@"BGI Graphics (Core Video)"];
	[cv_state.window center];

	cv_state.view = [[BGI_CVView alloc] initWithFrame:frame];
	[cv_state.window setContentView:cv_state.view];

	/* Create pixel buffer */
	pixelAttributes = [NSDictionary dictionaryWithObjectsAndKeys:
	                   [NSNumber numberWithBool:YES], (id)kCVPixelBufferCGImageCompatibilityKey,
	                   [NSNumber numberWithBool:YES], (id)kCVPixelBufferCGBitmapContextCompatibilityKey,
	                   nil];

	CVPixelBufferCreate(kCFAllocatorDefault, width, height,
	                    kCVPixelFormatType_32ARGB, (CFDictionaryRef)pixelAttributes,
	                    &cv_state.pixelBuffer);

	if (!cv_state.pixelBuffer) {
		[cv_state.view release];
		[cv_state.window release];
		*driver = grNotDetected;
		return;
	}

	CVPixelBufferLockBaseAddress(cv_state.pixelBuffer, 0);
	cv_state.baseAddress = CVPixelBufferGetBaseAddress(cv_state.pixelBuffer);
	cv_state.bytesPerRow = CVPixelBufferGetBytesPerRow(cv_state.pixelBuffer);

	/* Create display link for vsync */
	CVDisplayLinkCreateWithActiveCGDisplays(&cv_state.displayLink);
	CVDisplayLinkSetOutputCallback(cv_state.displayLink, &displayLinkCallback, NULL);
	CVDisplayLinkStart(cv_state.displayLink);

	for (i = 0; i < 16; i++)
		cv_state.colors[i] = ega_palette[i];

	cv_state.width = width;
	cv_state.height = height;
	cv_state.current_color = WHITE;
	cv_state.current_bkcolor = BLACK;
	cv_state.current_x = 0;
	cv_state.current_y = 0;
	cv_state.line_style = SOLID_LINE;
	cv_state.line_pattern = 0xFFFF;
	cv_state.fill_style = SOLID_FILL;
	cv_state.fill_color = WHITE;
	cv_state.text_justify_h = LEFT_TEXT;
	cv_state.text_justify_v = TOP_TEXT;
	cv_state.initialized = 1;

	[cv_state.window makeKeyAndOrderFront:nil];

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	if (!cv_state.initialized) return;
	
	if (cv_state.displayLink) {
		CVDisplayLinkStop(cv_state.displayLink);
		CVDisplayLinkRelease(cv_state.displayLink);
	}
	
	if (cv_state.pixelBuffer) {
		CVPixelBufferUnlockBaseAddress(cv_state.pixelBuffer, 0);
		CVPixelBufferRelease(cv_state.pixelBuffer);
	}
	
	if (cv_state.window) {
		[cv_state.window close];
		[cv_state.window release];
	}
	
	if (cv_state.view)
		[cv_state.view release];
	
	memset(&cv_state, 0, sizeof(cv_state));
}

int getmaxx(void) { return cv_state.width - 1; }
int getmaxy(void) { return cv_state.height - 1; }
void setcolor(int color) { cv_state.current_color = color & 0x0F; }
int getcolor(void) { return cv_state.current_color; }
void setbkcolor(int color) { cv_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return cv_state.current_bkcolor; }

static void update_display(void) {
	CVPixelBufferUnlockBaseAddress(cv_state.pixelBuffer, 0);
	[cv_state.view updateWithPixelBuffer:cv_state.pixelBuffer];
	CVPixelBufferLockBaseAddress(cv_state.pixelBuffer, 0);
}

void cleardevice(void) {
	uint32_t *fb = (uint32_t *)cv_state.baseAddress;
	uint32_t bg = cv_state.colors[cv_state.current_bkcolor];
	int i;
	for (i = 0; i < cv_state.width * cv_state.height; i++)
		fb[i] = bg;
	update_display();
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < cv_state.width && y >= 0 && y < cv_state.height) {
		uint32_t *row = (uint32_t *)((char *)cv_state.baseAddress + y * cv_state.bytesPerRow);
		row[x] = cv_state.colors[color & 0x0F];
	}
}

unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= cv_state.width || y < 0 || y >= cv_state.height) return 0;
	uint32_t *row = (uint32_t *)((char *)cv_state.baseAddress + y * cv_state.bytesPerRow);
	uint32_t pixel = row[x];
	int i;
	for (i = 0; i < 16; i++)
		if (cv_state.colors[i] == pixel)
			return i;
	return 0;
}

void moveto(int x, int y) { cv_state.current_x = x; cv_state.current_y = y; }
void moverel(int dx, int dy) { cv_state.current_x += dx; cv_state.current_y += dy; }
int getx(void) { return cv_state.current_x; }
int gety(void) { return cv_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;
	while (1) {
		if (cv_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, cv_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
	update_display();
}

void lineto(int x, int y) {
	line(cv_state.current_x, cv_state.current_y, x, y);
	cv_state.current_x = x; cv_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = cv_state.current_x + dx, y2 = cv_state.current_y + dy;
	line(cv_state.current_x, cv_state.current_y, x2, y2);
	cv_state.current_x = x2; cv_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;
	while (dx <= dy) {
		putpixel(x + dx, y + dy, cv_state.current_color);
		putpixel(x - dx, y + dy, cv_state.current_color);
		putpixel(x + dx, y - dy, cv_state.current_color);
		putpixel(x - dx, y - dy, cv_state.current_color);
		putpixel(x + dy, y + dx, cv_state.current_color);
		putpixel(x - dy, y + dx, cv_state.current_color);
		putpixel(x + dy, y - dx, cv_state.current_color);
		putpixel(x - dy, y - dx, cv_state.current_color);
		if (d < 0) d += 2 * dx + 3;
		else { d += 2 * (dx - dy) + 5; dy--; }
		dx++;
	}
	update_display();
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	cv_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: cv_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: cv_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: cv_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: cv_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: cv_state.line_pattern = pattern; break;
	default: cv_state.line_pattern = 0xFFFF;
	}
}

void setfillstyle(int pattern, int color) {
	cv_state.fill_style = pattern;
	cv_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	int x, y;
	for (y = top; y <= bottom; y++)
		for (x = left; x <= right; x++)
			putpixel(x, y, cv_state.fill_color);
	update_display();
}

void settextjustify(int horiz, int vert) {
	cv_state.text_justify_h = horiz;
	cv_state.text_justify_v = vert;
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
	return cv_state.initialized ? grOk : grNoInitGraph;
}
