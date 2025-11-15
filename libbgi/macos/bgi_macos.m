/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for macOS using Cocoa and Core Graphics
 *
 * Provides BGI graphics on modern macOS
 * Works on: macOS 10.0+ (Intel and Apple Silicon)
 * Requires: Cocoa framework, Core Graphics
 */

#include "../graphics.h"
#import <Cocoa/Cocoa.h>
#import <CoreGraphics/CoreGraphics.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Custom view for drawing */
@interface BGIView : NSView
{
	CGContextRef context;
	CGLayerRef layer;
}
- (void)drawRect:(NSRect)dirtyRect;
- (CGLayerRef)layer;
@end

@implementation BGIView

- (id)initWithFrame:(NSRect)frameRect
{
	self = [super initWithFrame:frameRect];
	if (self) {
		layer = nil;
	}
	return self;
}

- (void)drawRect:(NSRect)dirtyRect
{
	context = [[NSGraphicsContext currentContext] CGContext];
	if (layer) {
		CGContextDrawLayerInRect(context, self.bounds, layer);
	}
}

- (CGLayerRef)layer
{
	return layer;
}

- (void)setLayer:(CGLayerRef)newLayer
{
	if (layer)
		CGLayerRelease(layer);
	layer = newLayer;
	if (layer)
		CGLayerRetain(layer);
}

- (void)dealloc
{
	if (layer)
		CGLayerRelease(layer);
	[super dealloc];
}

@end

/* Internal state */
static struct {
	NSWindow *window;
	BGIView *view;
	CGLayerRef layer;
	CGContextRef context;
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
	CGColorRef colors[16];	/* EGA palette */
	int initialized;
} macos_state;

/* EGA color palette (RGBA) */
static const CGFloat ega_components[16][4] = {
	{0.0, 0.0, 0.0, 1.0},		/* BLACK */
	{0.0, 0.0, 0.67, 1.0},		/* BLUE */
	{0.0, 0.67, 0.0, 1.0},		/* GREEN */
	{0.0, 0.67, 0.67, 1.0},		/* CYAN */
	{0.67, 0.0, 0.0, 1.0},		/* RED */
	{0.67, 0.0, 0.67, 1.0},		/* MAGENTA */
	{0.67, 0.33, 0.0, 1.0},		/* BROWN */
	{0.67, 0.67, 0.67, 1.0},	/* LIGHTGRAY */
	{0.33, 0.33, 0.33, 1.0},	/* DARKGRAY */
	{0.33, 0.33, 1.0, 1.0},		/* LIGHTBLUE */
	{0.33, 1.0, 0.33, 1.0},		/* LIGHTGREEN */
	{0.33, 1.0, 1.0, 1.0},		/* LIGHTCYAN */
	{1.0, 0.33, 0.33, 1.0},		/* LIGHTRED */
	{1.0, 0.33, 1.0, 1.0},		/* LIGHTMAGENTA */
	{1.0, 1.0, 0.33, 1.0},		/* YELLOW */
	{1.0, 1.0, 1.0, 1.0}		/* WHITE */
};

void
initgraph(int *driver, int *mode, const char *path)
{
	NSRect frame;
	int width = 640;
	int height = 480;
	CGColorSpaceRef colorspace;
	CGSize layerSize;
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

	/* Initialize Cocoa if not already */
	[NSApplication sharedApplication];

	/* Create window */
	frame = NSMakeRect(0, 0, width, height);
	macos_state.window = [[NSWindow alloc]
	                       initWithContentRect:frame
	                       styleMask:(NSTitledWindowMask | NSClosableWindowMask | NSMiniaturizableWindowMask)
	                       backing:NSBackingStoreBuffered
	                       defer:NO];

	if (!macos_state.window) {
		*driver = grNotDetected;
		return;
	}

	[macos_state.window setTitle:@"BGI Graphics"];
	[macos_state.window center];

	/* Create custom view */
	macos_state.view = [[BGIView alloc] initWithFrame:frame];
	[macos_state.window setContentView:macos_state.view];

	/* Create CGLayer for off-screen rendering */
	layerSize = CGSizeMake(width, height);
	CGContextRef tempContext = [[NSGraphicsContext currentContext] CGContext];
	macos_state.layer = CGLayerCreateWithContext(tempContext, layerSize, NULL);

	if (!macos_state.layer) {
		[macos_state.view release];
		[macos_state.window release];
		*driver = grNotDetected;
		return;
	}

	macos_state.context = CGLayerGetContext(macos_state.layer);
	[macos_state.view setLayer:macos_state.layer];

	/* Create EGA color palette */
	colorspace = CGColorSpaceCreateDeviceRGB();
	for (i = 0; i < 16; i++) {
		macos_state.colors[i] = CGColorCreate(colorspace, ega_components[i]);
	}
	CGColorSpaceRelease(colorspace);

	macos_state.width = width;
	macos_state.height = height;
	macos_state.current_color = WHITE;
	macos_state.current_bkcolor = BLACK;
	macos_state.current_x = 0;
	macos_state.current_y = 0;
	macos_state.line_style = SOLID_LINE;
	macos_state.line_pattern = 0xFFFF;
	macos_state.fill_style = SOLID_FILL;
	macos_state.fill_color = WHITE;
	macos_state.text_justify_h = LEFT_TEXT;
	macos_state.text_justify_v = TOP_TEXT;
	macos_state.initialized = 1;

	/* Show window */
	[macos_state.window makeKeyAndOrderFront:nil];

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	int i;

	if (!macos_state.initialized)
		return;

	/* Release colors */
	for (i = 0; i < 16; i++) {
		if (macos_state.colors[i])
			CGColorRelease(macos_state.colors[i]);
	}

	/* Release layer */
	if (macos_state.layer)
		CGLayerRelease(macos_state.layer);

	/* Close window */
	if (macos_state.window) {
		[macos_state.window close];
		[macos_state.window release];
	}

	if (macos_state.view)
		[macos_state.view release];

	memset(&macos_state, 0, sizeof(macos_state));
}

int
getmaxx(void)
{
	return macos_state.width - 1;
}

int
getmaxy(void)
{
	return macos_state.height - 1;
}

void
setcolor(int color)
{
	macos_state.current_color = color & 0x0F;
	CGContextSetStrokeColorWithColor(macos_state.context,
	                                  macos_state.colors[macos_state.current_color]);
}

int
getcolor(void)
{
	return macos_state.current_color;
}

void
setbkcolor(int color)
{
	macos_state.current_bkcolor = color & 0x0F;
}

int
getbkcolor(void)
{
	return macos_state.current_bkcolor;
}

static void
update_display(void)
{
	[macos_state.view setNeedsDisplay:YES];
	[[NSRunLoop currentRunLoop] runUntilDate:[NSDate dateWithTimeIntervalSinceNow:0.001]];
}

void
cleardevice(void)
{
	CGRect rect = CGRectMake(0, 0, macos_state.width, macos_state.height);

	CGContextSetFillColorWithColor(macos_state.context,
	                                macos_state.colors[macos_state.current_bkcolor]);
	CGContextFillRect(macos_state.context, rect);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	CGRect rect;

	if (x >= 0 && x < macos_state.width && y >= 0 && y < macos_state.height) {
		/* Flip Y coordinate (macOS has origin at bottom-left) */
		y = macos_state.height - 1 - y;

		rect = CGRectMake(x, y, 1, 1);
		CGContextSetFillColorWithColor(macos_state.context,
		                                macos_state.colors[color & 0x0F]);
		CGContextFillRect(macos_state.context, rect);
	}
}

unsigned int
getpixel(int x, int y)
{
	/* Note: Getting pixel from CGLayer is expensive, would need to read bitmap data */
	/* For now, return 0 - full implementation would require maintaining a pixel buffer */
	return 0;
}

void
moveto(int x, int y)
{
	macos_state.current_x = x;
	macos_state.current_y = y;
}

void
moverel(int dx, int dy)
{
	macos_state.current_x += dx;
	macos_state.current_y += dy;
}

int
getx(void)
{
	return macos_state.current_x;
}

int
gety(void)
{
	return macos_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	/* Flip Y coordinates */
	y1 = macos_state.height - 1 - y1;
	y2 = macos_state.height - 1 - y2;

	if (macos_state.line_style == SOLID_LINE) {
		CGContextBeginPath(macos_state.context);
		CGContextMoveToPoint(macos_state.context, x1, y1);
		CGContextAddLineToPoint(macos_state.context, x2, y2);
		CGContextStrokePath(macos_state.context);
	} else {
		/* Bresenham's line algorithm for styled lines */
		int dx = abs(x2 - x1);
		int dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1;
		int sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy;
		int e2;
		int pattern_pos = 0;
		int orig_y1 = macos_state.height - 1 - y1;
		int orig_y2 = macos_state.height - 1 - y2;

		/* Restore original Y for pixel plotting */
		y1 = orig_y1;
		y2 = orig_y2;

		while (1) {
			if (macos_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, macos_state.current_color);

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
	line(macos_state.current_x, macos_state.current_y, x, y);
	macos_state.current_x = x;
	macos_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = macos_state.current_x + dx;
	int y2 = macos_state.current_y + dy;
	line(macos_state.current_x, macos_state.current_y, x2, y2);
	macos_state.current_x = x2;
	macos_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	CGRect rect;

	/* Flip Y coordinates */
	int temp = top;
	top = macos_state.height - 1 - bottom;
	bottom = macos_state.height - 1 - temp;

	rect = CGRectMake(left, top, right - left, bottom - top);
	CGContextStrokeRect(macos_state.context, rect);
	update_display();
}

void
circle(int x, int y, int radius)
{
	CGRect rect;

	/* Flip Y coordinate */
	y = macos_state.height - 1 - y;

	rect = CGRectMake(x - radius, y - radius, radius * 2, radius * 2);
	CGContextStrokeEllipseInRect(macos_state.context, rect);
	update_display();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	macos_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		macos_state.line_pattern = 0xFFFF;
		CGContextSetLineDash(macos_state.context, 0, NULL, 0);
		break;
	case DOTTED_LINE:
		macos_state.line_pattern = 0xCCCC;
		{
			CGFloat lengths[] = {2, 2};
			CGContextSetLineDash(macos_state.context, 0, lengths, 2);
		}
		break;
	case CENTER_LINE:
		macos_state.line_pattern = 0xF8F8;
		{
			CGFloat lengths[] = {10, 2, 2, 2};
			CGContextSetLineDash(macos_state.context, 0, lengths, 4);
		}
		break;
	case DASHED_LINE:
		macos_state.line_pattern = 0xF0F0;
		{
			CGFloat lengths[] = {8, 4};
			CGContextSetLineDash(macos_state.context, 0, lengths, 2);
		}
		break;
	case USERBIT_LINE:
		macos_state.line_pattern = pattern;
		/* Custom pattern - simplified */
		{
			CGFloat lengths[] = {4, 4};
			CGContextSetLineDash(macos_state.context, 0, lengths, 2);
		}
		break;
	default:
		macos_state.line_pattern = 0xFFFF;
		CGContextSetLineDash(macos_state.context, 0, NULL, 0);
	}

	if (thickness > 0)
		CGContextSetLineWidth(macos_state.context, thickness);
}

void
setfillstyle(int pattern, int color)
{
	macos_state.fill_style = pattern;
	macos_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	CGRect rect;

	/* Flip Y coordinates */
	int temp = top;
	top = macos_state.height - 1 - bottom;
	bottom = macos_state.height - 1 - temp;

	rect = CGRectMake(left, top, right - left + 1, bottom - top + 1);
	CGContextSetFillColorWithColor(macos_state.context,
	                                macos_state.colors[macos_state.fill_color & 0x0F]);
	CGContextFillRect(macos_state.context, rect);
	update_display();
}

void
settextjustify(int horiz, int vert)
{
	macos_state.text_justify_h = horiz;
	macos_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	return 12;	/* Default system font height */
}

int
textwidth(const char *textstring)
{
	NSString *str = [NSString stringWithUTF8String:textstring];
	NSDictionary *attrs = @{NSFontAttributeName: [NSFont systemFontOfSize:12]};
	NSSize size = [str sizeWithAttributes:attrs];
	return (int)size.width;
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
	return macos_state.initialized ? grOk : grNoInitGraph;
}
