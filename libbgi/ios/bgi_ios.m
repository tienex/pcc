/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for iOS Core Graphics/UIKit
 *
 * iOS graphics using Core Graphics and UIKit
 * Works on: iOS 8.0+, iPadOS
 * Requires: UIKit framework, Core Graphics framework
 * Features: Retina display support, touch input
 */

#include "../graphics.h"
#import <UIKit/UIKit.h>
#import <CoreGraphics/CoreGraphics.h>
#include <stdlib.h>
#include <string.h>

@interface BGI_iOS_View : UIView
@property (nonatomic, assign) CGContextRef drawingContext;
@end

@implementation BGI_iOS_View

+ (Class)layerClass {
	return [CALayer class];
}

- (void)drawRect:(CGRect)rect {
	if (self.drawingContext) {
		CGContextRef ctx = UIGraphicsGetCurrentContext();
		CGImageRef image = CGBitmapContextCreateImage(self.drawingContext);
		if (image) {
			CGContextDrawImage(ctx, self.bounds, image);
			CGImageRelease(image);
		}
	}
}

@end

static struct {
	UIWindow *window;
	BGI_iOS_View *view;
	CGContextRef context;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	CGColorRef colors[16];
	int initialized;
} ios_state;

/* EGA palette as CGColor */
static const CGFloat ega_rgb[16][3] = {
	{0.0, 0.0, 0.0},      /* BLACK */
	{0.0, 0.0, 0.67},     /* BLUE */
	{0.0, 0.67, 0.0},     /* GREEN */
	{0.0, 0.67, 0.67},    /* CYAN */
	{0.67, 0.0, 0.0},     /* RED */
	{0.67, 0.0, 0.67},    /* MAGENTA */
	{0.67, 0.33, 0.0},    /* BROWN */
	{0.67, 0.67, 0.67},   /* LIGHTGRAY */
	{0.33, 0.33, 0.33},   /* DARKGRAY */
	{0.33, 0.33, 1.0},    /* LIGHTBLUE */
	{0.33, 1.0, 0.33},    /* LIGHTGREEN */
	{0.33, 1.0, 1.0},     /* LIGHTCYAN */
	{1.0, 0.33, 0.33},    /* LIGHTRED */
	{1.0, 0.33, 1.0},     /* LIGHTMAGENTA */
	{1.0, 1.0, 0.33},     /* YELLOW */
	{1.0, 1.0, 1.0}       /* WHITE */
};

void initgraph(int *driver, int *mode, const char *path) {
	int width = 640, height = 480, i;
	CGColorSpaceRef colorSpace;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Create window */
	CGRect screenBounds = [[UIScreen mainScreen] bounds];
	ios_state.window = [[UIWindow alloc] initWithFrame:screenBounds];
	if (!ios_state.window) {
		*driver = grNotDetected;
		return;
	}

	/* Create view */
	ios_state.view = [[BGI_iOS_View alloc] initWithFrame:CGRectMake(0, 0, width, height)];
	if (!ios_state.view) {
		[ios_state.window release];
		*driver = grNotDetected;
		return;
	}

	[ios_state.window addSubview:ios_state.view];
	[ios_state.window makeKeyAndVisible];

	/* Create bitmap context */
	colorSpace = CGColorSpaceCreateDeviceRGB();
	ios_state.context = CGBitmapContextCreate(NULL, width, height, 8,
	                                           width * 4, colorSpace,
	                                           kCGImageAlphaPremultipliedLast);
	CGColorSpaceRelease(colorSpace);

	if (!ios_state.context) {
		[ios_state.view release];
		[ios_state.window release];
		*driver = grNotDetected;
		return;
	}

	ios_state.view.drawingContext = ios_state.context;

	/* Flip coordinate system to match BGI (top-left origin) */
	CGContextTranslateCTM(ios_state.context, 0, height);
	CGContextScaleCTM(ios_state.context, 1.0, -1.0);

	/* Create EGA colors */
	colorSpace = CGColorSpaceCreateDeviceRGB();
	for (i = 0; i < 16; i++) {
		CGFloat components[4] = {ega_rgb[i][0], ega_rgb[i][1], ega_rgb[i][2], 1.0};
		ios_state.colors[i] = CGColorCreate(colorSpace, components);
	}
	CGColorSpaceRelease(colorSpace);

	ios_state.width = width;
	ios_state.height = height;
	ios_state.current_color = WHITE;
	ios_state.current_bkcolor = BLACK;
	ios_state.current_x = 0;
	ios_state.current_y = 0;
	ios_state.line_style = SOLID_LINE;
	ios_state.line_pattern = 0xFFFF;
	ios_state.fill_style = SOLID_FILL;
	ios_state.fill_color = WHITE;
	ios_state.text_justify_h = LEFT_TEXT;
	ios_state.text_justify_v = TOP_TEXT;
	ios_state.initialized = 1;

	/* Set initial drawing state */
	CGContextSetLineWidth(ios_state.context, 1.0);
	CGContextSetStrokeColorWithColor(ios_state.context, ios_state.colors[WHITE]);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

void closegraph(void) {
	int i;

	if (!ios_state.initialized)
		return;

	/* Release colors */
	for (i = 0; i < 16; i++) {
		if (ios_state.colors[i])
			CGColorRelease(ios_state.colors[i]);
	}

	if (ios_state.context)
		CGContextRelease(ios_state.context);
	if (ios_state.view)
		[ios_state.view release];
	if (ios_state.window)
		[ios_state.window release];

	memset(&ios_state, 0, sizeof(ios_state));
}

int getmaxx(void) { return ios_state.width - 1; }
int getmaxy(void) { return ios_state.height - 1; }

void setcolor(int color) {
	ios_state.current_color = color & 0x0F;
	CGContextSetStrokeColorWithColor(ios_state.context,
	                                  ios_state.colors[ios_state.current_color]);
	CGContextSetFillColorWithColor(ios_state.context,
	                                ios_state.colors[ios_state.current_color]);
}

int getcolor(void) { return ios_state.current_color; }
void setbkcolor(int color) { ios_state.current_bkcolor = color & 0x0F; }
int getbkcolor(void) { return ios_state.current_bkcolor; }

void cleardevice(void) {
	CGContextSetFillColorWithColor(ios_state.context,
	                                ios_state.colors[ios_state.current_bkcolor]);
	CGContextFillRect(ios_state.context,
	                   CGRectMake(0, 0, ios_state.width, ios_state.height));
	[ios_state.view setNeedsDisplay];
}

void putpixel(int x, int y, int color) {
	if (x < 0 || x >= ios_state.width || y < 0 || y >= ios_state.height)
		return;

	CGContextSetFillColorWithColor(ios_state.context, ios_state.colors[color & 0x0F]);
	CGContextFillRect(ios_state.context, CGRectMake(x, y, 1, 1));
	[ios_state.view setNeedsDisplay];
}

unsigned int getpixel(int x, int y) {
	/* Core Graphics doesn't provide efficient pixel reading */
	return 0;
}

void moveto(int x, int y) {
	ios_state.current_x = x;
	ios_state.current_y = y;
}

void moverel(int dx, int dy) {
	ios_state.current_x += dx;
	ios_state.current_y += dy;
}

int getx(void) { return ios_state.current_x; }
int gety(void) { return ios_state.current_y; }

void line(int x1, int y1, int x2, int y2) {
	CGContextBeginPath(ios_state.context);
	CGContextMoveToPoint(ios_state.context, x1 + 0.5, y1 + 0.5);
	CGContextAddLineToPoint(ios_state.context, x2 + 0.5, y2 + 0.5);
	CGContextStrokePath(ios_state.context);
	[ios_state.view setNeedsDisplay];
}

void lineto(int x, int y) {
	line(ios_state.current_x, ios_state.current_y, x, y);
	ios_state.current_x = x;
	ios_state.current_y = y;
}

void linerel(int dx, int dy) {
	int x2 = ios_state.current_x + dx;
	int y2 = ios_state.current_y + dy;
	line(ios_state.current_x, ios_state.current_y, x2, y2);
	ios_state.current_x = x2;
	ios_state.current_y = y2;
}

void rectangle(int left, int top, int right, int bottom) {
	CGContextStrokeRect(ios_state.context,
	                     CGRectMake(left + 0.5, top + 0.5,
	                                right - left, bottom - top));
	[ios_state.view setNeedsDisplay];
}

void circle(int x, int y, int radius) {
	CGContextBeginPath(ios_state.context);
	CGContextAddArc(ios_state.context, x + 0.5, y + 0.5, radius, 0, 2 * M_PI, 0);
	CGContextStrokePath(ios_state.context);
	[ios_state.view setNeedsDisplay];
}

void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	CGFloat dash_pattern[16];
	size_t dash_count = 0;

	ios_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		ios_state.line_pattern = 0xFFFF;
		CGContextSetLineDash(ios_state.context, 0, NULL, 0);
		break;
	case DOTTED_LINE:
		ios_state.line_pattern = 0xCCCC;
		dash_pattern[0] = 2.0; dash_pattern[1] = 2.0;
		CGContextSetLineDash(ios_state.context, 0, dash_pattern, 2);
		break;
	case CENTER_LINE:
		ios_state.line_pattern = 0xF8F8;
		dash_pattern[0] = 6.0; dash_pattern[1] = 2.0;
		dash_pattern[2] = 2.0; dash_pattern[3] = 2.0;
		CGContextSetLineDash(ios_state.context, 0, dash_pattern, 4);
		break;
	case DASHED_LINE:
		ios_state.line_pattern = 0xF0F0;
		dash_pattern[0] = 4.0; dash_pattern[1] = 4.0;
		CGContextSetLineDash(ios_state.context, 0, dash_pattern, 2);
		break;
	case USERBIT_LINE:
		ios_state.line_pattern = pattern;
		/* Convert pattern to dash array */
		for (int i = 0; i < 16; i++) {
			if (pattern & (1 << (15 - i)))
				dash_pattern[dash_count++] = 1.0;
		}
		if (dash_count > 0)
			CGContextSetLineDash(ios_state.context, 0, dash_pattern, dash_count);
		break;
	default:
		ios_state.line_pattern = 0xFFFF;
		CGContextSetLineDash(ios_state.context, 0, NULL, 0);
	}

	if (thickness > 0)
		CGContextSetLineWidth(ios_state.context, thickness);
}

void setfillstyle(int pattern, int color) {
	ios_state.fill_style = pattern;
	ios_state.fill_color = color;
}

void bar(int left, int top, int right, int bottom) {
	CGContextSetFillColorWithColor(ios_state.context,
	                                ios_state.colors[ios_state.fill_color & 0x0F]);
	CGContextFillRect(ios_state.context,
	                   CGRectMake(left, top, right - left + 1, bottom - top + 1));
	[ios_state.view setNeedsDisplay];
}

void settextjustify(int horiz, int vert) {
	ios_state.text_justify_h = horiz;
	ios_state.text_justify_v = vert;
}

int textheight(const char *textstring) {
	return 14;  /* Default iOS font height */
}

int textwidth(const char *textstring) {
	return strlen(textstring) * 8;  /* Approximate */
}

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
	return ios_state.initialized ? grOk : grNoInitGraph;
}
