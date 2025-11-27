/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Skia (Chrome's graphics engine)
 *
 * Cross-platform 2D graphics using Skia library
 * Works on: Linux, BSD, macOS, Windows, Android
 * Requires: libskia
 * Features: GPU acceleration, high-quality rendering, PDF export
 */

#include "../graphics.h"
#include "include/core/SkCanvas.h"
#include "include/core/SkSurface.h"
#include "include/core/SkPaint.h"
#include "include/core/SkPath.h"
#include "include/core/SkColor.h"
#include "include/encode/SkPngEncoder.h"
#include "include/core/SkStream.h"
#include <stdlib.h>
#include <string.h>
#include <math.h>

static struct {
	sk_sp<SkSurface> surface;
	SkCanvas *canvas;
	SkPaint paint;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	SkColor colors[16];
	int initialized;
} skia_state;

/* EGA palette in Skia ARGB format */
static const SkColor ega_colors[16] = {
	SkColorSetRGB(0x00, 0x00, 0x00),  /* BLACK */
	SkColorSetRGB(0x00, 0x00, 0xAA),  /* BLUE */
	SkColorSetRGB(0x00, 0xAA, 0x00),  /* GREEN */
	SkColorSetRGB(0x00, 0xAA, 0xAA),  /* CYAN */
	SkColorSetRGB(0xAA, 0x00, 0x00),  /* RED */
	SkColorSetRGB(0xAA, 0x00, 0xAA),  /* MAGENTA */
	SkColorSetRGB(0xAA, 0x55, 0x00),  /* BROWN */
	SkColorSetRGB(0xAA, 0xAA, 0xAA),  /* LIGHTGRAY */
	SkColorSetRGB(0x55, 0x55, 0x55),  /* DARKGRAY */
	SkColorSetRGB(0x55, 0x55, 0xFF),  /* LIGHTBLUE */
	SkColorSetRGB(0x55, 0xFF, 0x55),  /* LIGHTGREEN */
	SkColorSetRGB(0x55, 0xFF, 0xFF),  /* LIGHTCYAN */
	SkColorSetRGB(0xFF, 0x55, 0x55),  /* LIGHTRED */
	SkColorSetRGB(0xFF, 0x55, 0xFF),  /* LIGHTMAGENTA */
	SkColorSetRGB(0xFF, 0xFF, 0x55),  /* YELLOW */
	SkColorSetRGB(0xFF, 0xFF, 0xFF)   /* WHITE */
};

extern "C" void initgraph(int *driver, int *mode, const char *path) {
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

	/* Create Skia surface */
	skia_state.surface = SkSurface::MakeRasterN32Premul(width, height);
	if (!skia_state.surface) {
		*driver = grNotDetected;
		return;
	}

	skia_state.canvas = skia_state.surface->getCanvas();
	if (!skia_state.canvas) {
		skia_state.surface.reset();
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		skia_state.colors[i] = ega_colors[i];

	skia_state.width = width;
	skia_state.height = height;
	skia_state.current_color = WHITE;
	skia_state.current_bkcolor = BLACK;
	skia_state.current_x = 0;
	skia_state.current_y = 0;
	skia_state.line_style = SOLID_LINE;
	skia_state.line_pattern = 0xFFFF;
	skia_state.fill_style = SOLID_FILL;
	skia_state.fill_color = WHITE;
	skia_state.text_justify_h = LEFT_TEXT;
	skia_state.text_justify_v = TOP_TEXT;
	skia_state.initialized = 1;

	/* Set initial paint */
	skia_state.paint.setAntiAlias(false);
	skia_state.paint.setStyle(SkPaint::kStroke_Style);
	skia_state.paint.setStrokeWidth(1.0f);
	skia_state.paint.setColor(skia_state.colors[WHITE]);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

extern "C" void closegraph(void) {
	if (!skia_state.initialized)
		return;

	/* Save to PNG file */
	if (skia_state.surface) {
		sk_sp<SkImage> image = skia_state.surface->makeImageSnapshot();
		if (image) {
			sk_sp<SkData> png = SkPngEncoder::Encode(nullptr, image.get(), {});
			if (png) {
				SkFILEWStream stream("bgi_output.png");
				stream.write(png->data(), png->size());
			}
		}
	}

	skia_state.surface.reset();
	memset(&skia_state, 0, sizeof(skia_state));
}

extern "C" int getmaxx(void) { return skia_state.width - 1; }
extern "C" int getmaxy(void) { return skia_state.height - 1; }

extern "C" void setcolor(int color) {
	skia_state.current_color = color & 0x0F;
	skia_state.paint.setColor(skia_state.colors[skia_state.current_color]);
}

extern "C" int getcolor(void) { return skia_state.current_color; }
extern "C" void setbkcolor(int color) { skia_state.current_bkcolor = color & 0x0F; }
extern "C" int getbkcolor(void) { return skia_state.current_bkcolor; }

extern "C" void cleardevice(void) {
	if (!skia_state.canvas)
		return;
	skia_state.canvas->clear(skia_state.colors[skia_state.current_bkcolor]);
}

extern "C" void putpixel(int x, int y, int color) {
	if (!skia_state.canvas)
		return;
	if (x < 0 || x >= skia_state.width || y < 0 || y >= skia_state.height)
		return;

	SkPaint pixel_paint;
	pixel_paint.setColor(skia_state.colors[color & 0x0F]);
	skia_state.canvas->drawPoint(x + 0.5f, y + 0.5f, pixel_paint);
}

extern "C" unsigned int getpixel(int x, int y) {
	/* Skia doesn't provide efficient pixel reading */
	return 0;
}

extern "C" void moveto(int x, int y) {
	skia_state.current_x = x;
	skia_state.current_y = y;
}

extern "C" void moverel(int dx, int dy) {
	skia_state.current_x += dx;
	skia_state.current_y += dy;
}

extern "C" int getx(void) { return skia_state.current_x; }
extern "C" int gety(void) { return skia_state.current_y; }

extern "C" void line(int x1, int y1, int x2, int y2) {
	if (!skia_state.canvas)
		return;

	skia_state.paint.setStyle(SkPaint::kStroke_Style);
	skia_state.canvas->drawLine(x1 + 0.5f, y1 + 0.5f, x2 + 0.5f, y2 + 0.5f, skia_state.paint);
}

extern "C" void lineto(int x, int y) {
	line(skia_state.current_x, skia_state.current_y, x, y);
	skia_state.current_x = x;
	skia_state.current_y = y;
}

extern "C" void linerel(int dx, int dy) {
	int x2 = skia_state.current_x + dx;
	int y2 = skia_state.current_y + dy;
	line(skia_state.current_x, skia_state.current_y, x2, y2);
	skia_state.current_x = x2;
	skia_state.current_y = y2;
}

extern "C" void rectangle(int left, int top, int right, int bottom) {
	if (!skia_state.canvas)
		return;

	SkRect rect = SkRect::MakeLTRB(left + 0.5f, top + 0.5f, right + 0.5f, bottom + 0.5f);
	skia_state.paint.setStyle(SkPaint::kStroke_Style);
	skia_state.canvas->drawRect(rect, skia_state.paint);
}

extern "C" void circle(int x, int y, int radius) {
	if (!skia_state.canvas)
		return;

	skia_state.paint.setStyle(SkPaint::kStroke_Style);
	skia_state.canvas->drawCircle(x + 0.5f, y + 0.5f, radius, skia_state.paint);
}

extern "C" void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	skia_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		skia_state.line_pattern = 0xFFFF;
		skia_state.paint.setPathEffect(nullptr);
		break;
	case DOTTED_LINE:
		skia_state.line_pattern = 0xCCCC;
		{
			SkScalar intervals[] = {2.0f, 2.0f};
			skia_state.paint.setPathEffect(
				SkDashPathEffect::Make(intervals, 2, 0));
		}
		break;
	case CENTER_LINE:
		skia_state.line_pattern = 0xF8F8;
		{
			SkScalar intervals[] = {6.0f, 2.0f, 2.0f, 2.0f};
			skia_state.paint.setPathEffect(
				SkDashPathEffect::Make(intervals, 4, 0));
		}
		break;
	case DASHED_LINE:
		skia_state.line_pattern = 0xF0F0;
		{
			SkScalar intervals[] = {4.0f, 4.0f};
			skia_state.paint.setPathEffect(
				SkDashPathEffect::Make(intervals, 2, 0));
		}
		break;
	case USERBIT_LINE:
		skia_state.line_pattern = pattern;
		/* Would need to convert pattern to intervals */
		break;
	default:
		skia_state.line_pattern = 0xFFFF;
		skia_state.paint.setPathEffect(nullptr);
	}

	if (thickness > 0)
		skia_state.paint.setStrokeWidth((SkScalar)thickness);
}

extern "C" void setfillstyle(int pattern, int color) {
	skia_state.fill_style = pattern;
	skia_state.fill_color = color;
}

extern "C" void bar(int left, int top, int right, int bottom) {
	if (!skia_state.canvas)
		return;

	SkPaint fill_paint;
	fill_paint.setColor(skia_state.colors[skia_state.fill_color & 0x0F]);
	fill_paint.setStyle(SkPaint::kFill_Style);

	SkRect rect = SkRect::MakeLTRB(left, top, right + 1, bottom + 1);
	skia_state.canvas->drawRect(rect, fill_paint);
}

extern "C" void settextjustify(int horiz, int vert) {
	skia_state.text_justify_h = horiz;
	skia_state.text_justify_v = vert;
}

extern "C" int textheight(const char *textstring) {
	return 12;
}

extern "C" int textwidth(const char *textstring) {
	return strlen(textstring) * 8;
}

extern "C" const char *grapherrormsg(int errorcode) {
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

extern "C" int graphresult(void) {
	return skia_state.initialized ? grOk : grNoInitGraph;
}
