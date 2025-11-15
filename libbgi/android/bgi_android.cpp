/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Android Canvas/SurfaceView
 *
 * Android graphics using Canvas API
 * Works on: Android 4.0+ (API 14+)
 * Requires: Android NDK, JNI
 * Features: Hardware-accelerated 2D graphics, touch support
 */

#include "../graphics.h"
#include <android/native_window.h>
#include <android/native_window_jni.h>
#include <android/log.h>
#include <jni.h>
#include <stdlib.h>
#include <string.h>

#define LOG_TAG "BGI_Android"
#define LOGI(...) __android_log_print(ANDROID_LOG_INFO, LOG_TAG, __VA_ARGS__)

static struct {
	ANativeWindow *window;
	ANativeWindow_Buffer buffer;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	uint32_t colors[16];
	int initialized;
} android_state;

/* EGA palette in ARGB8888 format */
static const uint32_t ega_argb[16] = {
	0xFF000000,  /* BLACK */
	0xFF0000AA,  /* BLUE */
	0xFF00AA00,  /* GREEN */
	0xFF00AAAA,  /* CYAN */
	0xFFAA0000,  /* RED */
	0xFFAA00AA,  /* MAGENTA */
	0xFFAA5500,  /* BROWN */
	0xFFAAAAAA,  /* LIGHTGRAY */
	0xFF555555,  /* DARKGRAY */
	0xFF5555FF,  /* LIGHTBLUE */
	0xFF55FF55,  /* LIGHTGREEN */
	0xFF55FFFF,  /* LIGHTCYAN */
	0xFFFF5555,  /* LIGHTRED */
	0xFFFF55FF,  /* LIGHTMAGENTA */
	0xFFFFFF55,  /* YELLOW */
	0xFFFFFFFF   /* WHITE */
};

static JavaVM *g_jvm = NULL;
static jobject g_surface_obj = NULL;

extern "C" JNIEXPORT void JNICALL
Java_com_pcc_bgi_BGIView_nativeInit(JNIEnv *env, jobject obj, jobject surface) {
	env->GetJavaVM(&g_jvm);
	g_surface_obj = env->NewGlobalRef(surface);
}

extern "C" void initgraph(int *driver, int *mode, const char *path) {
	JNIEnv *env;
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

	if (!g_jvm || !g_surface_obj) {
		*driver = grNotDetected;
		return;
	}

	if (g_jvm->GetEnv((void **)&env, JNI_VERSION_1_6) != JNI_OK) {
		*driver = grNotDetected;
		return;
	}

	/* Get native window from Surface */
	android_state.window = ANativeWindow_fromSurface(env, g_surface_obj);
	if (!android_state.window) {
		*driver = grNotDetected;
		return;
	}

	/* Set buffer geometry */
	ANativeWindow_setBuffersGeometry(android_state.window, width, height,
	                                   WINDOW_FORMAT_RGBX_8888);

	android_state.width = width;
	android_state.height = height;

	/* Copy EGA palette */
	for (i = 0; i < 16; i++)
		android_state.colors[i] = ega_argb[i];

	android_state.current_color = WHITE;
	android_state.current_bkcolor = BLACK;
	android_state.current_x = 0;
	android_state.current_y = 0;
	android_state.line_style = SOLID_LINE;
	android_state.line_pattern = 0xFFFF;
	android_state.fill_style = SOLID_FILL;
	android_state.fill_color = WHITE;
	android_state.text_justify_h = LEFT_TEXT;
	android_state.text_justify_v = TOP_TEXT;
	android_state.initialized = 1;

	LOGI("BGI initialized: %dx%d", width, height);

	cleardevice();
	*driver = 0;
	*mode = 0;
}

extern "C" void closegraph(void) {
	if (!android_state.initialized)
		return;

	if (android_state.window) {
		ANativeWindow_release(android_state.window);
		android_state.window = NULL;
	}

	memset(&android_state, 0, sizeof(android_state));
}

extern "C" int getmaxx(void) { return android_state.width - 1; }
extern "C" int getmaxy(void) { return android_state.height - 1; }
extern "C" void setcolor(int color) { android_state.current_color = color & 0x0F; }
extern "C" int getcolor(void) { return android_state.current_color; }
extern "C" void setbkcolor(int color) { android_state.current_bkcolor = color & 0x0F; }
extern "C" int getbkcolor(void) { return android_state.current_bkcolor; }

extern "C" void cleardevice(void) {
	if (!android_state.window)
		return;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	uint32_t color = android_state.colors[android_state.current_bkcolor];

	for (int y = 0; y < android_state.height; y++) {
		for (int x = 0; x < android_state.width; x++) {
			pixels[y * android_state.buffer.stride + x] = color;
		}
	}

	ANativeWindow_unlockAndPost(android_state.window);
}

extern "C" void putpixel(int x, int y, int color) {
	if (x < 0 || x >= android_state.width || y < 0 || y >= android_state.height)
		return;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	pixels[y * android_state.buffer.stride + x] = android_state.colors[color & 0x0F];

	ANativeWindow_unlockAndPost(android_state.window);
}

extern "C" unsigned int getpixel(int x, int y) {
	if (x < 0 || x >= android_state.width || y < 0 || y >= android_state.height)
		return 0;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return 0;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	uint32_t pixel = pixels[y * android_state.buffer.stride + x];

	ANativeWindow_unlockAndPost(android_state.window);

	/* Find matching EGA color */
	for (int i = 0; i < 16; i++)
		if (android_state.colors[i] == pixel)
			return i;

	return 0;
}

extern "C" void moveto(int x, int y) {
	android_state.current_x = x;
	android_state.current_y = y;
}

extern "C" void moverel(int dx, int dy) {
	android_state.current_x += dx;
	android_state.current_y += dy;
}

extern "C" int getx(void) { return android_state.current_x; }
extern "C" int gety(void) { return android_state.current_y; }

static int abs(int x) { return x < 0 ? -x : x; }

extern "C" void line(int x1, int y1, int x2, int y2) {
	if (!android_state.window)
		return;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	uint32_t color = android_state.colors[android_state.current_color];

	/* Bresenham's line algorithm */
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	while (1) {
		if (android_state.line_pattern & (1 << (pattern_pos & 15))) {
			if (x1 >= 0 && x1 < android_state.width &&
			    y1 >= 0 && y1 < android_state.height) {
				pixels[y1 * android_state.buffer.stride + x1] = color;
			}
		}
		if (x1 == x2 && y1 == y2)
			break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}

	ANativeWindow_unlockAndPost(android_state.window);
}

extern "C" void lineto(int x, int y) {
	line(android_state.current_x, android_state.current_y, x, y);
	android_state.current_x = x;
	android_state.current_y = y;
}

extern "C" void linerel(int dx, int dy) {
	int x2 = android_state.current_x + dx;
	int y2 = android_state.current_y + dy;
	line(android_state.current_x, android_state.current_y, x2, y2);
	android_state.current_x = x2;
	android_state.current_y = y2;
}

extern "C" void rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

extern "C" void circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	uint32_t color = android_state.colors[android_state.current_color];

	/* Bresenham's circle algorithm */
	while (dx <= dy) {
		#define PLOT(px, py) \
			if ((px) >= 0 && (px) < android_state.width && \
			    (py) >= 0 && (py) < android_state.height) \
				pixels[(py) * android_state.buffer.stride + (px)] = color

		PLOT(x + dx, y + dy); PLOT(x - dx, y + dy);
		PLOT(x + dx, y - dy); PLOT(x - dx, y - dy);
		PLOT(x + dy, y + dx); PLOT(x - dy, y + dx);
		PLOT(x + dy, y - dx); PLOT(x - dy, y - dx);

		#undef PLOT

		if (d < 0)
			d += 2 * dx + 3;
		else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}

	ANativeWindow_unlockAndPost(android_state.window);
}

extern "C" void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	android_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: android_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: android_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: android_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: android_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: android_state.line_pattern = pattern; break;
	default: android_state.line_pattern = 0xFFFF;
	}
}

extern "C" void setfillstyle(int pattern, int color) {
	android_state.fill_style = pattern;
	android_state.fill_color = color;
}

extern "C" void bar(int left, int top, int right, int bottom) {
	if (!android_state.window)
		return;

	if (ANativeWindow_lock(android_state.window, &android_state.buffer, NULL) < 0)
		return;

	uint32_t *pixels = (uint32_t *)android_state.buffer.bits;
	uint32_t color = android_state.colors[android_state.fill_color & 0x0F];

	for (int y = top; y <= bottom; y++) {
		for (int x = left; x <= right; x++) {
			if (x >= 0 && x < android_state.width &&
			    y >= 0 && y < android_state.height) {
				pixels[y * android_state.buffer.stride + x] = color;
			}
		}
	}

	ANativeWindow_unlockAndPost(android_state.window);
}

extern "C" void settextjustify(int horiz, int vert) {
	android_state.text_justify_h = horiz;
	android_state.text_justify_v = vert;
}

extern "C" int textheight(const char *textstring) { return 16; }
extern "C" int textwidth(const char *textstring) { return strlen(textstring) * 8; }

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
	return android_state.initialized ? grOk : grNoInitGraph;
}
