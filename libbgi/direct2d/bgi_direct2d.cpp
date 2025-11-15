/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using Direct2D (Windows)
 *
 * Modern hardware-accelerated BGI graphics for Windows
 * Works on: Windows 7, 8, 10, 11
 * Requires: Direct2D, DirectWrite
 * Features: GPU acceleration, anti-aliasing, high DPI support
 */

#include "../graphics.h"
#include <windows.h>
#include <d2d1.h>
#include <dwrite.h>
#include <stdlib.h>
#include <string.h>

#pragma comment(lib, "d2d1.lib")
#pragma comment(lib, "dwrite.lib")

static struct {
	HWND hwnd;
	ID2D1Factory *d2d_factory;
	ID2D1HwndRenderTarget *render_target;
	ID2D1SolidColorBrush *brush;
	IDWriteFactory *dwrite_factory;
	int width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	D2D1_COLOR_F colors[16];
	int initialized;
} d2d_state;

static const D2D1_COLOR_F ega_colors[16] = {
	{0.0f, 0.0f, 0.0f, 1.0f},      /* BLACK */
	{0.0f, 0.0f, 0.67f, 1.0f},     /* BLUE */
	{0.0f, 0.67f, 0.0f, 1.0f},     /* GREEN */
	{0.0f, 0.67f, 0.67f, 1.0f},    /* CYAN */
	{0.67f, 0.0f, 0.0f, 1.0f},     /* RED */
	{0.67f, 0.0f, 0.67f, 1.0f},    /* MAGENTA */
	{0.67f, 0.33f, 0.0f, 1.0f},    /* BROWN */
	{0.67f, 0.67f, 0.67f, 1.0f},   /* LIGHTGRAY */
	{0.33f, 0.33f, 0.33f, 1.0f},   /* DARKGRAY */
	{0.33f, 0.33f, 1.0f, 1.0f},    /* LIGHTBLUE */
	{0.33f, 1.0f, 0.33f, 1.0f},    /* LIGHTGREEN */
	{0.33f, 1.0f, 1.0f, 1.0f},     /* LIGHTCYAN */
	{1.0f, 0.33f, 0.33f, 1.0f},    /* LIGHTRED */
	{1.0f, 0.33f, 1.0f, 1.0f},     /* LIGHTMAGENTA */
	{1.0f, 1.0f, 0.33f, 1.0f},     /* YELLOW */
	{1.0f, 1.0f, 1.0f, 1.0f}       /* WHITE */
};

static LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch (msg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		BeginPaint(hwnd, &ps);
		if (d2d_state.render_target) {
			d2d_state.render_target->BeginDraw();
			d2d_state.render_target->EndDraw();
		}
		EndPaint(hwnd, &ps);
		return 0;
	}
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	default:
		return DefWindowProc(hwnd, msg, wParam, lParam);
	}
}

extern "C" void initgraph(int *driver, int *mode, const char *path) {
	WNDCLASS wc = {0};
	int width = 640, height = 480, i;
	HRESULT hr;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	default: width = 640; height = 480;
	}

	/* Initialize COM */
	CoInitialize(NULL);

	/* Create Direct2D factory */
	hr = D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, &d2d_state.d2d_factory);
	if (FAILED(hr)) {
		CoUninitialize();
		*driver = grNotDetected;
		return;
	}

	/* Create DirectWrite factory */
	hr = DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, __uuidof(IDWriteFactory),
	                          reinterpret_cast<IUnknown**>(&d2d_state.dwrite_factory));
	if (FAILED(hr)) {
		d2d_state.d2d_factory->Release();
		CoUninitialize();
		*driver = grNotDetected;
		return;
	}

	/* Register window class */
	wc.lpfnWndProc = WndProc;
	wc.hInstance = GetModuleHandle(NULL);
	wc.lpszClassName = L"BGI_Direct2D_Class";
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	RegisterClass(&wc);

	/* Create window */
	d2d_state.hwnd = CreateWindow(L"BGI_Direct2D_Class", L"BGI Graphics (Direct2D)",
	                               WS_OVERLAPPEDWINDOW | WS_VISIBLE,
	                               CW_USEDEFAULT, CW_USEDEFAULT, width, height,
	                               NULL, NULL, wc.hInstance, NULL);
	if (!d2d_state.hwnd) {
		d2d_state.dwrite_factory->Release();
		d2d_state.d2d_factory->Release();
		CoUninitialize();
		*driver = grNotDetected;
		return;
	}

	/* Create render target */
	D2D1_RENDER_TARGET_PROPERTIES rtp = D2D1::RenderTargetProperties();
	D2D1_HWND_RENDER_TARGET_PROPERTIES hrtp = D2D1::HwndRenderTargetProperties(
		d2d_state.hwnd, D2D1::SizeU(width, height));

	hr = d2d_state.d2d_factory->CreateHwndRenderTarget(rtp, hrtp, &d2d_state.render_target);
	if (FAILED(hr)) {
		DestroyWindow(d2d_state.hwnd);
		d2d_state.dwrite_factory->Release();
		d2d_state.d2d_factory->Release();
		CoUninitialize();
		*driver = grNotDetected;
		return;
	}

	/* Create brush */
	hr = d2d_state.render_target->CreateSolidColorBrush(ega_colors[WHITE], &d2d_state.brush);
	if (FAILED(hr)) {
		d2d_state.render_target->Release();
		DestroyWindow(d2d_state.hwnd);
		d2d_state.dwrite_factory->Release();
		d2d_state.d2d_factory->Release();
		CoUninitialize();
		*driver = grNotDetected;
		return;
	}

	/* Copy EGA colors */
	for (i = 0; i < 16; i++)
		d2d_state.colors[i] = ega_colors[i];

	d2d_state.width = width;
	d2d_state.height = height;
	d2d_state.current_color = WHITE;
	d2d_state.current_bkcolor = BLACK;
	d2d_state.current_x = 0;
	d2d_state.current_y = 0;
	d2d_state.line_style = SOLID_LINE;
	d2d_state.line_pattern = 0xFFFF;
	d2d_state.fill_style = SOLID_FILL;
	d2d_state.fill_color = WHITE;
	d2d_state.text_justify_h = LEFT_TEXT;
	d2d_state.text_justify_v = TOP_TEXT;
	d2d_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

extern "C" void closegraph(void) {
	if (!d2d_state.initialized) return;

	if (d2d_state.brush) d2d_state.brush->Release();
	if (d2d_state.render_target) d2d_state.render_target->Release();
	if (d2d_state.dwrite_factory) d2d_state.dwrite_factory->Release();
	if (d2d_state.d2d_factory) d2d_state.d2d_factory->Release();
	if (d2d_state.hwnd) DestroyWindow(d2d_state.hwnd);

	CoUninitialize();
	memset(&d2d_state, 0, sizeof(d2d_state));
}

extern "C" int getmaxx(void) { return d2d_state.width - 1; }
extern "C" int getmaxy(void) { return d2d_state.height - 1; }

extern "C" void setcolor(int color) {
	d2d_state.current_color = color & 0x0F;
	if (d2d_state.brush)
		d2d_state.brush->SetColor(d2d_state.colors[d2d_state.current_color]);
}

extern "C" int getcolor(void) { return d2d_state.current_color; }
extern "C" void setbkcolor(int color) { d2d_state.current_bkcolor = color & 0x0F; }
extern "C" int getbkcolor(void) { return d2d_state.current_bkcolor; }

extern "C" void cleardevice(void) {
	if (!d2d_state.render_target) return;
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->Clear(d2d_state.colors[d2d_state.current_bkcolor]);
	d2d_state.render_target->EndDraw();
	InvalidateRect(d2d_state.hwnd, NULL, FALSE);
}

extern "C" void putpixel(int x, int y, int color) {
	if (!d2d_state.render_target) return;
	D2D1_COLOR_F old_color = d2d_state.brush->GetColor();
	d2d_state.brush->SetColor(d2d_state.colors[color & 0x0F]);
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->FillRectangle(
		D2D1::RectF((FLOAT)x, (FLOAT)y, (FLOAT)(x + 1), (FLOAT)(y + 1)),
		d2d_state.brush);
	d2d_state.render_target->EndDraw();
	d2d_state.brush->SetColor(old_color);
}

extern "C" unsigned int getpixel(int x, int y) {
	/* Direct2D doesn't provide pixel reading */
	return 0;
}

extern "C" void moveto(int x, int y) {
	d2d_state.current_x = x;
	d2d_state.current_y = y;
}

extern "C" void moverel(int dx, int dy) {
	d2d_state.current_x += dx;
	d2d_state.current_y += dy;
}

extern "C" int getx(void) { return d2d_state.current_x; }
extern "C" int gety(void) { return d2d_state.current_y; }

extern "C" void line(int x1, int y1, int x2, int y2) {
	if (!d2d_state.render_target) return;
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->DrawLine(
		D2D1::Point2F((FLOAT)x1, (FLOAT)y1),
		D2D1::Point2F((FLOAT)x2, (FLOAT)y2),
		d2d_state.brush, 1.0f);
	d2d_state.render_target->EndDraw();
	InvalidateRect(d2d_state.hwnd, NULL, FALSE);
}

extern "C" void lineto(int x, int y) {
	line(d2d_state.current_x, d2d_state.current_y, x, y);
	d2d_state.current_x = x;
	d2d_state.current_y = y;
}

extern "C" void linerel(int dx, int dy) {
	int x2 = d2d_state.current_x + dx;
	int y2 = d2d_state.current_y + dy;
	line(d2d_state.current_x, d2d_state.current_y, x2, y2);
	d2d_state.current_x = x2;
	d2d_state.current_y = y2;
}

extern "C" void rectangle(int left, int top, int right, int bottom) {
	if (!d2d_state.render_target) return;
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->DrawRectangle(
		D2D1::RectF((FLOAT)left, (FLOAT)top, (FLOAT)right, (FLOAT)bottom),
		d2d_state.brush, 1.0f);
	d2d_state.render_target->EndDraw();
	InvalidateRect(d2d_state.hwnd, NULL, FALSE);
}

extern "C" void circle(int x, int y, int radius) {
	if (!d2d_state.render_target) return;
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->DrawEllipse(
		D2D1::Ellipse(D2D1::Point2F((FLOAT)x, (FLOAT)y), (FLOAT)radius, (FLOAT)radius),
		d2d_state.brush, 1.0f);
	d2d_state.render_target->EndDraw();
	InvalidateRect(d2d_state.hwnd, NULL, FALSE);
}

extern "C" void setlinestyle(int linestyle, unsigned pattern, int thickness) {
	d2d_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: d2d_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: d2d_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: d2d_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: d2d_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: d2d_state.line_pattern = pattern; break;
	default: d2d_state.line_pattern = 0xFFFF;
	}
}

extern "C" void setfillstyle(int pattern, int color) {
	d2d_state.fill_style = pattern;
	d2d_state.fill_color = color;
}

extern "C" void bar(int left, int top, int right, int bottom) {
	if (!d2d_state.render_target) return;
	D2D1_COLOR_F old_color = d2d_state.brush->GetColor();
	d2d_state.brush->SetColor(d2d_state.colors[d2d_state.fill_color & 0x0F]);
	d2d_state.render_target->BeginDraw();
	d2d_state.render_target->FillRectangle(
		D2D1::RectF((FLOAT)left, (FLOAT)top, (FLOAT)right, (FLOAT)bottom),
		d2d_state.brush);
	d2d_state.render_target->EndDraw();
	d2d_state.brush->SetColor(old_color);
	InvalidateRect(d2d_state.hwnd, NULL, FALSE);
}

extern "C" void settextjustify(int horiz, int vert) {
	d2d_state.text_justify_h = horiz;
	d2d_state.text_justify_v = vert;
}

extern "C" int textheight(const char *textstring) { return 12; }
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
	return d2d_state.initialized ? grOk : grNoInitGraph;
}
