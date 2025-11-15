/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Windows 3.x (16-bit GDI)
 *
 * Pure 16-bit Windows graphics using GDI
 * Works on: Windows 3.0, 3.1, 3.11 (16-bit)
 * Features: GDI double buffering, standard Windows graphics
 */

#include "../graphics.h"
#include <windows.h>
#include <stdlib.h>
#include <string.h>

static struct {
	HWND hwnd;
	HDC hdc;
	HDC mem_dc;
	HBITMAP mem_bitmap;
	HBITMAP old_bitmap;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	COLORREF colors[16];
	int initialized;
} win16_state;

static const COLORREF ega_colors[16] = {
	RGB(0, 0, 0), RGB(0, 0, 170), RGB(0, 170, 0), RGB(0, 170, 170),
	RGB(170, 0, 0), RGB(170, 0, 170), RGB(170, 85, 0), RGB(170, 170, 170),
	RGB(85, 85, 85), RGB(85, 85, 255), RGB(85, 255, 85), RGB(85, 255, 255),
	RGB(255, 85, 85), RGB(255, 85, 255), RGB(255, 255, 85), RGB(255, 255, 255)
};

static LRESULT FAR PASCAL WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch (msg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		HDC hdc = BeginPaint(hwnd, &ps);
		BitBlt(hdc, 0, 0, win16_state.width, win16_state.height,
		       win16_state.mem_dc, 0, 0, SRCCOPY);
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

void FAR PASCAL initgraph(int FAR *driver, int FAR *mode, const char FAR *path) {
	WNDCLASS wc;
	int width = 640, height = 480, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }

	switch (*mode) {
	case VGALO: width = 640; height = 200; break;
	case VGAMED: width = 640; height = 350; break;
	case VGAHI: width = 640; height = 480; break;
	default: width = 640; height = 480;
	}

	/* Register window class */
	_fmemset(&wc, 0, sizeof(wc));
	wc.lpfnWndProc = WndProc;
	wc.hInstance = (HINSTANCE)GetModuleHandle(NULL);
	wc.lpszClassName = "BGI_Win16_Class";
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	RegisterClass(&wc);

	/* Create window */
	win16_state.hwnd = CreateWindow("BGI_Win16_Class", "BGI Graphics (Win16 GDI)",
	                                 WS_OVERLAPPEDWINDOW | WS_VISIBLE,
	                                 CW_USEDEFAULT, CW_USEDEFAULT, width, height,
	                                 NULL, NULL, wc.hInstance, NULL);
	if (!win16_state.hwnd) { *driver = grNotDetected; return; }

	win16_state.hdc = GetDC(win16_state.hwnd);
	if (!win16_state.hdc) {
		DestroyWindow(win16_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	/* Create GDI double buffer */
	win16_state.mem_dc = CreateCompatibleDC(win16_state.hdc);
	if (!win16_state.mem_dc) {
		ReleaseDC(win16_state.hwnd, win16_state.hdc);
		DestroyWindow(win16_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	win16_state.mem_bitmap = CreateCompatibleBitmap(win16_state.hdc, width, height);
	if (!win16_state.mem_bitmap) {
		DeleteDC(win16_state.mem_dc);
		ReleaseDC(win16_state.hwnd, win16_state.hdc);
		DestroyWindow(win16_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	win16_state.old_bitmap = SelectObject(win16_state.mem_dc, win16_state.mem_bitmap);

	/* Copy EGA colors */
	for (i = 0; i < 16; i++)
		win16_state.colors[i] = ega_colors[i];

	win16_state.width = width;
	win16_state.height = height;
	win16_state.current_color = WHITE;
	win16_state.current_bkcolor = BLACK;
	win16_state.current_x = 0;
	win16_state.current_y = 0;
	win16_state.line_style = SOLID_LINE;
	win16_state.line_pattern = 0xFFFF;
	win16_state.fill_style = SOLID_FILL;
	win16_state.fill_color = WHITE;
	win16_state.text_justify_h = LEFT_TEXT;
	win16_state.text_justify_v = TOP_TEXT;
	win16_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void FAR PASCAL closegraph(void) {
	if (!win16_state.initialized) return;

	if (win16_state.old_bitmap)
		SelectObject(win16_state.mem_dc, win16_state.old_bitmap);
	if (win16_state.mem_bitmap)
		DeleteObject(win16_state.mem_bitmap);
	if (win16_state.mem_dc)
		DeleteDC(win16_state.mem_dc);
	if (win16_state.hdc)
		ReleaseDC(win16_state.hwnd, win16_state.hdc);
	if (win16_state.hwnd)
		DestroyWindow(win16_state.hwnd);

	_fmemset(&win16_state, 0, sizeof(win16_state));
}

int FAR PASCAL getmaxx(void) { return win16_state.width - 1; }
int FAR PASCAL getmaxy(void) { return win16_state.height - 1; }
void FAR PASCAL setcolor(int color) { win16_state.current_color = color & 0x0F; }
int FAR PASCAL getcolor(void) { return win16_state.current_color; }
void FAR PASCAL setbkcolor(int color) { win16_state.current_bkcolor = color & 0x0F; }
int FAR PASCAL getbkcolor(void) { return win16_state.current_bkcolor; }

void FAR PASCAL cleardevice(void) {
	HBRUSH brush = CreateSolidBrush(win16_state.colors[win16_state.current_bkcolor]);
	RECT rect = {0, 0, win16_state.width, win16_state.height};
	FillRect(win16_state.mem_dc, &rect, brush);
	DeleteObject(brush);
	InvalidateRect(win16_state.hwnd, NULL, FALSE);
}

void FAR PASCAL putpixel(int x, int y, int color) {
	if (x >= 0 && x < win16_state.width && y >= 0 && y < win16_state.height)
		SetPixel(win16_state.mem_dc, x, y, win16_state.colors[color & 0x0F]);
}

unsigned int FAR PASCAL getpixel(int x, int y) {
	COLORREF pixel;
	int i;
	if (x < 0 || x >= win16_state.width || y < 0 || y >= win16_state.height) return 0;

	pixel = GetPixel(win16_state.mem_dc, x, y);
	for (i = 0; i < 16; i++)
		if (win16_state.colors[i] == pixel)
			return i;
	return 0;
}

void FAR PASCAL moveto(int x, int y) { win16_state.current_x = x; win16_state.current_y = y; }
void FAR PASCAL moverel(int dx, int dy) { win16_state.current_x += dx; win16_state.current_y += dy; }
int FAR PASCAL getx(void) { return win16_state.current_x; }
int FAR PASCAL gety(void) { return win16_state.current_y; }

void FAR PASCAL line(int x1, int y1, int x2, int y2) {
	HPEN pen = CreatePen(PS_SOLID, 1, win16_state.colors[win16_state.current_color]);
	HPEN old_pen = SelectObject(win16_state.mem_dc, pen);
	MoveToEx(win16_state.mem_dc, x1, y1, NULL);
	LineTo(win16_state.mem_dc, x2, y2);
	SelectObject(win16_state.mem_dc, old_pen);
	DeleteObject(pen);
	InvalidateRect(win16_state.hwnd, NULL, FALSE);
}

void FAR PASCAL lineto(int x, int y) {
	line(win16_state.current_x, win16_state.current_y, x, y);
	win16_state.current_x = x; win16_state.current_y = y;
}

void FAR PASCAL linerel(int dx, int dy) {
	int x2 = win16_state.current_x + dx, y2 = win16_state.current_y + dy;
	line(win16_state.current_x, win16_state.current_y, x2, y2);
	win16_state.current_x = x2; win16_state.current_y = y2;
}

void FAR PASCAL rectangle(int left, int top, int right, int bottom) {
	HPEN pen = CreatePen(PS_SOLID, 1, win16_state.colors[win16_state.current_color]);
	HPEN old_pen = SelectObject(win16_state.mem_dc, pen);
	HBRUSH old_brush = SelectObject(win16_state.mem_dc, GetStockObject(NULL_BRUSH));
	Rectangle(win16_state.mem_dc, left, top, right, bottom);
	SelectObject(win16_state.mem_dc, old_brush);
	SelectObject(win16_state.mem_dc, old_pen);
	DeleteObject(pen);
	InvalidateRect(win16_state.hwnd, NULL, FALSE);
}

void FAR PASCAL circle(int x, int y, int radius) {
	HPEN pen = CreatePen(PS_SOLID, 1, win16_state.colors[win16_state.current_color]);
	HPEN old_pen = SelectObject(win16_state.mem_dc, pen);
	HBRUSH old_brush = SelectObject(win16_state.mem_dc, GetStockObject(NULL_BRUSH));
	Ellipse(win16_state.mem_dc, x - radius, y - radius, x + radius, y + radius);
	SelectObject(win16_state.mem_dc, old_brush);
	SelectObject(win16_state.mem_dc, old_pen);
	DeleteObject(pen);
	InvalidateRect(win16_state.hwnd, NULL, FALSE);
}

void FAR PASCAL setlinestyle(int linestyle, unsigned pattern, int thickness) {
	win16_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: win16_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: win16_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: win16_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: win16_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: win16_state.line_pattern = pattern; break;
	default: win16_state.line_pattern = 0xFFFF;
	}
}

void FAR PASCAL setfillstyle(int pattern, int color) {
	win16_state.fill_style = pattern; win16_state.fill_color = color;
}

void FAR PASCAL bar(int left, int top, int right, int bottom) {
	HBRUSH brush = CreateSolidBrush(win16_state.colors[win16_state.fill_color & 0x0F]);
	RECT rect = {left, top, right, bottom};
	FillRect(win16_state.mem_dc, &rect, brush);
	DeleteObject(brush);
	InvalidateRect(win16_state.hwnd, NULL, FALSE);
}

void FAR PASCAL settextjustify(int horiz, int vert) {
	win16_state.text_justify_h = horiz; win16_state.text_justify_v = vert;
}

int FAR PASCAL textheight(const char FAR *textstring) { return 12; }
int FAR PASCAL textwidth(const char FAR *textstring) { return _fstrlen(textstring) * 8; }

const char FAR * FAR PASCAL grapherrormsg(int errorcode) {
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

int FAR PASCAL graphresult(void) {
	return win16_state.initialized ? grOk : grNoInitGraph;
}
