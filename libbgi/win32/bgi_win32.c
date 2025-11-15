/*
 * Copyright (c) 2025 PCC Project
 * Borland Graphics Interface (BGI) - Windows GDI Implementation
 */

#ifdef _WIN32

#include "../bgi.h"
#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

static struct {
	int initialized;
	int width;
	int height;
	int color;
	int fillcolor;
	int linestyle;
	int fillstyle;
	int textjustify_h;
	int textjustify_v;
	HWND hwnd;
	HDC hdc;
	HDC memdc;
	HBITMAP hbitmap;
	HBITMAP old_bitmap;
	int x, y;
} bgi_state = {0};

static const COLORREF bgi_colors[] = {
	RGB(0,0,0),       /* BLACK */
	RGB(0,0,170),     /* BLUE */
	RGB(0,170,0),     /* GREEN */
	RGB(0,170,170),   /* CYAN */
	RGB(170,0,0),     /* RED */
	RGB(170,0,170),   /* MAGENTA */
	RGB(170,85,0),    /* BROWN */
	RGB(170,170,170), /* LIGHTGRAY */
	RGB(85,85,85),    /* DARKGRAY */
	RGB(85,85,255),   /* LIGHTBLUE */
	RGB(85,255,85),   /* LIGHTGREEN */
	RGB(85,255,255),  /* LIGHTCYAN */
	RGB(255,85,85),   /* LIGHTRED */
	RGB(255,85,255),  /* LIGHTMAGENTA */
	RGB(255,255,85),  /* YELLOW */
	RGB(255,255,255)  /* WHITE */
};

static LRESULT CALLBACK BGIWindowProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch (msg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		HDC hdc = BeginPaint(hwnd, &ps);
		BitBlt(hdc, 0, 0, bgi_state.width, bgi_state.height,
		       bgi_state.memdc, 0, 0, SRCCOPY);
		EndPaint(hwnd, &ps);
		return 0;
	}
	case WM_CLOSE:
		DestroyWindow(hwnd);
		return 0;
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	}
	return DefWindowProc(hwnd, msg, wParam, lParam);
}

void initgraph(int *driver, int *mode, const char *path) {
	(void)driver; (void)mode; (void)path;

	bgi_state.width = 640;
	bgi_state.height = 480;

	/* Register window class */
	WNDCLASSA wc = {0};
	wc.lpfnWndProc = BGIWindowProc;
	wc.hInstance = GetModuleHandle(NULL);
	wc.lpszClassName = "BGIWindowClass";
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	RegisterClassA(&wc);

	/* Create window */
	bgi_state.hwnd = CreateWindowA(
		"BGIWindowClass", "BGI Graphics",
		WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		CW_USEDEFAULT, CW_USEDEFAULT, bgi_state.width, bgi_state.height,
		NULL, NULL, wc.hInstance, NULL
	);

	/* Create memory DC for double buffering */
	bgi_state.hdc = GetDC(bgi_state.hwnd);
	bgi_state.memdc = CreateCompatibleDC(bgi_state.hdc);
	bgi_state.hbitmap = CreateCompatibleBitmap(bgi_state.hdc, bgi_state.width, bgi_state.height);
	bgi_state.old_bitmap = (HBITMAP)SelectObject(bgi_state.memdc, bgi_state.hbitmap);

	bgi_state.color = WHITE;
	bgi_state.fillcolor = WHITE;
	bgi_state.x = bgi_state.y = 0;
	bgi_state.initialized = 1;

	/* Clear to black */
	RECT rect = {0, 0, bgi_state.width, bgi_state.height};
	FillRect(bgi_state.memdc, &rect, (HBRUSH)GetStockObject(BLACK_BRUSH));
}

void closegraph(void) {
	if (bgi_state.initialized) {
		SelectObject(bgi_state.memdc, bgi_state.old_bitmap);
		DeleteObject(bgi_state.hbitmap);
		DeleteDC(bgi_state.memdc);
		ReleaseDC(bgi_state.hwnd, bgi_state.hdc);
		DestroyWindow(bgi_state.hwnd);
		bgi_state.initialized = 0;
	}
}

void cleardevice(void) {
	if (!bgi_state.initialized) return;
	RECT rect = {0, 0, bgi_state.width, bgi_state.height};
	FillRect(bgi_state.memdc, &rect, (HBRUSH)GetStockObject(BLACK_BRUSH));
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void setcolor(int color) {
	bgi_state.color = color;
}

void setfillstyle(int pattern, int color) {
	bgi_state.fillstyle = pattern;
	bgi_state.fillcolor = color;
}

void putpixel(int x, int y, int color) {
	if (!bgi_state.initialized) return;
	SetPixel(bgi_state.memdc, x, y, bgi_colors[color & 15]);
}

int getpixel(int x, int y) {
	if (!bgi_state.initialized) return 0;
	COLORREF c = GetPixel(bgi_state.memdc, x, y);
	for (int i = 0; i < 16; i++) {
		if (bgi_colors[i] == c) return i;
	}
	return 0;
}

void line(int x1, int y1, int x2, int y2) {
	if (!bgi_state.initialized) return;

	HPEN pen = CreatePen(PS_SOLID, 1, bgi_colors[bgi_state.color & 15]);
	HPEN old_pen = (HPEN)SelectObject(bgi_state.memdc, pen);

	MoveToEx(bgi_state.memdc, x1, y1, NULL);
	LineTo(bgi_state.memdc, x2, y2);

	SelectObject(bgi_state.memdc, old_pen);
	DeleteObject(pen);

	bgi_state.x = x2;
	bgi_state.y = y2;
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void lineto(int x, int y) {
	line(bgi_state.x, bgi_state.y, x, y);
}

void moveto(int x, int y) {
	bgi_state.x = x;
	bgi_state.y = y;
}

void circle(int x, int y, int radius) {
	if (!bgi_state.initialized) return;

	HPEN pen = CreatePen(PS_SOLID, 1, bgi_colors[bgi_state.color & 15]);
	HPEN old_pen = (HPEN)SelectObject(bgi_state.memdc, pen);
	HBRUSH old_brush = (HBRUSH)SelectObject(bgi_state.memdc, GetStockObject(NULL_BRUSH));

	Ellipse(bgi_state.memdc, x - radius, y - radius, x + radius, y + radius);

	SelectObject(bgi_state.memdc, old_brush);
	SelectObject(bgi_state.memdc, old_pen);
	DeleteObject(pen);
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void rectangle(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized) return;

	HPEN pen = CreatePen(PS_SOLID, 1, bgi_colors[bgi_state.color & 15]);
	HPEN old_pen = (HPEN)SelectObject(bgi_state.memdc, pen);
	HBRUSH old_brush = (HBRUSH)SelectObject(bgi_state.memdc, GetStockObject(NULL_BRUSH));

	Rectangle(bgi_state.memdc, left, top, right, bottom);

	SelectObject(bgi_state.memdc, old_brush);
	SelectObject(bgi_state.memdc, old_pen);
	DeleteObject(pen);
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void bar(int left, int top, int right, int bottom) {
	if (!bgi_state.initialized) return;

	HBRUSH brush = CreateSolidBrush(bgi_colors[bgi_state.fillcolor & 15]);
	RECT rect = {left, top, right, bottom};
	FillRect(bgi_state.memdc, &rect, brush);
	DeleteObject(brush);
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void outtextxy(int x, int y, const char *text) {
	if (!bgi_state.initialized) return;

	SetTextColor(bgi_state.memdc, bgi_colors[bgi_state.color & 15]);
	SetBkMode(bgi_state.memdc, TRANSPARENT);
	TextOutA(bgi_state.memdc, x, y, text, strlen(text));
	InvalidateRect(bgi_state.hwnd, NULL, FALSE);
}

void outtext(const char *text) {
	outtextxy(bgi_state.x, bgi_state.y, text);
}

void setbkcolor(int color) {
	SetBkColor(bgi_state.memdc, bgi_colors[color & 15]);
}

int getbkcolor(void) {
	COLORREF c = GetBkColor(bgi_state.memdc);
	for (int i = 0; i < 16; i++) {
		if (bgi_colors[i] == c) return i;
	}
	return 0;
}

int getcolor(void) {
	return bgi_state.color;
}

int getmaxx(void) {
	return bgi_state.width - 1;
}

int getmaxy(void) {
	return bgi_state.height - 1;
}

int getx(void) {
	return bgi_state.x;
}

int gety(void) {
	return bgi_state.y;
}

#endif /* _WIN32 */
