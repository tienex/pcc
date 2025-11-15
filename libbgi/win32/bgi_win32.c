/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Windows using GDI
 *
 * Provides BGI graphics on Windows
 * Works on: Windows 95/98/ME/NT/2000/XP/Vista/7/8/10/11
 * Requires: Windows API (GDI)
 */

#include "../graphics.h"
#include <windows.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Internal state */
static struct {
	HWND hwnd;
	HDC hdc;
	HDC memdc;			/* Memory DC for double buffering */
	HBITMAP bitmap;
	HBITMAP old_bitmap;
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
	COLORREF colors[16];	/* EGA palette */
	HPEN pens[16];
	HBRUSH brushes[16];
	int initialized;
} win32_state;

/* EGA color palette */
static const COLORREF ega_colors[16] = {
	RGB(0x00, 0x00, 0x00),	/* BLACK */
	RGB(0x00, 0x00, 0xAA),	/* BLUE */
	RGB(0x00, 0xAA, 0x00),	/* GREEN */
	RGB(0x00, 0xAA, 0xAA),	/* CYAN */
	RGB(0xAA, 0x00, 0x00),	/* RED */
	RGB(0xAA, 0x00, 0xAA),	/* MAGENTA */
	RGB(0xAA, 0x55, 0x00),	/* BROWN */
	RGB(0xAA, 0xAA, 0xAA),	/* LIGHTGRAY */
	RGB(0x55, 0x55, 0x55),	/* DARKGRAY */
	RGB(0x55, 0x55, 0xFF),	/* LIGHTBLUE */
	RGB(0x55, 0xFF, 0x55),	/* LIGHTGREEN */
	RGB(0x55, 0xFF, 0xFF),	/* LIGHTCYAN */
	RGB(0xFF, 0x55, 0x55),	/* LIGHTRED */
	RGB(0xFF, 0x55, 0xFF),	/* LIGHTMAGENTA */
	RGB(0xFF, 0xFF, 0x55),	/* YELLOW */
	RGB(0xFF, 0xFF, 0xFF)	/* WHITE */
};

/* Window procedure */
static LRESULT CALLBACK
WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg) {
	case WM_PAINT:
		{
			PAINTSTRUCT ps;
			HDC hdc = BeginPaint(hwnd, &ps);
			/* Copy from memory DC to screen */
			BitBlt(hdc, 0, 0, win32_state.width, win32_state.height,
			       win32_state.memdc, 0, 0, SRCCOPY);
			EndPaint(hwnd, &ps);
		}
		return 0;

	case WM_CLOSE:
		closegraph();
		PostQuitMessage(0);
		return 0;

	default:
		return DefWindowProc(hwnd, msg, wParam, lParam);
	}
}

void
initgraph(int *driver, int *mode, const char *path)
{
	WNDCLASS wc = {0};
	int width = 640;
	int height = 480;
	int i;
	RECT rect;

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

	/* Register window class */
	wc.lpfnWndProc = WndProc;
	wc.hInstance = GetModuleHandle(NULL);
	wc.lpszClassName = "BGIWindow";
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);

	if (!RegisterClass(&wc)) {
		/* Might already be registered */
	}

	/* Calculate window size with borders */
	rect.left = 0;
	rect.top = 0;
	rect.right = width;
	rect.bottom = height;
	AdjustWindowRect(&rect, WS_OVERLAPPEDWINDOW, FALSE);

	/* Create window */
	win32_state.hwnd = CreateWindow("BGIWindow", "BGI Graphics",
	                                 WS_OVERLAPPEDWINDOW,
	                                 CW_USEDEFAULT, CW_USEDEFAULT,
	                                 rect.right - rect.left, rect.bottom - rect.top,
	                                 NULL, NULL, GetModuleHandle(NULL), NULL);

	if (!win32_state.hwnd) {
		*driver = grNotDetected;
		return;
	}

	/* Get device context */
	win32_state.hdc = GetDC(win32_state.hwnd);
	if (!win32_state.hdc) {
		DestroyWindow(win32_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	/* Create memory DC for double buffering */
	win32_state.memdc = CreateCompatibleDC(win32_state.hdc);
	if (!win32_state.memdc) {
		ReleaseDC(win32_state.hwnd, win32_state.hdc);
		DestroyWindow(win32_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	/* Create bitmap */
	win32_state.bitmap = CreateCompatibleBitmap(win32_state.hdc, width, height);
	if (!win32_state.bitmap) {
		DeleteDC(win32_state.memdc);
		ReleaseDC(win32_state.hwnd, win32_state.hdc);
		DestroyWindow(win32_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	win32_state.old_bitmap = SelectObject(win32_state.memdc, win32_state.bitmap);

	/* Create pens and brushes for each color */
	for (i = 0; i < 16; i++) {
		win32_state.colors[i] = ega_colors[i];
		win32_state.pens[i] = CreatePen(PS_SOLID, 1, ega_colors[i]);
		win32_state.brushes[i] = CreateSolidBrush(ega_colors[i]);
	}

	win32_state.width = width;
	win32_state.height = height;
	win32_state.current_color = WHITE;
	win32_state.current_bkcolor = BLACK;
	win32_state.current_x = 0;
	win32_state.current_y = 0;
	win32_state.line_style = SOLID_LINE;
	win32_state.line_pattern = 0xFFFF;
	win32_state.fill_style = SOLID_FILL;
	win32_state.fill_color = WHITE;
	win32_state.text_justify_h = LEFT_TEXT;
	win32_state.text_justify_v = TOP_TEXT;
	win32_state.initialized = 1;

	/* Show window */
	ShowWindow(win32_state.hwnd, SW_SHOW);
	UpdateWindow(win32_state.hwnd);

	/* Clear to background color */
	cleardevice();

	*driver = 0;
	*mode = 0;
}

void
closegraph(void)
{
	int i;

	if (!win32_state.initialized)
		return;

	/* Delete pens and brushes */
	for (i = 0; i < 16; i++) {
		DeleteObject(win32_state.pens[i]);
		DeleteObject(win32_state.brushes[i]);
	}

	/* Clean up GDI objects */
	if (win32_state.memdc) {
		SelectObject(win32_state.memdc, win32_state.old_bitmap);
		DeleteDC(win32_state.memdc);
	}

	if (win32_state.bitmap)
		DeleteObject(win32_state.bitmap);

	if (win32_state.hdc && win32_state.hwnd)
		ReleaseDC(win32_state.hwnd, win32_state.hdc);

	if (win32_state.hwnd)
		DestroyWindow(win32_state.hwnd);

	memset(&win32_state, 0, sizeof(win32_state));
}

int
getmaxx(void)
{
	return win32_state.width - 1;
}

int
getmaxy(void)
{
	return win32_state.height - 1;
}

void
setcolor(int color)
{
	win32_state.current_color = color & 0x0F;
	SelectObject(win32_state.memdc, win32_state.pens[win32_state.current_color]);
	SetTextColor(win32_state.memdc, win32_state.colors[win32_state.current_color]);
}

int
getcolor(void)
{
	return win32_state.current_color;
}

void
setbkcolor(int color)
{
	win32_state.current_bkcolor = color & 0x0F;
	SetBkColor(win32_state.memdc, win32_state.colors[win32_state.current_bkcolor]);
}

int
getbkcolor(void)
{
	return win32_state.current_bkcolor;
}

static void
update_display(void)
{
	/* Copy from memory DC to screen */
	BitBlt(win32_state.hdc, 0, 0, win32_state.width, win32_state.height,
	       win32_state.memdc, 0, 0, SRCCOPY);

	/* Process pending messages */
	MSG msg;
	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
}

void
cleardevice(void)
{
	RECT rect;
	HBRUSH brush;

	rect.left = 0;
	rect.top = 0;
	rect.right = win32_state.width;
	rect.bottom = win32_state.height;

	brush = win32_state.brushes[win32_state.current_bkcolor];
	FillRect(win32_state.memdc, &rect, brush);
	update_display();
}

void
putpixel(int x, int y, int color)
{
	if (x >= 0 && x < win32_state.width && y >= 0 && y < win32_state.height) {
		SetPixel(win32_state.memdc, x, y, win32_state.colors[color & 0x0F]);
	}
}

unsigned int
getpixel(int x, int y)
{
	COLORREF pixel;
	int i;

	if (x < 0 || x >= win32_state.width || y < 0 || y >= win32_state.height)
		return 0;

	pixel = GetPixel(win32_state.memdc, x, y);

	/* Find matching EGA color */
	for (i = 0; i < 16; i++) {
		if (win32_state.colors[i] == pixel)
			return i;
	}

	return 0;
}

void
moveto(int x, int y)
{
	win32_state.current_x = x;
	win32_state.current_y = y;
	MoveToEx(win32_state.memdc, x, y, NULL);
}

void
moverel(int dx, int dy)
{
	win32_state.current_x += dx;
	win32_state.current_y += dy;
	MoveToEx(win32_state.memdc, win32_state.current_x, win32_state.current_y, NULL);
}

int
getx(void)
{
	return win32_state.current_x;
}

int
gety(void)
{
	return win32_state.current_y;
}

void
line(int x1, int y1, int x2, int y2)
{
	if (win32_state.line_style == SOLID_LINE) {
		MoveToEx(win32_state.memdc, x1, y1, NULL);
		LineTo(win32_state.memdc, x2, y2);
	} else {
		/* Bresenham's line algorithm for styled lines */
		int dx = abs(x2 - x1);
		int dy = abs(y2 - y1);
		int sx = (x1 < x2) ? 1 : -1;
		int sy = (y1 < y2) ? 1 : -1;
		int err = dx - dy;
		int e2;
		int pattern_pos = 0;

		while (1) {
			if (win32_state.line_pattern & (1 << (pattern_pos & 15)))
				putpixel(x1, y1, win32_state.current_color);

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
	line(win32_state.current_x, win32_state.current_y, x, y);
	win32_state.current_x = x;
	win32_state.current_y = y;
}

void
linerel(int dx, int dy)
{
	int x2 = win32_state.current_x + dx;
	int y2 = win32_state.current_y + dy;
	line(win32_state.current_x, win32_state.current_y, x2, y2);
	win32_state.current_x = x2;
	win32_state.current_y = y2;
}

void
rectangle(int left, int top, int right, int bottom)
{
	HBRUSH old_brush;

	/* Use null brush for outline only */
	old_brush = SelectObject(win32_state.memdc, GetStockObject(NULL_BRUSH));
	Rectangle(win32_state.memdc, left, top, right + 1, bottom + 1);
	SelectObject(win32_state.memdc, old_brush);
	update_display();
}

void
circle(int x, int y, int radius)
{
	HBRUSH old_brush;

	/* Use null brush for outline only */
	old_brush = SelectObject(win32_state.memdc, GetStockObject(NULL_BRUSH));
	Ellipse(win32_state.memdc, x - radius, y - radius, x + radius, y + radius);
	SelectObject(win32_state.memdc, old_brush);
	update_display();
}

void
setlinestyle(int linestyle, unsigned pattern, int thickness)
{
	HPEN pen;
	int pen_style;

	win32_state.line_style = linestyle;

	switch (linestyle) {
	case SOLID_LINE:
		win32_state.line_pattern = 0xFFFF;
		pen_style = PS_SOLID;
		break;
	case DOTTED_LINE:
		win32_state.line_pattern = 0xCCCC;
		pen_style = PS_DOT;
		break;
	case CENTER_LINE:
		win32_state.line_pattern = 0xF8F8;
		pen_style = PS_DASHDOT;
		break;
	case DASHED_LINE:
		win32_state.line_pattern = 0xF0F0;
		pen_style = PS_DASH;
		break;
	case USERBIT_LINE:
		win32_state.line_pattern = pattern;
		pen_style = PS_DASH;	/* Approximate */
		break;
	default:
		win32_state.line_pattern = 0xFFFF;
		pen_style = PS_SOLID;
	}

	/* Create new pen with style and thickness */
	pen = CreatePen(pen_style, thickness > 0 ? thickness : 1,
	                win32_state.colors[win32_state.current_color]);
	if (pen) {
		HPEN old_pen = SelectObject(win32_state.memdc, pen);
		if (old_pen != win32_state.pens[win32_state.current_color])
			DeleteObject(old_pen);
	}
}

void
setfillstyle(int pattern, int color)
{
	win32_state.fill_style = pattern;
	win32_state.fill_color = color;
}

void
bar(int left, int top, int right, int bottom)
{
	RECT rect;

	rect.left = left;
	rect.top = top;
	rect.right = right + 1;
	rect.bottom = bottom + 1;

	FillRect(win32_state.memdc, &rect, win32_state.brushes[win32_state.fill_color & 0x0F]);
	update_display();
}

void
settextjustify(int horiz, int vert)
{
	win32_state.text_justify_h = horiz;
	win32_state.text_justify_v = vert;
}

int
textheight(const char *textstring)
{
	TEXTMETRIC tm;
	GetTextMetrics(win32_state.memdc, &tm);
	return tm.tmHeight;
}

int
textwidth(const char *textstring)
{
	SIZE size;
	GetTextExtentPoint32(win32_state.memdc, textstring, strlen(textstring), &size);
	return size.cx;
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
	return win32_state.initialized ? grOk : grNoInitGraph;
}
