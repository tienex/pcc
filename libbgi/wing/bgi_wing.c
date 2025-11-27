/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation for Windows 3.x WinG (16-bit)
 *
 * Hardware-accelerated 16-bit Windows graphics using WinG
 * Works on: Windows 3.1, 3.11 with WinG library
 * Requires: WING.DLL (WinG runtime)
 * Features: Direct framebuffer access, hardware acceleration
 */

#include "../graphics.h"
#include <windows.h>
#include <wing.h>
#include <stdlib.h>
#include <string.h>

static struct {
	HWND hwnd;
	HDC hdc;
	HDC wing_dc;
	HBITMAP wing_bitmap;
	void FAR *bits;
	int width, height;
	int current_color, current_bkcolor, current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color, text_justify_h, text_justify_v;
	BYTE FAR *palette;
	int initialized;
} wing_state;

static const RGBQUAD ega_palette[16] = {
	{0, 0, 0, 0}, {170, 0, 0, 0}, {0, 170, 0, 0}, {170, 170, 0, 0},
	{0, 0, 170, 0}, {170, 0, 170, 0}, {0, 85, 170, 0}, {170, 170, 170, 0},
	{85, 85, 85, 0}, {255, 85, 85, 0}, {85, 255, 85, 0}, {255, 255, 85, 0},
	{85, 85, 255, 0}, {255, 85, 255, 0}, {85, 255, 255, 0}, {255, 255, 255, 0}
};

static LRESULT FAR PASCAL WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	switch (msg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		HDC hdc = BeginPaint(hwnd, &ps);
		/* WinG recommends using WinGBitBlt for best performance */
		WinGBitBlt(hdc, 0, 0, wing_state.width, wing_state.height,
		           wing_state.wing_dc, 0, 0);
		EndPaint(hwnd, &ps);
		return 0;
	}
	case WM_QUERYNEWPALETTE:
		if (wing_state.palette) {
			WinGSetDIBColorTable(wing_state.wing_dc, 0, 16, (RGBQUAD FAR *)wing_state.palette);
			InvalidateRect(hwnd, NULL, FALSE);
			return TRUE;
		}
		return FALSE;
	case WM_PALETTECHANGED:
		if ((HWND)wParam != hwnd && wing_state.palette) {
			WinGSetDIBColorTable(wing_state.wing_dc, 0, 16, (RGBQUAD FAR *)wing_state.palette);
			InvalidateRect(hwnd, NULL, FALSE);
		}
		return 0;
	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	default:
		return DefWindowProc(hwnd, msg, wParam, lParam);
	}
}

void FAR PASCAL initgraph(int FAR *driver, int FAR *mode, const char FAR *path) {
	WNDCLASS wc;
	BITMAPINFO FAR *bmi;
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
	wc.lpszClassName = "BGI_WinG_Class";
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	RegisterClass(&wc);

	/* Create window */
	wing_state.hwnd = CreateWindow("BGI_WinG_Class", "BGI Graphics (WinG)",
	                                WS_OVERLAPPEDWINDOW | WS_VISIBLE,
	                                CW_USEDEFAULT, CW_USEDEFAULT, width, height,
	                                NULL, NULL, wc.hInstance, NULL);
	if (!wing_state.hwnd) { *driver = grNotDetected; return; }

	wing_state.hdc = GetDC(wing_state.hwnd);
	if (!wing_state.hdc) {
		DestroyWindow(wing_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	/* Initialize WinG */
	wing_state.wing_dc = WinGCreateDC();
	if (!wing_state.wing_dc) {
		ReleaseDC(wing_state.hwnd, wing_state.hdc);
		DestroyWindow(wing_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	/* Create WinG DIB with 8-bit palette */
	bmi = (BITMAPINFO FAR *)_fmalloc(sizeof(BITMAPINFOHEADER) + sizeof(RGBQUAD) * 256);
	if (!bmi) {
		WinGDeleteDC(wing_state.wing_dc);
		ReleaseDC(wing_state.hwnd, wing_state.hdc);
		DestroyWindow(wing_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	bmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	bmi->bmiHeader.biWidth = width;
	bmi->bmiHeader.biHeight = -height;  /* Top-down DIB */
	bmi->bmiHeader.biPlanes = 1;
	bmi->bmiHeader.biBitCount = 8;
	bmi->bmiHeader.biCompression = BI_RGB;
	bmi->bmiHeader.biSizeImage = 0;
	bmi->bmiHeader.biXPelsPerMeter = 0;
	bmi->bmiHeader.biYPelsPerMeter = 0;
	bmi->bmiHeader.biClrUsed = 16;
	bmi->bmiHeader.biClrImportant = 16;

	/* Set EGA palette */
	for (i = 0; i < 16; i++)
		bmi->bmiColors[i] = ega_palette[i];

	wing_state.wing_bitmap = WinGCreateBitmap(wing_state.wing_dc, bmi, &wing_state.bits);
	_ffree(bmi);

	if (!wing_state.wing_bitmap) {
		WinGDeleteDC(wing_state.wing_dc);
		ReleaseDC(wing_state.hwnd, wing_state.hdc);
		DestroyWindow(wing_state.hwnd);
		*driver = grNotDetected;
		return;
	}

	SelectObject(wing_state.wing_dc, wing_state.wing_bitmap);

	/* Store palette for later use */
	wing_state.palette = (BYTE FAR *)_fmalloc(sizeof(RGBQUAD) * 16);
	if (wing_state.palette)
		_fmemcpy(wing_state.palette, ega_palette, sizeof(RGBQUAD) * 16);

	/* Recommend DIB format for best performance */
	WinGRecommendDIBFormat((BITMAPINFO FAR *)wing_state.palette);

	wing_state.width = width;
	wing_state.height = height;
	wing_state.current_color = WHITE;
	wing_state.current_bkcolor = BLACK;
	wing_state.current_x = 0;
	wing_state.current_y = 0;
	wing_state.line_style = SOLID_LINE;
	wing_state.line_pattern = 0xFFFF;
	wing_state.fill_style = SOLID_FILL;
	wing_state.fill_color = WHITE;
	wing_state.text_justify_h = LEFT_TEXT;
	wing_state.text_justify_v = TOP_TEXT;
	wing_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void FAR PASCAL closegraph(void) {
	if (!wing_state.initialized) return;

	if (wing_state.palette)
		_ffree(wing_state.palette);
	if (wing_state.wing_bitmap)
		DeleteObject(wing_state.wing_bitmap);
	if (wing_state.wing_dc)
		WinGDeleteDC(wing_state.wing_dc);
	if (wing_state.hdc)
		ReleaseDC(wing_state.hwnd, wing_state.hdc);
	if (wing_state.hwnd)
		DestroyWindow(wing_state.hwnd);

	_fmemset(&wing_state, 0, sizeof(wing_state));
}

int FAR PASCAL getmaxx(void) { return wing_state.width - 1; }
int FAR PASCAL getmaxy(void) { return wing_state.height - 1; }
void FAR PASCAL setcolor(int color) { wing_state.current_color = color & 0x0F; }
int FAR PASCAL getcolor(void) { return wing_state.current_color; }
void FAR PASCAL setbkcolor(int color) { wing_state.current_bkcolor = color & 0x0F; }
int FAR PASCAL getbkcolor(void) { return wing_state.current_bkcolor; }

void FAR PASCAL cleardevice(void) {
	BYTE FAR *fb = (BYTE FAR *)wing_state.bits;
	long size = (long)wing_state.width * wing_state.height;
	long i;

	/* Direct framebuffer access for maximum speed */
	for (i = 0; i < size; i++)
		fb[i] = wing_state.current_bkcolor;

	InvalidateRect(wing_state.hwnd, NULL, FALSE);
}

void FAR PASCAL putpixel(int x, int y, int color) {
	BYTE FAR *fb;
	if (x < 0 || x >= wing_state.width || y < 0 || y >= wing_state.height)
		return;

	fb = (BYTE FAR *)wing_state.bits;
	fb[y * wing_state.width + x] = color & 0x0F;
}

unsigned int FAR PASCAL getpixel(int x, int y) {
	BYTE FAR *fb;
	if (x < 0 || x >= wing_state.width || y < 0 || y >= wing_state.height)
		return 0;

	fb = (BYTE FAR *)wing_state.bits;
	return fb[y * wing_state.width + x];
}

void FAR PASCAL moveto(int x, int y) { wing_state.current_x = x; wing_state.current_y = y; }
void FAR PASCAL moverel(int dx, int dy) { wing_state.current_x += dx; wing_state.current_y += dy; }
int FAR PASCAL getx(void) { return wing_state.current_x; }
int FAR PASCAL gety(void) { return wing_state.current_y; }

void FAR PASCAL line(int x1, int y1, int x2, int y2) {
	int dx = abs(x2 - x1), dy = abs(y2 - y1);
	int sx = (x1 < x2) ? 1 : -1, sy = (y1 < y2) ? 1 : -1;
	int err = dx - dy, e2, pattern_pos = 0;

	/* Bresenham's line algorithm with direct framebuffer access */
	while (1) {
		if (wing_state.line_pattern & (1 << (pattern_pos & 15)))
			putpixel(x1, y1, wing_state.current_color);
		if (x1 == x2 && y1 == y2) break;
		e2 = 2 * err;
		if (e2 > -dy) { err -= dy; x1 += sx; }
		if (e2 < dx) { err += dx; y1 += sy; }
		pattern_pos++;
	}
	InvalidateRect(wing_state.hwnd, NULL, FALSE);
}

void FAR PASCAL lineto(int x, int y) {
	line(wing_state.current_x, wing_state.current_y, x, y);
	wing_state.current_x = x; wing_state.current_y = y;
}

void FAR PASCAL linerel(int dx, int dy) {
	int x2 = wing_state.current_x + dx, y2 = wing_state.current_y + dy;
	line(wing_state.current_x, wing_state.current_y, x2, y2);
	wing_state.current_x = x2; wing_state.current_y = y2;
}

void FAR PASCAL rectangle(int left, int top, int right, int bottom) {
	line(left, top, right, top);
	line(right, top, right, bottom);
	line(right, bottom, left, bottom);
	line(left, bottom, left, top);
}

void FAR PASCAL circle(int x, int y, int radius) {
	int dx = 0, dy = radius, d = 1 - radius;

	/* Bresenham's circle algorithm */
	while (dx <= dy) {
		putpixel(x + dx, y + dy, wing_state.current_color);
		putpixel(x - dx, y + dy, wing_state.current_color);
		putpixel(x + dx, y - dy, wing_state.current_color);
		putpixel(x - dx, y - dy, wing_state.current_color);
		putpixel(x + dy, y + dx, wing_state.current_color);
		putpixel(x - dy, y + dx, wing_state.current_color);
		putpixel(x + dy, y - dx, wing_state.current_color);
		putpixel(x - dy, y - dx, wing_state.current_color);

		if (d < 0)
			d += 2 * dx + 3;
		else {
			d += 2 * (dx - dy) + 5;
			dy--;
		}
		dx++;
	}
	InvalidateRect(wing_state.hwnd, NULL, FALSE);
}

void FAR PASCAL setlinestyle(int linestyle, unsigned pattern, int thickness) {
	wing_state.line_style = linestyle;
	switch (linestyle) {
	case SOLID_LINE: wing_state.line_pattern = 0xFFFF; break;
	case DOTTED_LINE: wing_state.line_pattern = 0xCCCC; break;
	case CENTER_LINE: wing_state.line_pattern = 0xF8F8; break;
	case DASHED_LINE: wing_state.line_pattern = 0xF0F0; break;
	case USERBIT_LINE: wing_state.line_pattern = pattern; break;
	default: wing_state.line_pattern = 0xFFFF;
	}
}

void FAR PASCAL setfillstyle(int pattern, int color) {
	wing_state.fill_style = pattern;
	wing_state.fill_color = color;
}

void FAR PASCAL bar(int left, int top, int right, int bottom) {
	int x, y;
	BYTE FAR *fb = (BYTE FAR *)wing_state.bits;
	BYTE color = wing_state.fill_color & 0x0F;

	/* Direct framebuffer fill for maximum speed */
	for (y = top; y <= bottom; y++) {
		if (y >= 0 && y < wing_state.height) {
			for (x = left; x <= right; x++) {
				if (x >= 0 && x < wing_state.width)
					fb[y * wing_state.width + x] = color;
			}
		}
	}
	InvalidateRect(wing_state.hwnd, NULL, FALSE);
}

void FAR PASCAL settextjustify(int horiz, int vert) {
	wing_state.text_justify_h = horiz;
	wing_state.text_justify_v = vert;
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
	return wing_state.initialized ? grOk : grNoInitGraph;
}
