/*
 * Copyright (c) 2025 PCC Project
 * Terminal Emulation - Windows Implementation
 * Uses Windows Console API with custom rendering window
 */

#ifdef _WIN32

#include "../term.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Platform-specific data */
typedef struct {
	HWND hwnd;
	HDC hdc;
	HDC memdc;
	HBITMAP bitmap;
	HFONT font;
	int char_width;
	int char_height;
	HANDLE input_thread;
	int running;
} term_win32_t;

/* Color table */
static const COLORREF win32_colors[] = {
	RGB(0,0,0), RGB(170,0,0), RGB(0,170,0), RGB(170,85,0),
	RGB(0,0,170), RGB(170,0,170), RGB(0,170,170), RGB(170,170,170),
	RGB(85,85,85), RGB(255,85,85), RGB(85,255,85), RGB(255,255,85),
	RGB(85,85,255), RGB(255,85,255), RGB(85,255,255), RGB(255,255,255)
};

extern term_t *term_create(const term_config_t *config);
extern void term_destroy(term_t *term);
extern void term_write(term_t *term, const char *data, size_t len);
extern term_cell_t *term_get_cell(term_t *term, int col, int row);
extern void term_get_size(term_t *term, int *cols, int *rows);
extern void term_get_cursor(term_t *term, int *col, int *row);

static LRESULT CALLBACK TermWindowProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam);

/* Create platform-specific window */
static int term_win32_create_window(term_t *term, term_win32_t *win32) {
	/* Register window class */
	WNDCLASSA wc = {0};
	wc.lpfnWndProc = TermWindowProc;
	wc.hInstance = GetModuleHandle(NULL);
	wc.lpszClassName = "TerminalWindowClass";
	wc.hbrBackground = (HBRUSH)GetStockObject(BLACK_BRUSH);
	wc.hCursor = LoadCursor(NULL, IDC_IBEAM);
	RegisterClassA(&wc);

	/* Calculate window size */
	int cols, rows;
	term_get_size(term, &cols, &rows);
	int width = cols * win32->char_width + 20;
	int height = rows * win32->char_height + 40;

	/* Create window */
	win32->hwnd = CreateWindowA(
		"TerminalWindowClass", "Terminal",
		WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		CW_USEDEFAULT, CW_USEDEFAULT, width, height,
		NULL, NULL, wc.hInstance, NULL
	);

	if (!win32->hwnd) return 0;

	SetWindowLongPtr(win32->hwnd, GWLP_USERDATA, (LONG_PTR)term);

	/* Create memory DC for double buffering */
	win32->hdc = GetDC(win32->hwnd);
	win32->memdc = CreateCompatibleDC(win32->hdc);
	win32->bitmap = CreateCompatibleBitmap(win32->hdc, width, height);
	SelectObject(win32->memdc, win32->bitmap);

	/* Create font */
	win32->font = CreateFontA(
		win32->char_height, win32->char_width, 0, 0, FW_NORMAL,
		FALSE, FALSE, FALSE, DEFAULT_CHARSET,
		OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS,
		CLEARTYPE_QUALITY, FIXED_PITCH | FF_MODERN,
		"Consolas"
	);
	SelectObject(win32->memdc, win32->font);

	/* Set text rendering mode */
	SetBkMode(win32->memdc, OPAQUE);

	return 1;
}

/* Render terminal to window */
static void term_win32_render(term_t *term, term_win32_t *win32) {
	int cols, rows;
	term_get_size(term, &cols, &rows);

	/* Clear background */
	RECT rect = {0, 0, cols * win32->char_width + 20, rows * win32->char_height + 40};
	FillRect(win32->memdc, &rect, (HBRUSH)GetStockObject(BLACK_BRUSH));

	/* Render each cell */
	for (int row = 0; row < rows; row++) {
		for (int col = 0; col < cols; col++) {
			term_cell_t *cell = term_get_cell(term, col, row);
			if (!cell) continue;

			/* Set colors */
			COLORREF fg = (cell->fg < 16) ? win32_colors[cell->fg] : RGB(170,170,170);
			COLORREF bg = (cell->bg < 16) ? win32_colors[cell->bg] : RGB(0,0,0);

			if (cell->attr & TERM_ATTR_REVERSE) {
				COLORREF tmp = fg;
				fg = bg;
				bg = tmp;
			}

			SetTextColor(win32->memdc, fg);
			SetBkColor(win32->memdc, bg);

			/* Draw character */
			char ch = (char)(cell->ch & 0xFF);
			RECT char_rect = {
				col * win32->char_width,
				row * win32->char_height,
				(col + 1) * win32->char_width,
				(row + 1) * win32->char_height
			};
			DrawTextA(win32->memdc, &ch, 1, &char_rect, DT_SINGLELINE | DT_NOCLIP);
		}
	}

	/* Draw cursor */
	int cursor_x, cursor_y;
	term_get_cursor(term, &cursor_x, &cursor_y);
	RECT cursor_rect = {
		cursor_x * win32->char_width,
		cursor_y * win32->char_height + win32->char_height - 2,
		(cursor_x + 1) * win32->char_width,
		(cursor_y + 1) * win32->char_height
	};
	HBRUSH cursor_brush = CreateSolidBrush(RGB(255, 255, 255));
	FillRect(win32->memdc, &cursor_rect, cursor_brush);
	DeleteObject(cursor_brush);

	/* Blit to window */
	BitBlt(win32->hdc, 0, 0, rect.right, rect.bottom, win32->memdc, 0, 0, SRCCOPY);
}

/* Window procedure */
static LRESULT CALLBACK TermWindowProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
	term_t *term = (term_t *)GetWindowLongPtr(hwnd, GWLP_USERDATA);
	if (!term) return DefWindowProc(hwnd, msg, wParam, lParam);

	term_win32_t *win32 = (term_win32_t *)term->platform_data;

	switch (msg) {
	case WM_PAINT: {
		PAINTSTRUCT ps;
		BeginPaint(hwnd, &ps);
		term_win32_render(term, win32);
		EndPaint(hwnd, &ps);
		return 0;
	}

	case WM_CHAR:
		if (wParam >= 32 || wParam == '\r' || wParam == '\n' || wParam == '\t' || wParam == '\b') {
			char c = (char)wParam;
			if (term->output_cb) {
				term->output_cb(term, &c, 1, term->userdata);
			}
			InvalidateRect(hwnd, NULL, FALSE);
		}
		return 0;

	case WM_KEYDOWN:
		/* Handle special keys */
		switch (wParam) {
		case VK_BACK:
			if (term->output_cb) {
				term->output_cb(term, "\b", 1, term->userdata);
			}
			break;
		case VK_RETURN:
			if (term->output_cb) {
				term->output_cb(term, "\r\n", 2, term->userdata);
			}
			break;
		case VK_UP:
			if (term->output_cb) {
				term->output_cb(term, "\033[A", 3, term->userdata);
			}
			break;
		case VK_DOWN:
			if (term->output_cb) {
				term->output_cb(term, "\033[B", 3, term->userdata);
			}
			break;
		case VK_RIGHT:
			if (term->output_cb) {
				term->output_cb(term, "\033[C", 3, term->userdata);
			}
			break;
		case VK_LEFT:
			if (term->output_cb) {
				term->output_cb(term, "\033[D", 3, term->userdata);
			}
			break;
		}
		InvalidateRect(hwnd, NULL, FALSE);
		return 0;

	case WM_LBUTTONDOWN: {
		int x = LOWORD(lParam) / win32->char_width;
		int y = HIWORD(lParam) / win32->char_height;
		/* Handle mouse click */
		return 0;
	}

	case WM_MOUSEWHEEL: {
		int delta = GET_WHEEL_DELTA_WPARAM(wParam);
		/* Handle scrolling */
		return 0;
	}

	case WM_CLOSE:
		win32->running = 0;
		DestroyWindow(hwnd);
		return 0;

	case WM_DESTROY:
		PostQuitMessage(0);
		return 0;
	}

	return DefWindowProc(hwnd, msg, wParam, lParam);
}

/* Platform initialization */
int term_platform_init(void) {
	return 1;
}

void term_platform_shutdown(void) {
}

/* Public API with platform integration */
term_t *term_create_window(const term_config_t *config) {
	/* Create core terminal */
	term_t *term = term_create(config);
	if (!term) return NULL;

	/* Create platform data */
	term_win32_t *win32 = (term_win32_t *)calloc(1, sizeof(term_win32_t));
	if (!win32) {
		term_destroy(term);
		return NULL;
	}

	win32->char_width = 9;
	win32->char_height = 16;
	win32->running = 1;
	term->platform_data = win32;

	/* Create window */
	if (!term_win32_create_window(term, win32)) {
		free(win32);
		term_destroy(term);
		return NULL;
	}

	return term;
}

void term_destroy_window(term_t *term) {
	if (!term) return;

	term_win32_t *win32 = (term_win32_t *)term->platform_data;
	if (win32) {
		if (win32->font) DeleteObject(win32->font);
		if (win32->bitmap) DeleteObject(win32->bitmap);
		if (win32->memdc) DeleteDC(win32->memdc);
		if (win32->hdc) ReleaseDC(win32->hwnd, win32->hdc);
		if (win32->hwnd) DestroyWindow(win32->hwnd);
		free(win32);
	}

	term_destroy(term);
}

void term_run(term_t *term) {
	if (!term) return;

	term_win32_t *win32 = (term_win32_t *)term->platform_data;
	MSG msg;

	while (win32->running && GetMessage(&msg, NULL, 0, 0)) {
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	}
}

void term_refresh(term_t *term) {
	if (!term) return;

	term_win32_t *win32 = (term_win32_t *)term->platform_data;
	InvalidateRect(win32->hwnd, NULL, FALSE);
	UpdateWindow(win32->hwnd);
}

#endif /* _WIN32 */
