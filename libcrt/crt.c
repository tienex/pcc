/*
 * Copyright (c) 2025 PCC Project
 * Borland CRT Unit Emulation Library - Implementation
 * Cross-platform console I/O using ANSI escape sequences and platform APIs
 */

#include "crt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#include <conio.h>
#else
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#endif

/*
 * Platform-specific state
 */
static struct {
	int initialized;
	int cursor_x;
	int cursor_y;
	int window_x1, window_y1, window_x2, window_y2;
	crt_color_t text_color;
	crt_color_t background_color;
	uint8_t text_attr;
	uint8_t normal_attr;
	crt_text_mode_t current_mode;
	crt_text_mode_t last_mode;
	int screen_width;
	int screen_height;
	int use_ansi;
	crt_break_handler_t break_handler;

#ifndef _WIN32
	struct termios orig_termios;
	int termios_saved;
#else
	HANDLE h_stdout;
	HANDLE h_stdin;
	CONSOLE_SCREEN_BUFFER_INFO orig_csbi;
#endif
} crt_state = {
	.initialized = 0,
	.cursor_x = 1,
	.cursor_y = 1,
	.window_x1 = 1,
	.window_y1 = 1,
	.window_x2 = 80,
	.window_y2 = 25,
	.text_color = CRT_LIGHTGRAY,
	.background_color = CRT_BLACK,
	.text_attr = 0x07,
	.normal_attr = 0x07,
	.current_mode = CRT_CO80,
	.last_mode = CRT_CO80,
	.screen_width = 80,
	.screen_height = 25,
	.use_ansi = 1,
	.break_handler = NULL
};

/* Global variables for Borland compatibility */
int crt_directvideo = 0;
int crt_checksnow = 0;
int crt_checkbreak = 1;
crt_text_mode_t crt_lastmode_var = CRT_CO80;

/*
 * ANSI Color Mapping
 */
static const int ansi_colors[] = {
	30, // Black
	34, // Blue
	32, // Green
	36, // Cyan
	31, // Red
	35, // Magenta
	33, // Brown/Yellow
	37, // Light Gray
	90, // Dark Gray
	94, // Light Blue
	92, // Light Green
	96, // Light Cyan
	91, // Light Red
	95, // Light Magenta
	93, // Yellow
	97  // White
};

/*
 * Internal Helper Functions
 */

#ifndef _WIN32
static void disable_raw_mode(void) {
	if (crt_state.termios_saved) {
		tcsetattr(STDIN_FILENO, TCSAFLUSH, &crt_state.orig_termios);
	}
}

static void enable_raw_mode(void) {
	struct termios raw;

	if (!crt_state.termios_saved) {
		tcgetattr(STDIN_FILENO, &crt_state.orig_termios);
		crt_state.termios_saved = 1;
		atexit(disable_raw_mode);
	}

	raw = crt_state.orig_termios;
	raw.c_lflag &= ~(ECHO | ICANON);
	raw.c_cc[VMIN] = 1;
	raw.c_cc[VTIME] = 0;

	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
#endif

static void update_screen_size(void) {
#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	if (GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi)) {
		crt_state.screen_width = csbi.srWindow.Right - csbi.srWindow.Left + 1;
		crt_state.screen_height = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
	}
#else
	struct winsize w;
	if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &w) == 0) {
		crt_state.screen_width = w.ws_col;
		crt_state.screen_height = w.ws_row;
	}
#endif

	crt_state.window_x2 = crt_state.screen_width;
	crt_state.window_y2 = crt_state.screen_height;
}

/*
 * Initialization and Cleanup
 */

int crt_init(void) {
	if (crt_state.initialized) {
		return 0;
	}

#ifdef _WIN32
	crt_state.h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
	crt_state.h_stdin = GetStdHandle(STD_INPUT_HANDLE);
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &crt_state.orig_csbi);
	crt_state.use_ansi = 0;
#else
	crt_state.use_ansi = isatty(STDOUT_FILENO);
	if (crt_state.use_ansi) {
		enable_raw_mode();
	}
#endif

	update_screen_size();
	crt_state.initialized = 1;

	return 0;
}

void crt_done(void) {
	if (!crt_state.initialized) {
		return;
	}

#ifndef _WIN32
	disable_raw_mode();
#endif

	crt_state.initialized = 0;
}

/*
 * Screen Control
 */

void crt_clrscr(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	COORD coordScreen = { 0, 0 };
	DWORD cCharsWritten;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	DWORD dwConSize;

	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	dwConSize = csbi.dwSize.X * csbi.dwSize.Y;

	FillConsoleOutputCharacter(crt_state.h_stdout, ' ', dwConSize, coordScreen, &cCharsWritten);
	FillConsoleOutputAttribute(crt_state.h_stdout, csbi.wAttributes, dwConSize, coordScreen, &cCharsWritten);
	SetConsoleCursorPosition(crt_state.h_stdout, coordScreen);
#else
	if (crt_state.use_ansi) {
		printf("\033[2J\033[H");
		fflush(stdout);
	}
#endif

	crt_state.cursor_x = 1;
	crt_state.cursor_y = 1;
}

void crt_clreol(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	COORD coord;
	DWORD written;

	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	coord = csbi.dwCursorPosition;

	DWORD length = csbi.dwSize.X - coord.X;
	FillConsoleOutputCharacter(crt_state.h_stdout, ' ', length, coord, &written);
#else
	if (crt_state.use_ansi) {
		printf("\033[K");
		fflush(stdout);
	}
#endif
}

void crt_gotoxy(int x, int y) {
	if (!crt_state.initialized) crt_init();

	crt_state.cursor_x = x;
	crt_state.cursor_y = y;

#ifdef _WIN32
	COORD coord;
	coord.X = x - 1;
	coord.Y = y - 1;
	SetConsoleCursorPosition(crt_state.h_stdout, coord);
#else
	if (crt_state.use_ansi) {
		printf("\033[%d;%dH", y, x);
		fflush(stdout);
	}
#endif
}

int crt_wherex(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	return csbi.dwCursorPosition.X + 1;
#else
	return crt_state.cursor_x;
#endif
}

int crt_wherey(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	return csbi.dwCursorPosition.Y + 1;
#else
	return crt_state.cursor_y;
#endif
}

void crt_insline(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	// Not easily supported on Windows Console
#else
	if (crt_state.use_ansi) {
		printf("\033[L");
		fflush(stdout);
	}
#endif
}

void crt_delline(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	// Not easily supported on Windows Console
#else
	if (crt_state.use_ansi) {
		printf("\033[M");
		fflush(stdout);
	}
#endif
}

/*
 * Text Attributes
 */

void crt_textcolor(crt_color_t color) {
	if (!crt_state.initialized) crt_init();

	crt_state.text_color = color & 0x0F;

#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	WORD attrs = (csbi.wAttributes & 0xF0) | (crt_state.text_color);
	SetConsoleTextAttribute(crt_state.h_stdout, attrs);
#else
	if (crt_state.use_ansi) {
		int intense = (color & 0x08) ? 1 : 0;
		printf("\033[%d;%dm", intense, ansi_colors[color & 0x07]);
		fflush(stdout);
	}
#endif
}

void crt_textbackground(crt_color_t color) {
	if (!crt_state.initialized) crt_init();

	crt_state.background_color = color & 0x0F;

#ifdef _WIN32
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	WORD attrs = (csbi.wAttributes & 0x0F) | (crt_state.background_color << 4);
	SetConsoleTextAttribute(crt_state.h_stdout, attrs);
#else
	if (crt_state.use_ansi) {
		printf("\033[%dm", ansi_colors[color & 0x07] + 10);
		fflush(stdout);
	}
#endif
}

void crt_textattr(uint8_t attr) {
	crt_state.text_attr = attr;
	crt_textcolor((crt_color_t)(attr & 0x0F));
	crt_textbackground((crt_color_t)((attr >> 4) & 0x0F));
}

void crt_normvideo(void) {
	crt_textattr(crt_state.normal_attr);
}

void crt_lowvideo(void) {
	crt_textcolor((crt_color_t)(crt_state.text_color & 0x07));
}

void crt_highvideo(void) {
	crt_textcolor((crt_color_t)(crt_state.text_color | 0x08));
}

/*
 * Text Modes
 */

void crt_textmode(crt_text_mode_t mode) {
	crt_state.last_mode = crt_state.current_mode;
	crt_state.current_mode = mode;
	crt_lastmode_var = crt_state.last_mode;

	// Update screen dimensions based on mode
	switch (mode) {
	case CRT_BW40:
	case CRT_CO40:
		crt_state.screen_width = 40;
		crt_state.screen_height = 25;
		break;
	case CRT_BW80:
	case CRT_CO80:
	case CRT_MONO:
		crt_state.screen_width = 80;
		crt_state.screen_height = 25;
		break;
	default:
		update_screen_size();
		break;
	}
}

crt_text_mode_t crt_lastmode(void) {
	return crt_state.last_mode;
}

/*
 * Window Management
 */

void crt_window(int x1, int y1, int x2, int y2) {
	crt_state.window_x1 = x1;
	crt_state.window_y1 = y1;
	crt_state.window_x2 = x2;
	crt_state.window_y2 = y2;
}

void crt_getwindow(int *x1, int *y1, int *x2, int *y2) {
	if (x1) *x1 = crt_state.window_x1;
	if (y1) *y1 = crt_state.window_y1;
	if (x2) *x2 = crt_state.window_x2;
	if (y2) *y2 = crt_state.window_y2;
}

/*
 * Keyboard Input
 */

int crt_keypressed(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	return _kbhit();
#else
	struct timeval tv = { 0, 0 };
	fd_set fds;
	FD_ZERO(&fds);
	FD_SET(STDIN_FILENO, &fds);
	return select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) > 0;
#endif
}

int crt_readkey(void) {
	if (!crt_state.initialized) crt_init();

#ifdef _WIN32
	return _getch();
#else
	char ch;
	if (read(STDIN_FILENO, &ch, 1) == 1) {
		return (int)(unsigned char)ch;
	}
	return 0;
#endif
}

uint16_t crt_readkey_ex(void) {
	int ch = crt_readkey();

	// Check for extended key (arrow keys, function keys, etc.)
	if (ch == 0 || ch == 224) {
		int ext = crt_readkey();
		return (uint16_t)((ext << 8) | ch);
	}

	return (uint16_t)ch;
}

/*
 * Sound (simplified - no actual PC speaker emulation)
 */

void crt_sound(int frequency) {
	// PC speaker emulation not implemented
	// Could use platform-specific audio APIs
	(void)frequency;
}

void crt_nosound(void) {
	// PC speaker emulation not implemented
}

void crt_delay(int milliseconds) {
#ifdef _WIN32
	Sleep(milliseconds);
#else
	struct timespec ts;
	ts.tv_sec = milliseconds / 1000;
	ts.tv_nsec = (milliseconds % 1000) * 1000000;
	nanosleep(&ts, NULL);
#endif
}

/*
 * Screen Size
 */

int crt_screenwidth(void) {
	if (!crt_state.initialized) crt_init();
	return crt_state.screen_width;
}

int crt_screenheight(void) {
	if (!crt_state.initialized) crt_init();
	return crt_state.screen_height;
}

/*
 * Text Information
 */

void crt_gettextinfo(crt_text_info_t *info) {
	if (!info) return;
	if (!crt_state.initialized) crt_init();

	info->winleft = crt_state.window_x1;
	info->wintop = crt_state.window_y1;
	info->winright = crt_state.window_x2;
	info->winbottom = crt_state.window_y2;
	info->attribute = crt_state.text_attr;
	info->normattr = crt_state.normal_attr;
	info->currmode = crt_state.current_mode;
	info->screenheight = crt_state.screen_height;
	info->screenwidth = crt_state.screen_width;
	info->curx = crt_state.cursor_x;
	info->cury = crt_state.cursor_y;
}

/*
 * Advanced Functions
 */

void crt_putch(int ch) {
	putchar(ch);
	fflush(stdout);
}

void crt_cputs(const char *str) {
	if (str) {
		fputs(str, stdout);
		fflush(stdout);
	}
}

int crt_getch(void) {
	return crt_readkey();
}

int crt_getche(void) {
	int ch = crt_readkey();
	crt_putch(ch);
	return ch;
}

int crt_ungetch(int ch) {
#ifdef _WIN32
	return _ungetch(ch);
#else
	// Not easily supported on Unix
	(void)ch;
	return -1;
#endif
}

int crt_ctrlbrk(void) {
	// Check for Ctrl+Break
	return 0;
}

void crt_setcbreak(crt_break_handler_t handler) {
	crt_state.break_handler = handler;
}

/*
 * Borland-specific Routines
 */

void crt_assigncrt(void) {
	// Assign CRT to standard I/O - no-op on modern systems
}

void crt_flushkeybuf(void) {
#ifdef _WIN32
	while (_kbhit()) {
		_getch();
	}
#else
	tcflush(STDIN_FILENO, TCIFLUSH);
#endif
}

void crt_setblink(int enable) {
	// Blink mode control - not widely supported
	(void)enable;
}

int crt_getblink(void) {
	return 0;
}

void crt_setcursortype(crt_cursor_type_t type) {
#ifdef _WIN32
	CONSOLE_CURSOR_INFO cci;
	switch (type) {
	case CRT_CURSOR_OFF:
		cci.bVisible = FALSE;
		break;
	case CRT_CURSOR_NORMAL:
		cci.dwSize = 25;
		cci.bVisible = TRUE;
		break;
	case CRT_CURSOR_SOLID:
		cci.dwSize = 100;
		cci.bVisible = TRUE;
		break;
	}
	SetConsoleCursorInfo(crt_state.h_stdout, &cci);
#else
	if (crt_state.use_ansi) {
		switch (type) {
		case CRT_CURSOR_OFF:
			printf("\033[?25l");
			break;
		case CRT_CURSOR_NORMAL:
		case CRT_CURSOR_SOLID:
			printf("\033[?25h");
			break;
		}
		fflush(stdout);
	}
#endif
}

crt_cursor_type_t crt_getcursortype(void) {
#ifdef _WIN32
	CONSOLE_CURSOR_INFO cci;
	GetConsoleCursorInfo(crt_state.h_stdout, &cci);
	if (!cci.bVisible) return CRT_CURSOR_OFF;
	if (cci.dwSize >= 50) return CRT_CURSOR_SOLID;
	return CRT_CURSOR_NORMAL;
#else
	return CRT_CURSOR_NORMAL;
#endif
}

/*
 * Platform-Specific Configuration
 */

void crt_use_ansi(int enable) {
	crt_state.use_ansi = enable;
}

void crt_use_ncurses(int enable) {
	// ncurses support not implemented yet
	(void)enable;
}

void crt_use_wincon(int enable) {
	// Already using Windows Console API on Windows
	(void)enable;
}
