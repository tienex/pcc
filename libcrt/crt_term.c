/*
 * Copyright (c) 2025 PCC Project
 * CRT Library - Terminal Emulation Backend
 * Uses libterm for GUI terminal window instead of native console
 */

#include "crt.h"
#include "../libterm/term.h"
#include <stdlib.h>
#include <string.h>

/* Backend state */
static struct {
	int initialized;
	int use_term;
	term_t *terminal;
	int width;
	int height;
	crt_color_t current_fg;
	crt_color_t current_bg;
	int cursor_x;
	int cursor_y;
} term_backend = {0};

/* Color mapping: CRT colors to terminal colors */
static const term_color_t crt_to_term_color[] = {
	TERM_COLOR_BLACK,           /* CRT_BLACK */
	TERM_COLOR_BLUE,            /* CRT_BLUE */
	TERM_COLOR_GREEN,           /* CRT_GREEN */
	TERM_COLOR_CYAN,            /* CRT_CYAN */
	TERM_COLOR_RED,             /* CRT_RED */
	TERM_COLOR_MAGENTA,         /* CRT_MAGENTA */
	TERM_COLOR_YELLOW,          /* CRT_BROWN */
	TERM_COLOR_WHITE,           /* CRT_LIGHTGRAY */
	TERM_COLOR_BRIGHT_BLACK,    /* CRT_DARKGRAY */
	TERM_COLOR_BRIGHT_BLUE,     /* CRT_LIGHTBLUE */
	TERM_COLOR_BRIGHT_GREEN,    /* CRT_LIGHTGREEN */
	TERM_COLOR_BRIGHT_CYAN,     /* CRT_LIGHTCYAN */
	TERM_COLOR_BRIGHT_RED,      /* CRT_LIGHTRED */
	TERM_COLOR_BRIGHT_MAGENTA,  /* CRT_LIGHTMAGENTA */
	TERM_COLOR_BRIGHT_YELLOW,   /* CRT_YELLOW */
	TERM_COLOR_BRIGHT_WHITE     /* CRT_WHITE */
};

/* Terminal backend output callback */
static void term_output_cb(term_t *term, const char *data, size_t len, void *userdata) {
	(void)term;
	(void)userdata;
	/* Handle keyboard input - echo back to terminal */
	for (size_t i = 0; i < len; i++) {
		/* Process special keys if needed */
	}
}

/* Initialize terminal backend */
int crt_term_init(int width, int height) {
	if (term_backend.initialized) {
		return 1;
	}

	/* Configure terminal */
	term_config_t config = {0};
	config.cols = width > 0 ? width : 80;
	config.rows = height > 0 ? height : 25;
	config.scrollback_lines = 1000;
	config.enable_mouse = 1;
	config.enable_scroll = 1;
	config.enable_resize = 1;
	config.output_cb = term_output_cb;

	/* Create terminal window */
#if defined(_WIN32)
	extern term_t *term_create_window(const term_config_t *config);
	term_backend.terminal = term_create_window(&config);
#elif defined(__unix__) || defined(__linux__)
	/* Would create X11 or curses terminal */
	term_backend.terminal = term_create(&config);
#else
	/* Fallback to core terminal */
	term_backend.terminal = term_create(&config);
#endif

	if (!term_backend.terminal) {
		return 0;
	}

	term_backend.width = config.cols;
	term_backend.height = config.rows;
	term_backend.current_fg = CRT_LIGHTGRAY;
	term_backend.current_bg = CRT_BLACK;
	term_backend.initialized = 1;
	term_backend.use_term = 1;

	return 1;
}

/* Shutdown terminal backend */
void crt_term_shutdown(void) {
	if (!term_backend.initialized) return;

#if defined(_WIN32)
	extern void term_destroy_window(term_t *term);
	term_destroy_window(term_backend.terminal);
#else
	term_destroy(term_backend.terminal);
#endif

	term_backend.initialized = 0;
	term_backend.use_term = 0;
}

/* Check if terminal backend is active */
int crt_is_using_term(void) {
	return term_backend.use_term;
}

/* CRT functions using terminal backend */

void crt_term_clrscr(void) {
	if (!term_backend.use_term) return;
	term_clear(term_backend.terminal);
	term_backend.cursor_x = 0;
	term_backend.cursor_y = 0;
}

void crt_term_gotoxy(int x, int y) {
	if (!term_backend.use_term) return;
	term_backend.cursor_x = x;
	term_backend.cursor_y = y;
	term_set_cursor(term_backend.terminal, x, y);
}

int crt_term_wherex(void) {
	if (!term_backend.use_term) return 0;
	return term_backend.cursor_x;
}

int crt_term_wherey(void) {
	if (!term_backend.use_term) return 0;
	return term_backend.cursor_y;
}

void crt_term_textcolor(crt_color_t color) {
	if (!term_backend.use_term) return;
	term_backend.current_fg = color;
	term_set_fg(term_backend.terminal, crt_to_term_color[color & 15]);
}

void crt_term_textbackground(crt_color_t color) {
	if (!term_backend.use_term) return;
	term_backend.current_bg = color;
	term_set_bg(term_backend.terminal, crt_to_term_color[color & 15]);
}

void crt_term_putch(char c) {
	if (!term_backend.use_term) return;
	term_putc(term_backend.terminal, c);

	/* Update cursor position */
	int x, y;
	term_get_cursor(term_backend.terminal, &x, &y);
	term_backend.cursor_x = x;
	term_backend.cursor_y = y;

#if defined(_WIN32)
	/* Refresh window */
	extern void term_refresh(term_t *term);
	term_refresh(term_backend.terminal);
#endif
}

void crt_term_cputs(const char *str) {
	if (!term_backend.use_term) return;
	while (*str) {
		crt_term_putch(*str++);
	}
}

int crt_term_getch(void) {
	if (!term_backend.use_term) return 0;

	/* Poll for input from terminal window */
	/* This would need platform-specific message loop integration */
#if defined(_WIN32)
	/* Windows message loop would provide keyboard input */
	extern void term_run(term_t *term);
	/* Run one iteration of message loop */
#endif

	/* For now, return a placeholder */
	return 0;
}

void crt_term_clreol(void) {
	if (!term_backend.use_term) return;
	term_clear_line(term_backend.terminal);
}

void crt_term_insline(void) {
	if (!term_backend.use_term) return;
	/* Insert line at current cursor position */
	/* Would need to implement in libterm */
}

void crt_term_delline(void) {
	if (!term_backend.use_term) return;
	/* Delete current line */
	/* Would need to implement in libterm */
}

void crt_term_highvideo(void) {
	if (!term_backend.use_term) return;
	term_set_attr(term_backend.terminal, TERM_ATTR_BOLD);
}

void crt_term_lowvideo(void) {
	if (!term_backend.use_term) return;
	term_set_attr(term_backend.terminal, TERM_ATTR_DIM);
}

void crt_term_normvideo(void) {
	if (!term_backend.use_term) return;
	term_reset_attr(term_backend.terminal);
}

/* Runtime selection between native console and terminal */
void crt_set_backend(int use_terminal) {
	if (use_terminal && !term_backend.initialized) {
		/* Initialize terminal backend */
		crt_term_init(80, 25);
	} else if (!use_terminal && term_backend.initialized) {
		/* Switch back to native console */
		crt_term_shutdown();
	}
}

/* Auto-detection: Use terminal on GUI platforms */
int crt_should_use_terminal(void) {
#if defined(_WIN32) && !defined(_CONSOLE)
	/* Windows GUI application */
	return 1;
#elif defined(__unix__) && defined(DISPLAY)
	/* Unix with X11 display available */
	return 1;
#else
	/* Console application or no GUI available */
	return 0;
#endif
}
