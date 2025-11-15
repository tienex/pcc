/*
 * Copyright (c) 2025 PCC Project
 * Terminal Emulation Library
 * Cross-platform terminal with scroll, mouse, custom fonts, and remoting
 */

#ifndef TERM_H
#define TERM_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declarations */
typedef struct term term_t;
typedef struct term_config term_config_t;
typedef struct term_cell term_cell_t;
typedef struct term_font term_font_t;
typedef struct term_remote term_remote_t;

/* Terminal cell attributes */
typedef enum {
	TERM_ATTR_NONE       = 0,
	TERM_ATTR_BOLD       = (1 << 0),
	TERM_ATTR_DIM        = (1 << 1),
	TERM_ATTR_ITALIC     = (1 << 2),
	TERM_ATTR_UNDERLINE  = (1 << 3),
	TERM_ATTR_BLINK      = (1 << 4),
	TERM_ATTR_REVERSE    = (1 << 5),
	TERM_ATTR_HIDDEN     = (1 << 6),
	TERM_ATTR_STRIKE     = (1 << 7)
} term_attr_t;

/* Terminal colors (16 basic colors + extended) */
typedef enum {
	TERM_COLOR_BLACK = 0,
	TERM_COLOR_RED,
	TERM_COLOR_GREEN,
	TERM_COLOR_YELLOW,
	TERM_COLOR_BLUE,
	TERM_COLOR_MAGENTA,
	TERM_COLOR_CYAN,
	TERM_COLOR_WHITE,
	TERM_COLOR_BRIGHT_BLACK,
	TERM_COLOR_BRIGHT_RED,
	TERM_COLOR_BRIGHT_GREEN,
	TERM_COLOR_BRIGHT_YELLOW,
	TERM_COLOR_BRIGHT_BLUE,
	TERM_COLOR_BRIGHT_MAGENTA,
	TERM_COLOR_BRIGHT_CYAN,
	TERM_COLOR_BRIGHT_WHITE,
	TERM_COLOR_DEFAULT = 256  /* Use default foreground/background */
} term_color_t;

/* Terminal cell (character + attributes) */
struct term_cell {
	uint32_t ch;           /* Unicode codepoint */
	term_attr_t attr;      /* Attributes (bold, underline, etc.) */
	term_color_t fg;       /* Foreground color */
	term_color_t bg;       /* Background color */
};

/* Mouse event types */
typedef enum {
	TERM_MOUSE_PRESS,
	TERM_MOUSE_RELEASE,
	TERM_MOUSE_MOTION,
	TERM_MOUSE_WHEEL_UP,
	TERM_MOUSE_WHEEL_DOWN
} term_mouse_event_t;

/* Mouse button */
typedef enum {
	TERM_MOUSE_BUTTON_LEFT = 1,
	TERM_MOUSE_BUTTON_MIDDLE = 2,
	TERM_MOUSE_BUTTON_RIGHT = 3
} term_mouse_button_t;

/* Keyboard modifiers */
typedef enum {
	TERM_MOD_NONE  = 0,
	TERM_MOD_SHIFT = (1 << 0),
	TERM_MOD_CTRL  = (1 << 1),
	TERM_MOD_ALT   = (1 << 2),
	TERM_MOD_META  = (1 << 3)
} term_modifier_t;

/* Remote transport types */
typedef enum {
	TERM_REMOTE_NONE,
	TERM_REMOTE_SOCKET,    /* TCP/Unix socket */
	TERM_REMOTE_PIPE,      /* Named pipe */
	TERM_REMOTE_SERIAL,    /* Serial port */
	TERM_REMOTE_SSH,       /* SSH connection */
	TERM_REMOTE_CUSTOM     /* Custom transport */
} term_remote_type_t;

/* Callbacks */
typedef void (*term_output_cb_t)(term_t *term, const char *data, size_t len, void *userdata);
typedef void (*term_resize_cb_t)(term_t *term, int cols, int rows, void *userdata);
typedef void (*term_title_cb_t)(term_t *term, const char *title, void *userdata);
typedef void (*term_bell_cb_t)(term_t *term, void *userdata);

/* Remote transport callbacks */
typedef ssize_t (*term_remote_read_cb_t)(void *ctx, void *buf, size_t len);
typedef ssize_t (*term_remote_write_cb_t)(void *ctx, const void *buf, size_t len);
typedef void (*term_remote_close_cb_t)(void *ctx);

/* Terminal configuration */
struct term_config {
	int cols;                   /* Number of columns (default: 80) */
	int rows;                   /* Number of rows (default: 24) */
	int scrollback_lines;       /* Scrollback buffer size (default: 1000) */
	int enable_mouse;           /* Enable mouse support */
	int enable_scroll;          /* Enable scrolling */
	int enable_resize;          /* Enable dynamic resizing */
	term_font_t *font;          /* Custom font (NULL = system default) */
	term_output_cb_t output_cb; /* Output callback */
	term_resize_cb_t resize_cb; /* Resize callback */
	term_title_cb_t title_cb;   /* Title change callback */
	term_bell_cb_t bell_cb;     /* Bell callback */
	void *userdata;             /* User data passed to callbacks */
};

/* Font structure */
struct term_font {
	const char *name;
	int width;                  /* Character width in pixels */
	int height;                 /* Character height in pixels */
	int baseline;               /* Baseline offset */
	void *data;                 /* Platform-specific font data */
	void (*render)(term_font_t *font, uint32_t ch, void *dest, int pitch);
	void (*destroy)(term_font_t *font);
};

/* Remote transport */
struct term_remote {
	term_remote_type_t type;
	void *ctx;                  /* Transport context */
	term_remote_read_cb_t read;
	term_remote_write_cb_t write;
	term_remote_close_cb_t close;
};

/*
 * Core API
 */

/* Create/destroy terminal */
term_t *term_create(const term_config_t *config);
void term_destroy(term_t *term);

/* Configuration */
void term_get_size(term_t *term, int *cols, int *rows);
void term_resize(term_t *term, int cols, int rows);
void term_set_font(term_t *term, term_font_t *font);
term_font_t *term_get_font(term_t *term);

/* Input/output */
void term_write(term_t *term, const char *data, size_t len);
void term_printf(term_t *term, const char *fmt, ...);
void term_putc(term_t *term, char c);
int term_read(term_t *term, char *buf, size_t len);

/* Cursor control */
void term_get_cursor(term_t *term, int *col, int *row);
void term_set_cursor(term_t *term, int col, int row);
void term_show_cursor(term_t *term, int show);

/* Screen manipulation */
void term_clear(term_t *term);
void term_clear_line(term_t *term);
void term_scroll(term_t *term, int lines);  /* Negative = scroll up, positive = scroll down */
void term_reset(term_t *term);

/* Cell access */
term_cell_t *term_get_cell(term_t *term, int col, int row);
void term_set_cell(term_t *term, int col, int row, const term_cell_t *cell);

/* Attributes */
void term_set_attr(term_t *term, term_attr_t attr);
void term_set_fg(term_t *term, term_color_t color);
void term_set_bg(term_t *term, term_color_t color);
void term_reset_attr(term_t *term);

/* Mouse support */
void term_mouse_event(term_t *term, term_mouse_event_t event,
                      int x, int y, term_mouse_button_t button,
                      term_modifier_t mods);
void term_enable_mouse(term_t *term, int enable);

/* Scrollback */
int term_get_scrollback_size(term_t *term);
void term_set_scrollback_size(term_t *term, int lines);
term_cell_t *term_get_scrollback_line(term_t *term, int line);

/* Title */
void term_set_title(term_t *term, const char *title);
const char *term_get_title(term_t *term);

/* Bell */
void term_bell(term_t *term);

/* Remote transport */
void term_set_remote(term_t *term, term_remote_t *remote);
term_remote_t *term_get_remote(term_t *term);
void term_remote_disconnect(term_t *term);

/* Platform-specific features */
void *term_get_native_handle(term_t *term);  /* Get platform-specific handle */
void term_use_native_feature(term_t *term, const char *feature, void *param);

/*
 * Font API
 */

/* Load font */
term_font_t *term_font_load(const char *name, int width, int height);
term_font_t *term_font_load_file(const char *path, int width, int height);
term_font_t *term_font_create_bitmap(const char *name, int width, int height,
                                      const uint8_t *bitmap_data);
void term_font_destroy(term_font_t *font);

/*
 * Remote transport API
 */

/* Create remote transports */
term_remote_t *term_remote_create_socket(const char *host, int port);
term_remote_t *term_remote_create_pipe(const char *name);
term_remote_t *term_remote_create_serial(const char *device, int baudrate);
term_remote_t *term_remote_create_custom(void *ctx,
                                          term_remote_read_cb_t read,
                                          term_remote_write_cb_t write,
                                          term_remote_close_cb_t close);
void term_remote_destroy(term_remote_t *remote);

/*
 * Platform initialization (called automatically)
 */
int term_platform_init(void);
void term_platform_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* TERM_H */
