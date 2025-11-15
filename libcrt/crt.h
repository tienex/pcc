/*
 * Copyright (c) 2025 PCC Project
 *
 * Borland CRT Unit Emulation Library
 *
 * Provides compatibility layer for Borland Pascal CRT unit functions
 * including console I/O, text modes, colors, and keyboard handling.
 * Cross-platform implementation using ANSI escape sequences and ncurses.
 */

#ifndef _PCC_CRT_H_
#define _PCC_CRT_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Text Colors (Borland Pascal compatible)
 */
typedef enum {
	CRT_BLACK = 0,
	CRT_BLUE = 1,
	CRT_GREEN = 2,
	CRT_CYAN = 3,
	CRT_RED = 4,
	CRT_MAGENTA = 5,
	CRT_BROWN = 6,
	CRT_LIGHTGRAY = 7,
	CRT_DARKGRAY = 8,
	CRT_LIGHTBLUE = 9,
	CRT_LIGHTGREEN = 10,
	CRT_LIGHTCYAN = 11,
	CRT_LIGHTRED = 12,
	CRT_LIGHTMAGENTA = 13,
	CRT_YELLOW = 14,
	CRT_WHITE = 15,
	CRT_BLINK = 128
} crt_color_t;

/*
 * Text Modes
 */
typedef enum {
	CRT_BW40 = 0,          /* 40x25 B/W */
	CRT_CO40 = 1,          /* 40x25 Color */
	CRT_BW80 = 2,          /* 80x25 B/W */
	CRT_CO80 = 3,          /* 80x25 Color */
	CRT_MONO = 7,          /* 80x25 Monochrome */
	CRT_FONT8X8 = 256      /* 8x8 font flag */
} crt_text_mode_t;

/*
 * Special Key Codes (for ReadKey)
 */
#define CRT_KEY_F1       0x3B00
#define CRT_KEY_F2       0x3C00
#define CRT_KEY_F3       0x3D00
#define CRT_KEY_F4       0x3E00
#define CRT_KEY_F5       0x3F00
#define CRT_KEY_F6       0x4000
#define CRT_KEY_F7       0x4100
#define CRT_KEY_F8       0x4200
#define CRT_KEY_F9       0x4300
#define CRT_KEY_F10      0x4400
#define CRT_KEY_HOME     0x4700
#define CRT_KEY_UP       0x4800
#define CRT_KEY_PGUP     0x4900
#define CRT_KEY_LEFT     0x4B00
#define CRT_KEY_RIGHT    0x4D00
#define CRT_KEY_END      0x4F00
#define CRT_KEY_DOWN     0x5000
#define CRT_KEY_PGDN     0x5100
#define CRT_KEY_INSERT   0x5200
#define CRT_KEY_DELETE   0x5300

/*
 * Initialization and Cleanup
 */

/* Initialize CRT system */
int crt_init(void);

/* Shutdown CRT system */
void crt_done(void);

/*
 * Screen Control
 */

/* Clear screen */
void crt_clrscr(void);

/* Clear to end of line */
void crt_clreol(void);

/* Move cursor to position (1-based) */
void crt_gotoxy(int x, int y);

/* Get cursor X position */
int crt_wherex(void);

/* Get cursor Y position */
int crt_wherey(void);

/* Insert line at cursor */
void crt_insline(void);

/* Delete line at cursor */
void crt_delline(void);

/*
 * Text Attributes
 */

/* Set text color */
void crt_textcolor(crt_color_t color);

/* Set background color */
void crt_textbackground(crt_color_t color);

/* Low-level set attribute */
void crt_textattr(uint8_t attr);

/* Set normal video */
void crt_normvideo(void);

/* Set low intensity */
void crt_lowvideo(void);

/* Set high intensity */
void crt_highvideo(void);

/*
 * Text Modes
 */

/* Set text mode */
void crt_textmode(crt_text_mode_t mode);

/* Get last text mode */
crt_text_mode_t crt_lastmode(void);

/*
 * Window Management
 */

/* Define text window */
void crt_window(int x1, int y1, int x2, int y2);

/* Get window boundaries */
void crt_getwindow(int *x1, int *y1, int *x2, int *y2);

/*
 * Keyboard Input
 */

/* Check if key pressed */
int crt_keypressed(void);

/* Read character (blocking) */
int crt_readkey(void);

/* Read extended key code */
uint16_t crt_readkey_ex(void);

/*
 * Sound (PC Speaker emulation)
 */

/* Generate sound at frequency */
void crt_sound(int frequency);

/* Stop sound */
void crt_nosound(void);

/* Delay in milliseconds */
void crt_delay(int milliseconds);

/*
 * Screen Size
 */

/* Get screen width */
int crt_screenwidth(void);

/* Get screen height */
int crt_screenheight(void);

/*
 * Direct Video Memory Access (emulated)
 */

/* Enable/disable direct video */
extern int crt_directvideo;

/* Check snow (CGA snow elimination) */
extern int crt_checksnow;

/* Check for break */
extern int crt_checkbreak;

/* Last text mode */
extern crt_text_mode_t crt_lastmode_var;

/*
 * Text Information
 */
typedef struct {
	int winleft;          /* Left window coordinate */
	int wintop;           /* Top window coordinate */
	int winright;         /* Right window coordinate */
	int winbottom;        /* Bottom window coordinate */
	uint8_t attribute;    /* Current attribute */
	uint8_t normattr;     /* Normal attribute */
	crt_text_mode_t currmode;  /* Current text mode */
	int screenheight;     /* Screen height */
	int screenwidth;      /* Screen width */
	int curx;             /* Current X position */
	int cury;             /* Current Y position */
} crt_text_info_t;

/* Get text information */
void crt_gettextinfo(crt_text_info_t *info);

/*
 * Advanced Functions
 */

/* Write character at cursor */
void crt_putch(int ch);

/* Write string at cursor */
void crt_cputs(const char *str);

/* Get character from keyboard (with echo) */
int crt_getch(void);

/* Get character from keyboard (without echo) */
int crt_getche(void);

/* Put character back into keyboard buffer */
int crt_ungetch(int ch);

/* Check for Ctrl-Break */
int crt_ctrlbrk(void);

/* Set Ctrl-Break handler */
typedef void (*crt_break_handler_t)(void);
void crt_setcbreak(crt_break_handler_t handler);

/*
 * Borland-specific Routines
 */

/* Assign CRT to standard I/O */
void crt_assigncrt(void);

/* Flush keyboard buffer */
void crt_flushkeybuf(void);

/* Set blink mode */
void crt_setblink(int enable);

/* Get blink mode */
int crt_getblink(void);

/* Set cursor type */
typedef enum {
	CRT_CURSOR_OFF = 0,
	CRT_CURSOR_NORMAL = 1,
	CRT_CURSOR_SOLID = 2
} crt_cursor_type_t;

void crt_setcursortype(crt_cursor_type_t type);

/* Get cursor type */
crt_cursor_type_t crt_getcursortype(void);

/*
 * Platform-Specific Configuration
 */

/* Use ANSI escape sequences (default on Unix) */
void crt_use_ansi(int enable);

/* Use ncurses (if available) */
void crt_use_ncurses(int enable);

/* Use Windows Console API (if available) */
void crt_use_wincon(int enable);

/*
 * Compatibility Macros (for Pascal code)
 */
#define ClrScr()           crt_clrscr()
#define ClrEol()           crt_clreol()
#define GotoXY(x, y)       crt_gotoxy(x, y)
#define WhereX()           crt_wherex()
#define WhereY()           crt_wherey()
#define TextColor(c)       crt_textcolor(c)
#define TextBackground(c)  crt_textbackground(c)
#define TextAttr(a)        crt_textattr(a)
#define TextMode(m)        crt_textmode(m)
#define Window(x1,y1,x2,y2) crt_window(x1, y1, x2, y2)
#define KeyPressed()       crt_keypressed()
#define ReadKey()          crt_readkey()
#define Sound(f)           crt_sound(f)
#define NoSound()          crt_nosound()
#define Delay(ms)          crt_delay(ms)
#define NormVideo()        crt_normvideo()
#define LowVideo()         crt_lowvideo()
#define HighVideo()        crt_highvideo()
#define InsLine()          crt_insline()
#define DelLine()          crt_delline()

#ifdef __cplusplus
}
#endif

#endif /* _PCC_CRT_H_ */
