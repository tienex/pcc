/*
 * Copyright (c) 2025 PCC Project
 *
 * Borland Graphics Interface (BGI) Emulation Library
 *
 * Provides compatibility layer for:
 * - Borland BGI (Graphics Interface)
 * - Microsoft GRAPH library
 * - Microsoft PGCHART (Presentation Graphics)
 *
 * Cross-platform implementation using SDL2, OpenGL, or framebuffer
 */

#ifndef _PCC_BGI_H_
#define _PCC_BGI_H_

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Graphics Drivers
 */
typedef enum {
	DETECT = 0,           /* Auto-detect */
	CGA = 1,
	MCGA = 2,
	EGA = 3,
	EGA64 = 4,
	EGAMONO = 5,
	IBM8514 = 6,
	HERCMONO = 7,
	ATT400 = 8,
	VGA = 9,
	PC3270 = 10,
	SDL = 100,            /* SDL2 backend */
	OPENGL = 101,         /* OpenGL backend */
	FRAMEBUFFER = 102     /* Linux framebuffer */
} bgi_driver_t;

/*
 * Graphics Modes
 */
typedef enum {
	CGAC0 = 0,            /* 320x200 C0 */
	CGAC1 = 1,            /* 320x200 C1 */
	CGAC2 = 2,            /* 320x200 C2 */
	CGAC3 = 3,            /* 320x200 C3 */
	CGAHI = 4,            /* 640x200 */
	MCGAC0 = 0,           /* 320x200 C0 */
	MCGAC1 = 1,           /* 320x200 C1 */
	MCGAC2 = 2,           /* 320x200 C2 */
	MCGAC3 = 3,           /* 320x200 C3 */
	MCGAMED = 4,          /* 640x200 */
	MCGAHI = 5,           /* 640x480 */
	EGALO = 0,            /* 640x200 */
	EGAHI = 1,            /* 640x350 */
	EGA64LO = 0,          /* 640x200 */
	EGA64HI = 1,          /* 640x350 */
	EGAMONOHI = 3,        /* 640x350 */
	HERCMONOHI = 0,       /* 720x348 */
	ATT400C0 = 0,         /* 320x200 C0 */
	ATT400C1 = 1,         /* 320x200 C1 */
	ATT400C2 = 2,         /* 320x200 C2 */
	ATT400C3 = 3,         /* 320x200 C3 */
	ATT400MED = 4,        /* 640x200 */
	ATT400HI = 5,         /* 640x400 */
	VGALO = 0,            /* 640x200 */
	VGAMED = 1,           /* 640x350 */
	VGAHI = 2,            /* 640x480 */
	PC3270HI = 0,         /* 720x350 */
	IBM8514LO = 0,        /* 640x480 */
	IBM8514HI = 1         /* 1024x768 */
} bgi_mode_t;

/*
 * Colors (Borland 16-color palette)
 */
typedef enum {
	BGI_BLACK = 0,
	BGI_BLUE = 1,
	BGI_GREEN = 2,
	BGI_CYAN = 3,
	BGI_RED = 4,
	BGI_MAGENTA = 5,
	BGI_BROWN = 6,
	BGI_LIGHTGRAY = 7,
	BGI_DARKGRAY = 8,
	BGI_LIGHTBLUE = 9,
	BGI_LIGHTGREEN = 10,
	BGI_LIGHTCYAN = 11,
	BGI_LIGHTRED = 12,
	BGI_LIGHTMAGENTA = 13,
	BGI_YELLOW = 14,
	BGI_WHITE = 15
} bgi_color_t;

/*
 * Line Styles
 */
typedef enum {
	SOLID_LINE = 0,
	DOTTED_LINE = 1,
	CENTER_LINE = 2,
	DASHED_LINE = 3,
	USERBIT_LINE = 4
} bgi_line_style_t;

/*
 * Fill Patterns
 */
typedef enum {
	EMPTY_FILL = 0,
	SOLID_FILL = 1,
	LINE_FILL = 2,
	LTSLASH_FILL = 3,
	SLASH_FILL = 4,
	BKSLASH_FILL = 5,
	LTBKSLASH_FILL = 6,
	HATCH_FILL = 7,
	XHATCH_FILL = 8,
	INTERLEAVE_FILL = 9,
	WIDE_DOT_FILL = 10,
	CLOSE_DOT_FILL = 11,
	USER_FILL = 12
} bgi_fill_pattern_t;

/*
 * Text Directions
 */
typedef enum {
	HORIZ_DIR = 0,
	VERT_DIR = 1
} bgi_text_direction_t;

/*
 * Text Justification
 */
typedef enum {
	LEFT_TEXT = 0,
	CENTER_TEXT = 1,
	RIGHT_TEXT = 2,
	BOTTOM_TEXT = 0,
	TOP_TEXT = 2
} bgi_text_just_t;

/*
 * Fonts
 */
typedef enum {
	DEFAULT_FONT = 0,
	TRIPLEX_FONT = 1,
	SMALL_FONT = 2,
	SANSSERIF_FONT = 3,
	GOTHIC_FONT = 4,
	SCRIPT_FONT = 5,
	SIMPLEX_FONT = 6,
	TRIPLEX_SCR_FONT = 7,
	COMPLEX_FONT = 8,
	EUROPEAN_FONT = 9,
	BOLD_FONT = 10
} bgi_font_t;

/*
 * Write Modes
 */
typedef enum {
	COPY_PUT = 0,
	XOR_PUT = 1,
	OR_PUT = 2,
	AND_PUT = 3,
	NOT_PUT = 4
} bgi_write_mode_t;

/*
 * Initialization
 */

/* Initialize graphics */
void bgi_initgraph(bgi_driver_t *driver, bgi_mode_t *mode, const char *path);

/* Detect graphics driver */
void bgi_detectgraph(bgi_driver_t *driver, bgi_mode_t *mode);

/* Close graphics */
void bgi_closegraph(void);

/* Register BGI driver */
int bgi_registerbgidriver(void (*driver)(void));

/* Register BGI font */
int bgi_registerbgifont(void (*font)(void));

/* Set graphics mode */
void bgi_setgraphmode(bgi_mode_t mode);

/* Get graphics mode */
bgi_mode_t bgi_getgraphmode(void);

/* Restore CRT mode */
void bgi_restorecrtmode(void);

/*
 * Screen Information
 */

/* Get maximum X coordinate */
int bgi_getmaxx(void);

/* Get maximum Y coordinate */
int bgi_getmaxy(void);

/* Get maximum color value */
int bgi_getmaxcolor(void);

/* Get driver name */
char *bgi_getdrivername(void);

/* Get mode name */
char *bgi_getmodename(bgi_mode_t mode);

/* Get graphics error */
int bgi_graphresult(void);

/* Get error message */
char *bgi_grapherrormsg(int errorcode);

/*
 * Drawing Primitives
 */

/* Clear screen */
void bgi_cleardevice(void);

/* Clear viewport */
void bgi_clearviewport(void);

/* Put pixel */
void bgi_putpixel(int x, int y, int color);

/* Get pixel */
int bgi_getpixel(int x, int y);

/* Draw line */
void bgi_line(int x1, int y1, int x2, int y2);

/* Draw line to */
void bgi_lineto(int x, int y);

/* Draw line relative */
void bgi_linerel(int dx, int dy);

/* Move to */
void bgi_moveto(int x, int y);

/* Move relative */
void bgi_moverel(int dx, int dy);

/* Get current position */
int bgi_getx(void);
int bgi_gety(void);

/* Draw rectangle */
void bgi_rectangle(int left, int top, int right, int bottom);

/* Draw filled rectangle */
void bgi_bar(int left, int top, int right, int bottom);

/* Draw 3D bar */
void bgi_bar3d(int left, int top, int right, int bottom, int depth, int topflag);

/* Draw circle */
void bgi_circle(int x, int y, int radius);

/* Draw arc */
void bgi_arc(int x, int y, int startangle, int endangle, int radius);

/* Draw ellipse */
void bgi_ellipse(int x, int y, int startangle, int endangle, int xradius, int yradius);

/* Fill ellipse */
void bgi_fillellipse(int x, int y, int xradius, int yradius);

/* Draw pie slice */
void bgi_pieslice(int x, int y, int startangle, int endangle, int radius);

/* Draw sector */
void bgi_sector(int x, int y, int startangle, int endangle, int xradius, int yradius);

/* Draw polygon */
void bgi_drawpoly(int numpoints, int *polypoints);

/* Fill polygon */
void bgi_fillpoly(int numpoints, int *polypoints);

/* Flood fill */
void bgi_floodfill(int x, int y, int border);

/*
 * Color and Palette
 */

/* Set color */
void bgi_setcolor(int color);

/* Get color */
int bgi_getcolor(void);

/* Set background color */
void bgi_setbkcolor(int color);

/* Get background color */
int bgi_getbkcolor(void);

/* Set palette */
void bgi_setpalette(int index, int color);

/* Set all palette */
typedef struct {
	uint8_t size;
	int colors[256];
} bgi_palette_t;

void bgi_setallpalette(const bgi_palette_t *palette);

/* Get palette */
void bgi_getpalette(bgi_palette_t *palette);

/* Get default palette */
void bgi_getdefaultpalette(bgi_palette_t *palette);

/* Get palette size */
int bgi_getpalettesize(void);

/* Set RGB palette (256-color modes) */
void bgi_setrgbpalette(int index, int red, int green, int blue);

/*
 * Line and Fill Styles
 */

/* Set line style */
void bgi_setlinestyle(bgi_line_style_t style, unsigned pattern, int thickness);

/* Get line settings */
typedef struct {
	bgi_line_style_t linestyle;
	unsigned upattern;
	int thickness;
} bgi_linesettings_t;

void bgi_getlinesettings(bgi_linesettings_t *settings);

/* Set fill style */
void bgi_setfillstyle(bgi_fill_pattern_t pattern, int color);

/* Set fill pattern */
void bgi_setfillpattern(const uint8_t *upattern, int color);

/* Get fill settings */
typedef struct {
	bgi_fill_pattern_t pattern;
	int color;
} bgi_fillsettings_t;

void bgi_getfillsettings(bgi_fillsettings_t *settings);

/*
 * Text Output
 */

/* Output text */
void bgi_outtext(const char *text);

/* Output text at position */
void bgi_outtextxy(int x, int y, const char *text);

/* Set text style */
void bgi_settextstyle(bgi_font_t font, bgi_text_direction_t direction, int charsize);

/* Set user character size */
void bgi_setusercharsize(int multx, int divx, int multy, int divy);

/* Get text settings */
typedef struct {
	bgi_font_t font;
	bgi_text_direction_t direction;
	int charsize;
	int horiz;
	int vert;
} bgi_textsettings_t;

void bgi_gettextsettings(bgi_textsettings_t *settings);

/* Set text justification */
void bgi_settextjustify(int horiz, int vert);

/* Get text width */
int bgi_textwidth(const char *text);

/* Get text height */
int bgi_textheight(const char *text);

/*
 * Viewport and Clipping
 */

/* Set viewport */
void bgi_setviewport(int left, int top, int right, int bottom, int clip);

/* Get viewport settings */
typedef struct {
	int left;
	int top;
	int right;
	int bottom;
	int clip;
} bgi_viewportsettings_t;

void bgi_getviewsettings(bgi_viewportsettings_t *settings);

/*
 * Image Manipulation
 */

/* Get image size */
unsigned bgi_imagesize(int left, int top, int right, int bottom);

/* Get image */
void bgi_getimage(int left, int top, int right, int bottom, void *bitmap);

/* Put image */
void bgi_putimage(int left, int top, void *bitmap, int op);

/*
 * Write Mode
 */

/* Set write mode */
void bgi_setwritemode(bgi_write_mode_t mode);

/*
 * Microsoft GRAPH Compatibility
 */

/* Set video mode */
short bgi_setvideomode(short mode);

/* Get video config */
typedef struct {
	short numxpixels;
	short numypixels;
	short numtextcols;
	short numtextrows;
	short numcolors;
	short bitsperpixel;
	short numvideopages;
	short mode;
	short adapter;
	short monitor;
	short memory;
} bgi_videoconfig_t;

void bgi_getvideoconfig(bgi_videoconfig_t *config);

/* Set active page */
void bgi_setactivepage(short page);

/* Set visual page */
void bgi_setvisualpage(short page);

/* Get current position (Microsoft) */
typedef struct {
	short row;
	short col;
} bgi_rccoord_t;

bgi_rccoord_t bgi_getcurrentposition(void);

/* Set pixel (Microsoft) */
short bgi_setpixel_m(short x, short y);

/* Get pixel (Microsoft) */
short bgi_getpixel_m(short x, short y);

/* Move to (Microsoft) */
bgi_rccoord_t bgi_moveto_m(short x, short y);

/* Line to (Microsoft) */
void bgi_lineto_m(short x, short y);

/* Rectangle (Microsoft) */
void bgi_rectangle_m(short control, short x1, short y1, short x2, short y2);

/* Ellipse (Microsoft) */
void bgi_ellipse_m(short control, short x, short y, short xr, short yr);

/* Arc (Microsoft) */
void bgi_arc_m(short x, short y, short xstart, short ystart, short xend, short yend);

/* Pie (Microsoft) */
void bgi_pie_m(short control, short x, short y, short xstart, short ystart, short xend, short yend);

/* Flood fill (Microsoft) */
void bgi_floodfill_m(short x, short y, short boundary);

/*
 * Microsoft PGCHART (Presentation Graphics)
 */

/* Chart types */
typedef enum {
	PG_COLUMNCHART = 1,
	PG_BARCHART = 2,
	PG_LINECHART = 3,
	PG_SCATTERCHART = 4,
	PG_PIECHART = 5
} pg_charttype_t;

/* Chart styles */
typedef enum {
	PG_PLAINBARS = 1,
	PG_STACKEDBARS = 2,
	PG_PERCENTBARS = 3
} pg_chartstyle_t;

/* Legend */
typedef struct {
	short legend;
	short place;
	char **legends;
} pg_legend_t;

/* Axis */
typedef struct {
	short grid;
	short gridstyle;
	short ticinterval;
	short ticformat;
	short scalefactor;
	double scalemin;
	double scalemax;
	char *axistitle;
} pg_axis_t;

/* Chart environment */
typedef struct {
	short charttype;
	short chartstyle;
	char *maintitle;
	char *subtitle;
	char *xaxis;
	char *yaxis;
	pg_legend_t legend;
	pg_axis_t xaxis_def;
	pg_axis_t yaxis_def;
	short datawindow[4];
} pg_chartenv_t;

/* Palette entry */
typedef struct {
	short color;
	short style;
	short fill;
	char *plotchar;
} pg_palentry_t;

/* Initialize chart system */
short pg_initchart(void);

/* Default chart */
short pg_defaultchart(pg_chartenv_t *env, short type, short style);

/* Chart */
short pg_chart(pg_chartenv_t *env, const char **categories, float *values, short n);

/* Chart scatter */
short pg_chartscatter(pg_chartenv_t *env, float *xvalues, float *yvalues, short n);

/* Chart pie */
short pg_chartpie(pg_chartenv_t *env, const char **categories, float *values,
                  short *explode, short n);

/* Get palette */
short pg_getpalette(pg_palentry_t *palette);

/* Set palette */
short pg_setpalette(const pg_palentry_t *palette);

/* Reset palette */
short pg_resetpalette(void);

/*
 * SDL2 Backend Specific
 */

/* Get SDL window */
void *bgi_get_sdl_window(void);

/* Get SDL renderer */
void *bgi_get_sdl_renderer(void);

/* Set SDL window */
void bgi_set_sdl_window(void *window);

/* Set SDL renderer */
void bgi_set_sdl_renderer(void *renderer);

/* Process SDL events */
int bgi_process_events(void);

/* Wait for event */
int bgi_wait_event(void);

/*
 * Cleanup
 */

void bgi_cleanup(void);

#ifdef __cplusplus
}
#endif

#endif /* _PCC_BGI_H_ */
