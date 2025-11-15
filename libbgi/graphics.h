/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Complete BGI (Borland Graphics Interface) API Header
 *
 * Full implementation of the Borland Graphics Interface
 * Compatible with Turbo C/C++ BGI specification
 */

#ifndef _GRAPHICS_H
#define _GRAPHICS_H

/* Graphics drivers */
#define DETECT          0
#define CGA             1
#define MCGA            2
#define EGA             3
#define EGA64           4
#define EGAMONO         5
#define IBM8514         6
#define HERCMONO        7
#define ATT400          8
#define VGA             9
#define PC3270          10

/* Graphics modes */
#define CGAC0           0
#define CGAC1           1
#define CGAC2           2
#define CGAC3           3
#define CGAHI           4
#define MCGAC0          0
#define MCGAC1          1
#define MCGAC2          2
#define MCGAC3          3
#define MCGAMED         4
#define MCGAHI          5
#define EGALO           0
#define EGAHI           1
#define EGA64LO         0
#define EGA64HI         1
#define EGAMONOHI       3
#define HERCMONOHI      0
#define ATT400C0        0
#define ATT400C1        1
#define ATT400C2        2
#define ATT400C3        3
#define ATT400MED       4
#define ATT400HI        5
#define VGALO           0
#define VGAMED          1
#define VGAHI           2
#define PC3270HI        0
#define IBM8514LO       0
#define IBM8514HI       1

/* Extended modes for modern backends */
#define SVGA_640_400    0x100
#define SVGA_640_480    0x101
#define SVGA_800_600    0x102
#define SVGA_1024_768   0x103
#define SVGA_1280_1024  0x104

/* Colors */
#define BLACK           0
#define BLUE            1
#define GREEN           2
#define CYAN            3
#define RED             4
#define MAGENTA         5
#define BROWN           6
#define LIGHTGRAY       7
#define DARKGRAY        8
#define LIGHTBLUE       9
#define LIGHTGREEN      10
#define LIGHTCYAN       11
#define LIGHTRED        12
#define LIGHTMAGENTA    13
#define YELLOW          14
#define WHITE           15

/* Line styles */
#define SOLID_LINE      0
#define DOTTED_LINE     1
#define CENTER_LINE     2
#define DASHED_LINE     3
#define USERBIT_LINE    4

/* Line thickness */
#define NORM_WIDTH      1
#define THICK_WIDTH     3

/* Fill patterns */
#define EMPTY_FILL      0
#define SOLID_FILL      1
#define LINE_FILL       2
#define LTSLASH_FILL    3
#define SLASH_FILL      4
#define BKSLASH_FILL    5
#define LTBKSLASH_FILL  6
#define HATCH_FILL      7
#define XHATCH_FILL     8
#define INTERLEAVE_FILL 9
#define WIDE_DOT_FILL   10
#define CLOSE_DOT_FILL  11
#define USER_FILL       12

/* Text alignment */
#define LEFT_TEXT       0
#define CENTER_TEXT     1
#define RIGHT_TEXT      2
#define BOTTOM_TEXT     0
#define TOP_TEXT        2

/* Text direction */
#define HORIZ_DIR       0
#define VERT_DIR        1

/* Font types */
#define DEFAULT_FONT    0
#define TRIPLEX_FONT    1
#define SMALL_FONT      2
#define SANS_SERIF_FONT 3
#define GOTHIC_FONT     4

/* Character sizes */
#define USER_CHAR_SIZE  0

/* Error codes */
#define grOk                0
#define grNoInitGraph      -1
#define grNotDetected      -2
#define grFileNotFound     -3
#define grInvalidDriver    -4
#define grNoLoadMem        -5
#define grNoScanMem        -6
#define grNoFloodMem       -7
#define grFontNotFound     -8
#define grNoFontMem        -9
#define grInvalidMode     -10
#define grError           -11
#define grIOerror         -12
#define grInvalidFont     -13
#define grInvalidFontNum  -14
#define grInvalidVersion  -18

/* Structures */
struct palettetype {
	unsigned char size;
	signed char colors[16];
};

struct linesettingstype {
	int linestyle;
	unsigned upattern;
	int thickness;
};

struct textsettingstype {
	int font;
	int direction;
	int charsize;
	int horiz;
	int vert;
};

struct fillsettingstype {
	int pattern;
	int color;
};

struct pointtype {
	int x;
	int y;
};

struct viewporttype {
	int left;
	int top;
	int right;
	int bottom;
	int clip;
};

struct arccoordstype {
	int x;
	int y;
	int xstart;
	int ystart;
	int xend;
	int yend;
};

/* Core graphics functions */
void initgraph(int *graphdriver, int *graphmode, const char *pathtodriver);
void closegraph(void);
int graphresult(void);
const char *grapherrormsg(int errorcode);

/* Screen control */
void cleardevice(void);
void clearviewport(void);
void setactivepage(int page);
void setvisualpage(int page);

/* Coordinate functions */
int getmaxx(void);
int getmaxy(void);
int getx(void);
int gety(void);
void getviewsettings(struct viewporttype *viewport);
void setviewport(int left, int top, int right, int bottom, int clip);

/* Drawing functions */
void moveto(int x, int y);
void moverel(int dx, int dy);
void lineto(int x, int y);
void linerel(int dx, int dy);
void line(int x1, int y1, int x2, int y2);
void rectangle(int left, int top, int right, int bottom);
void circle(int x, int y, int radius);
void arc(int x, int y, int stangle, int endangle, int radius);
void ellipse(int x, int y, int stangle, int endangle, int xradius, int yradius);
void drawpoly(int numpoints, const int *polypoints);
void fillpoly(int numpoints, const int *polypoints);
void fillellipse(int x, int y, int xradius, int yradius);
void sector(int x, int y, int stangle, int endangle, int xradius, int yradius);
void pieslice(int x, int y, int stangle, int endangle, int radius);
void bar(int left, int top, int right, int bottom);
void bar3d(int left, int top, int right, int bottom, int depth, int topflag);

/* Pixel functions */
void putpixel(int x, int y, int color);
unsigned int getpixel(int x, int y);

/* Fill functions */
void floodfill(int x, int y, int border);
void setfillstyle(int pattern, int color);
void setfillpattern(const char *upattern, int color);
void getfillsettings(struct fillsettingstype *fillinfo);
void getfillpattern(char *pattern);

/* Line style functions */
void setlinestyle(int linestyle, unsigned pattern, int thickness);
void getlinesettings(struct linesettingstype *lineinfo);

/* Color functions */
void setcolor(int color);
int getcolor(void);
void setbkcolor(int color);
int getbkcolor(void);
void setpalette(int colornum, int color);
void setallpalette(const struct palettetype *palette);
void getpalette(struct palettetype *palette);
int getpalettesize(void);
void getdefaultpalette(struct palettetype *palette);

/* Text functions */
void outtext(const char *textstring);
void outtextxy(int x, int y, const char *textstring);
void settextjustify(int horiz, int vert);
void settextstyle(int font, int direction, int charsize);
void setusercharsize(int multx, int divx, int multy, int divy);
void gettextsettings(struct textsettingstype *texttypeinfo);
int textheight(const char *textstring);
int textwidth(const char *textstring);

/* Image functions */
unsigned int imagesize(int left, int top, int right, int bottom);
void getimage(int left, int top, int right, int bottom, void *bitmap);
void putimage(int left, int top, const void *bitmap, int op);

/* Arc coordinate functions */
void getarccoords(struct arccoordstype *arccoords);

/* Aspect ratio */
void getaspectratio(int *xasp, int *yasp);
void setaspectratio(int xasp, int yasp);

/* Graphics mode info */
int getgraphmode(void);
void setgraphmode(int mode);
const char *getdrivername(void);
const char *getmodename(int mode);
void getmoderange(int graphdriver, int *lomode, int *himode);

/* Write mode */
void setwritemode(int mode);

/* Additional BGI-compatible functions */
int installuserdriver(const char *name, int (*detect)(void));
int installuserfont(const char *name);
int registerbgidriver(void (*driver)(void));
int registerbgifont(void (*font)(void));

#endif /* _GRAPHICS_H */
