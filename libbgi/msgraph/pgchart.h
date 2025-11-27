/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * Microsoft Presentation Graphics Library (pgchart.h) Compatibility
 *
 * Compatible with Microsoft C 6.0+, Visual C++ 1.x presentation graphics
 * Provides high-level charting functions for business graphics
 */

#ifndef PGCHART_H
#define PGCHART_H

#include "graph_compat.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Chart types */
#define _PG_BARCHART		1	/* Vertical bar chart */
#define _PG_COLUMNCHART		2	/* Horizontal bar chart */
#define _PG_LINECHART		3	/* Line chart */
#define _PG_SCATTERCHART	4	/* Scatter plot */
#define _PG_PIECHART		5	/* Pie chart */

/* Chart styles */
#define _PG_PLAINBARS		1	/* Plain bars */
#define _PG_STACKEDBARS		2	/* Stacked bars */
#define _PG_PERCENTBARS		3	/* Percentage bars */

/* Legend options */
#define _PG_RIGHT		1	/* Legend on right */
#define _PG_BOTTOM		2	/* Legend on bottom */
#define _PG_OVERLAY		3	/* Legend overlaid on chart */

/* Palette entries */
#define _PG_COLORS		16	/* Number of palette entries */

/* Maximum values */
#define _PG_MAXSERIES		16	/* Maximum data series */
#define _PG_MAXCATEGORIES	32	/* Maximum categories */
#define _PG_TITLELEN		256	/* Maximum title length */

/* Palette definition */
typedef struct {
	unsigned short color[_PG_COLORS];	/* Color palette */
	unsigned short style[_PG_COLORS];	/* Fill patterns */
	char symbol[_PG_COLORS];		/* Point symbols */
} paletteentry;

/* Chart title definition */
typedef struct {
	char title[_PG_TITLELEN];	/* Title text */
	short titlecolor;		/* Title color */
	short justify;			/* Justification */
} titletype;

/* Axis definition */
typedef struct {
	short grid;			/* Grid on/off */
	short gridstyle;		/* Grid line style */
	titletype axistitle;		/* Axis title */
	short axiscolor;		/* Axis color */
	short labeled;			/* Labels on/off */
	short rangetype;		/* Auto/manual range */
	double logbase;			/* Log scale base */
	short autoscale;		/* Auto-scaling on/off */
	double scalemin;		/* Scale minimum */
	double scalemax;		/* Scale maximum */
	double scalefactor;		/* Scale factor */
	titletype scaletitle;		/* Scale title */
	double ticinterval;		/* Tick interval */
	short ticformat;		/* Tick label format */
	short ticdecimals;		/* Tick decimals */
} axistype;

/* Window definition */
typedef struct {
	short x1, y1;			/* Upper-left corner */
	short x2, y2;			/* Lower-right corner */
	short border;			/* Border on/off */
	short background;		/* Background color */
	short borderstyle;		/* Border style */
	short bordercolor;		/* Border color */
} windowtype;

/* Legend definition */
typedef struct {
	short legend;			/* Legend on/off */
	short place;			/* Legend placement */
	short textcolor;		/* Text color */
	short autosize;			/* Auto-size legend */
	windowtype legendwindow;	/* Legend window */
} legendtype;

/* Chart environment (common settings) */
typedef struct {
	short charttype;		/* Chart type */
	short chartstyle;		/* Chart style */
	windowtype chartwindow;		/* Chart window */
	windowtype datawindow;		/* Data window */
	titletype maintitle;		/* Main title */
	titletype subtitle;		/* Subtitle */
	axistype xaxis;			/* X-axis definition */
	axistype yaxis;			/* Y-axis definition */
	legendtype legend;		/* Legend definition */
} chartenv;

/* Single-series data */
typedef struct {
	float *data;			/* Data array */
	short nseries;			/* Number of series (1) */
	short n;			/* Number of data points */
	short arraydim;			/* Array dimension */
	char **categories;		/* Category labels */
} chartseries;

/* Multi-series data */
typedef struct {
	float **data;			/* Data array [series][point] */
	short nseries;			/* Number of series */
	short n;			/* Number of data points per series */
	short arraydim;			/* Array dimension */
	char **categories;		/* Category labels */
	char **seriesnames;		/* Series names */
} chartseriesms;

/* Pie chart data */
typedef struct {
	float *data;			/* Data values */
	char **categories;		/* Category labels */
	short n;			/* Number of slices */
	short explode;			/* Exploded slice index */
	short percent;			/* Show percentages */
} piecharttype;

/* Scatter chart data */
typedef struct {
	float *xdata;			/* X coordinates */
	float *ydata;			/* Y coordinates */
	short n;			/* Number of points */
} scattertype;

/*
 * Chart initialization and configuration
 */
short _pg_initchart(void);
short _pg_defaultchart(chartenv *env, short type, short style);
short _pg_chart(chartenv *env, char **categories, float *values, short n);
short _pg_chartms(chartenv *env, char **categories, float **values,
                  short nseries, short n, short arraydim, char **seriesnames);
short _pg_chartpie(chartenv *env, char **categories, float *values,
                   short *explode, short n);
short _pg_chartscatter(chartenv *env, float *xvalues, float *yvalues, short n);
short _pg_chartscatterms(chartenv *env, float **xvalues, float **yvalues,
                         short nseries, short n, short arraydim, char **seriesnames);

/*
 * Palette functions
 */
short _pg_getpalette(paletteentry *palette);
short _pg_setpalette(paletteentry *palette);
short _pg_resetpalette(void);

/*
 * Chart window functions
 */
short _pg_getstyleset(unsigned short *styleset);
short _pg_setstyleset(unsigned short *styleset);
short _pg_resetstyleset(void);

/*
 * Analysis functions
 */
short _pg_analyzechart(chartenv *env, char **categories, float *values, short n);
short _pg_analyzechartms(chartenv *env, char **categories, float **values,
                         short nseries, short n, short arraydim);
short _pg_analyzepie(chartenv *env, char **categories, float *values, short n);
short _pg_analyzescatter(chartenv *env, float *xvalues, float *yvalues, short n);
short _pg_analyzescatterms(chartenv *env, float **xvalues, float **yvalues,
                           short nseries, short n, short arraydim);

/*
 * Utility functions
 */
short _pg_hlabelchart(chartenv *env, short x, short y, short color, char *label);
short _pg_vlabelchart(chartenv *env, short x, short y, short color, char *label);
short _pg_getchardef(short ch, unsigned char *def);
short _pg_setchardef(short ch, unsigned char *def);

#ifdef __cplusplus
}
#endif

#endif /* PGCHART_H */
