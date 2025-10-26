/*
 * Copyright (c) 2025 PCC Xbase++ Runtime Library
 *
 * Date/Time functions
 */

#include "xbrt.h"
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Convert Y/M/D to Julian day number */
int32_t xb_julian_from_ymd(int year, int month, int day) {
	int a = (14 - month) / 12;
	int y = year + 4800 - a;
	int m = month + 12 * a - 3;
	return day + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045;
}

/* Convert Julian day to Y/M/D */
void xb_ymd_from_julian(int32_t julian, int *year, int *month, int *day) {
	int a = julian + 32044;
	int b = (4 * a + 3) / 146097;
	int c = a - (146097 * b) / 4;
	int d = (4 * c + 3) / 1461;
	int e = c - (1461 * d) / 4;
	int m = (5 * e + 2) / 153;

	*day = e - (153 * m + 2) / 5 + 1;
	*month = m + 3 - 12 * (m / 10);
	*year = 100 * b + d - 4800 + m / 10;
}

/* DATE() - Current date as Julian */
int32_t xb_date(void) {
	time_t now = time(NULL);
	struct tm *tm = localtime(&now);
	return xb_julian_from_ymd(tm->tm_year + 1900, tm->tm_mon + 1, tm->tm_mday);
}

/* YEAR() - Extract year from date */
int xb_year(int32_t date) {
	int y, m, d;
	xb_ymd_from_julian(date, &y, &m, &d);
	return y;
}

/* MONTH() - Extract month from date */
int xb_month(int32_t date) {
	int y, m, d;
	xb_ymd_from_julian(date, &y, &m, &d);
	return m;
}

/* DAY() - Extract day from date */
int xb_day(int32_t date) {
	int y, m, d;
	xb_ymd_from_julian(date, &y, &m, &d);
	return d;
}

/* DOW() - Day of week (1=Sunday) */
int xb_dow(int32_t date) {
	return (date % 7) + 1;
}

/* CDOW() - Day of week name */
char *xb_cdow(int32_t date) {
	static const char *days[] = {"Sunday", "Monday", "Tuesday", "Wednesday",
	                             "Thursday", "Friday", "Saturday"};
	int dow = xb_dow(date) - 1;
	return strdup(days[dow % 7]);
}

/* CMONTH() - Month name */
char *xb_cmonth(int32_t date) {
	static const char *months[] = {"January", "February", "March", "April",
	                               "May", "June", "July", "August",
	                               "September", "October", "November", "December"};
	int month = xb_month(date);
	return strdup(months[(month - 1) % 12]);
}

/* CTOD() - Convert string to date */
int32_t xb_ctod(const char *str) {
	if (!str) return 0;

	int m, d, y;
	if (sscanf(str, "%d/%d/%d", &m, &d, &y) == 3) {
		if (y < 100) y += (y < 50) ? 2000 : 1900;
		return xb_julian_from_ymd(y, m, d);
	}
	return 0;
}

/* DTOC() - Convert date to string (MM/DD/YYYY) */
char *xb_dtoc(int32_t date) {
	int y, m, d;
	char *result = malloc(11);
	xb_ymd_from_julian(date, &y, &m, &d);
	if (result)
		snprintf(result, 11, "%02d/%02d/%04d", m, d, y);
	return result ? result : strdup("");
}

/* DTOS() - Convert date to string (YYYYMMDD) */
char *xb_dtos(int32_t date) {
	int y, m, d;
	char *result = malloc(9);
	xb_ymd_from_julian(date, &y, &m, &d);
	if (result)
		snprintf(result, 9, "%04d%02d%02d", y, m, d);
	return result ? result : strdup("");
}

/* STOD() - Convert string (YYYYMMDD) to date */
int32_t xb_stod(const char *str) {
	if (!str || strlen(str) < 8) return 0;

	int y, m, d;
	if (sscanf(str, "%4d%2d%2d", &y, &m, &d) == 3)
		return xb_julian_from_ymd(y, m, d);
	return 0;
}

/* TRANSFORM() - Format value */
char *xb_transform(const xb_value_t *val, const char *format) {
	/* TODO: Implement proper format transformation */
	return xb_value_to_string(val);
}
