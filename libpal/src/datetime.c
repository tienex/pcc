/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * Date and time functions
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <time.h>
#include "../include/palrt.h"

PAL_Date pal_today(void)
{
	time_t now;
	struct tm *tm_info;
	PAL_Date date;

	now = time(NULL);
	tm_info = localtime(&now);

	date.year = tm_info->tm_year + 1900;
	date.month = tm_info->tm_mon + 1;
	date.day = tm_info->tm_mday;

	return date;
}

PAL_DateTime pal_now(void)
{
	time_t now;
	struct tm *tm_info;
	PAL_DateTime dt;

	now = time(NULL);
	tm_info = localtime(&now);

	dt.date.year = tm_info->tm_year + 1900;
	dt.date.month = tm_info->tm_mon + 1;
	dt.date.day = tm_info->tm_mday;

	dt.time.hour = tm_info->tm_hour;
	dt.time.minute = tm_info->tm_min;
	dt.time.second = tm_info->tm_sec;
	dt.time.millisecond = 0;

	return dt;
}

int32_t pal_year(PAL_Date date)
{
	return date.year;
}

int32_t pal_month(PAL_Date date)
{
	return date.month;
}

int32_t pal_day(PAL_Date date)
{
	return date.day;
}

int32_t pal_hour(PAL_Time time)
{
	return time.hour;
}

int32_t pal_minute(PAL_Time time)
{
	return time.minute;
}

int32_t pal_second(PAL_Time time)
{
	return time.second;
}

PAL_Date pal_date(int32_t year, int32_t month, int32_t day)
{
	PAL_Date date;

	/* Basic validation */
	if (year < 1900 || year > 3000)
		year = 1900;
	if (month < 1 || month > 12)
		month = 1;
	if (day < 1 || day > 31)
		day = 1;

	date.year = year;
	date.month = month;
	date.day = day;

	return date;
}

PAL_Time pal_time(int32_t hour, int32_t minute, int32_t second)
{
	PAL_Time time;

	/* Basic validation */
	if (hour < 0 || hour > 23)
		hour = 0;
	if (minute < 0 || minute > 59)
		minute = 0;
	if (second < 0 || second > 59)
		second = 0;

	time.hour = hour;
	time.minute = minute;
	time.second = second;
	time.millisecond = 0;

	return time;
}

PAL_DateTime pal_datetime(int32_t year, int32_t month, int32_t day,
                          int32_t hour, int32_t minute, int32_t second)
{
	PAL_DateTime dt;

	dt.date = pal_date(year, month, day);
	dt.time = pal_time(hour, minute, second);

	return dt;
}

PAL_String *pal_datetostr(PAL_Date date, const char *format)
{
	char buffer[128];
	struct tm tm_info = {0};

	tm_info.tm_year = date.year - 1900;
	tm_info.tm_mon = date.month - 1;
	tm_info.tm_mday = date.day;

	if (format && format[0] != '\0') {
		strftime(buffer, sizeof(buffer), format, &tm_info);
	} else {
		/* Default format: MM/DD/YYYY */
		snprintf(buffer, sizeof(buffer), "%02d/%02d/%04d",
		         date.month, date.day, date.year);
	}

	return pal_string_new(buffer);
}

PAL_Date pal_strtodate(const char *str, const char *format)
{
	PAL_Date date;
	int year, month, day;

	/* Simple MM/DD/YYYY parsing */
	if (sscanf(str, "%d/%d/%d", &month, &day, &year) == 3) {
		date = pal_date(year, month, day);
	} else {
		/* Return today on parse error */
		date = pal_today();
	}

	return date;
}

/* Convert date to days since epoch (Jan 1, 1900) */
static int32_t date_to_days(PAL_Date date)
{
	struct tm tm_info = {0};
	time_t t;

	tm_info.tm_year = date.year - 1900;
	tm_info.tm_mon = date.month - 1;
	tm_info.tm_mday = date.day;

	t = mktime(&tm_info);
	return (int32_t)(t / (24 * 60 * 60));
}

/* Convert days since epoch to date */
static PAL_Date days_to_date(int32_t days)
{
	time_t t;
	struct tm *tm_info;
	PAL_Date date;

	t = (time_t)days * 24 * 60 * 60;
	tm_info = localtime(&t);

	date.year = tm_info->tm_year + 1900;
	date.month = tm_info->tm_mon + 1;
	date.day = tm_info->tm_mday;

	return date;
}

PAL_Date pal_adddays(PAL_Date date, int32_t days)
{
	int32_t total_days;

	total_days = date_to_days(date) + days;
	return days_to_date(total_days);
}

int32_t pal_datediff(PAL_Date date1, PAL_Date date2)
{
	int32_t days1, days2;

	days1 = date_to_days(date1);
	days2 = date_to_days(date2);

	return days1 - days2;
}
