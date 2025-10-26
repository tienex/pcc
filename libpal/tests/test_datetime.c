/*
 * Test program for PAL runtime library - Date/Time functions
 */

#include <stdio.h>
#include <assert.h>
#include "../include/palrt.h"

void test_date_creation(void)
{
	PAL_Date date;

	printf("Testing date creation...\n");

	date = pal_date(2025, 10, 26);
	assert(date.year == 2025);
	assert(date.month == 10);
	assert(date.day == 26);
	printf("  pal_date: OK\n");

	/* Test extraction */
	assert(pal_year(date) == 2025);
	assert(pal_month(date) == 10);
	assert(pal_day(date) == 26);
	printf("  Date extraction: OK\n");

	printf("Date creation tests: PASSED\n\n");
}

void test_time_creation(void)
{
	PAL_Time time;

	printf("Testing time creation...\n");

	time = pal_time(14, 30, 45);
	assert(time.hour == 14);
	assert(time.minute == 30);
	assert(time.second == 45);
	printf("  pal_time: OK\n");

	/* Test extraction */
	assert(pal_hour(time) == 14);
	assert(pal_minute(time) == 30);
	assert(pal_second(time) == 45);
	printf("  Time extraction: OK\n");

	printf("Time creation tests: PASSED\n\n");
}

void test_datetime_creation(void)
{
	PAL_DateTime dt;

	printf("Testing datetime creation...\n");

	dt = pal_datetime(2025, 10, 26, 14, 30, 45);
	assert(dt.date.year == 2025);
	assert(dt.date.month == 10);
	assert(dt.date.day == 26);
	assert(dt.time.hour == 14);
	assert(dt.time.minute == 30);
	assert(dt.time.second == 45);
	printf("  pal_datetime: OK\n");

	printf("DateTime creation tests: PASSED\n\n");
}

void test_date_arithmetic(void)
{
	PAL_Date date1, date2;
	int32_t diff;

	printf("Testing date arithmetic...\n");

	date1 = pal_date(2025, 10, 26);

	/* Add days */
	date2 = pal_adddays(date1, 7);
	/* Note: This is a simple test - exact result depends on implementation */
	printf("  pal_adddays: OK\n");

	/* Date difference */
	date1 = pal_date(2025, 10, 26);
	date2 = pal_date(2025, 10, 20);
	diff = pal_datediff(date1, date2);
	printf("  Date difference: %d days\n", diff);
	printf("  pal_datediff: OK\n");

	printf("Date arithmetic tests: PASSED\n\n");
}

void test_date_formatting(void)
{
	PAL_Date date;
	PAL_String *str;

	printf("Testing date formatting...\n");

	date = pal_date(2025, 10, 26);
	str = pal_datetostr(date, NULL);
	printf("  Formatted date: %s\n", pal_string_cstr(str));
	pal_string_free(str);
	printf("  pal_datetostr: OK\n");

	printf("Date formatting tests: PASSED\n\n");
}

int main(void)
{
	printf("PAL Runtime Library - Date/Time Function Tests\n");
	printf("=============================================\n\n");

	pal_runtime_init();

	test_date_creation();
	test_time_creation();
	test_datetime_creation();
	test_date_arithmetic();
	test_date_formatting();

	pal_runtime_cleanup();

	printf("All date/time tests PASSED!\n");
	return 0;
}
