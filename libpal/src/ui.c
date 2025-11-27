/*
 * Copyright (c) 2025 PCC Paradox PAL Runtime Library
 *
 * UI functions (message boxes, beep, etc.)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "../include/palrt.h"

/* Platform-specific UI implementations */
#ifdef _WIN32
#include <windows.h>

void pal_msginfo(const char *title, const char *message)
{
	MessageBoxA(NULL, message, title, MB_OK | MB_ICONINFORMATION);
}

void pal_msgwarning(const char *title, const char *message)
{
	MessageBoxA(NULL, message, title, MB_OK | MB_ICONWARNING);
}

void pal_msgerror(const char *title, const char *message)
{
	MessageBoxA(NULL, message, title, MB_OK | MB_ICONERROR);
}

PAL_Logical pal_msgquestion(const char *title, const char *message)
{
	int result = MessageBoxA(NULL, message, title, MB_YESNO | MB_ICONQUESTION);
	return (result == IDYES) ? PAL_TRUE : PAL_FALSE;
}

void pal_msgok(const char *title, const char *message)
{
	MessageBoxA(NULL, message, title, MB_OK);
}

PAL_Logical pal_msgyesno(const char *title, const char *message)
{
	int result = MessageBoxA(NULL, message, title, MB_YESNO);
	return (result == IDYES) ? PAL_TRUE : PAL_FALSE;
}

PAL_Logical pal_msgokcancel(const char *title, const char *message)
{
	int result = MessageBoxA(NULL, message, title, MB_OKCANCEL);
	return (result == IDOK) ? PAL_TRUE : PAL_FALSE;
}

void pal_beep(void)
{
	Beep(750, 300);
}

void pal_sleep(int32_t milliseconds)
{
	Sleep(milliseconds);
}

#else  /* Unix/Linux */

/* Use terminal-based UI for Unix */
static void print_box(const char *title, const char *message, const char *icon)
{
	int title_len = strlen(title);
	int msg_len = strlen(message);
	int width = (title_len > msg_len) ? title_len : msg_len;
	int i;

	if (width < 40)
		width = 40;

	/* Top border */
	printf("\n+");
	for (i = 0; i < width + 2; i++)
		printf("-");
	printf("+\n");

	/* Title */
	printf("| %s%s ", icon, title);
	for (i = strlen(title) + strlen(icon) + 1; i < width + 2; i++)
		printf(" ");
	printf("|\n");

	/* Separator */
	printf("|");
	for (i = 0; i < width + 2; i++)
		printf("-");
	printf("|\n");

	/* Message */
	printf("| %s", message);
	for (i = strlen(message); i < width + 1; i++)
		printf(" ");
	printf("|\n");

	/* Bottom border */
	printf("+");
	for (i = 0; i < width + 2; i++)
		printf("-");
	printf("+\n\n");
}

void pal_msginfo(const char *title, const char *message)
{
	print_box(title, message, "[i] ");
}

void pal_msgwarning(const char *title, const char *message)
{
	print_box(title, message, "[!] ");
}

void pal_msgerror(const char *title, const char *message)
{
	print_box(title, message, "[X] ");
}

PAL_Logical pal_msgquestion(const char *title, const char *message)
{
	char response[10];

	print_box(title, message, "[?] ");
	printf("(Y/N)? ");
	fflush(stdout);

	if (fgets(response, sizeof(response), stdin)) {
		if (response[0] == 'y' || response[0] == 'Y')
			return PAL_TRUE;
	}

	return PAL_FALSE;
}

void pal_msgok(const char *title, const char *message)
{
	char response[10];

	print_box(title, message, "    ");
	printf("Press Enter to continue...");
	fflush(stdout);
	fgets(response, sizeof(response), stdin);
}

PAL_Logical pal_msgyesno(const char *title, const char *message)
{
	return pal_msgquestion(title, message);
}

PAL_Logical pal_msgokcancel(const char *title, const char *message)
{
	char response[10];

	print_box(title, message, "[?] ");
	printf("(O)K/(C)ancel? ");
	fflush(stdout);

	if (fgets(response, sizeof(response), stdin)) {
		if (response[0] == 'o' || response[0] == 'O')
			return PAL_TRUE;
	}

	return PAL_FALSE;
}

void pal_beep(void)
{
	/* Terminal bell */
	printf("\a");
	fflush(stdout);
}

void pal_sleep(int32_t milliseconds)
{
	usleep(milliseconds * 1000);
}

#endif  /* _WIN32 */

void pal_wait(void)
{
	char buffer[10];
	printf("Press Enter to continue...");
	fflush(stdout);
	fgets(buffer, sizeof(buffer), stdin);
}
