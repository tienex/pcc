/* Win32 Console Implementation */
#ifdef _WIN32
#include <windows.h>
#include <conio.h>

static struct { HANDLE h_stdout; CONSOLE_SCREEN_BUFFER_INFO csbi; int text_color; } crt_state;

void crt_init(void) {
	crt_state.h_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &crt_state.csbi);
	crt_state.text_color = LIGHTGRAY;
}

void crt_done(void) {}

void crt_clrscr(void) {
	COORD coordScreen = {0, 0};
	DWORD cCharsWritten;
	CONSOLE_SCREEN_BUFFER_INFO csbi;
	GetConsoleScreenBufferInfo(crt_state.h_stdout, &csbi);
	DWORD dwConSize = csbi.dwSize.X * csbi.dwSize.Y;
	FillConsoleOutputCharacter(crt_state.h_stdout, ' ', dwConSize, coordScreen, &cCharsWritten);
	SetConsoleCursorPosition(crt_state.h_stdout, coordScreen);
}

void crt_gotoxy(int x, int y) {
	COORD coord = {x - 1, y - 1};
	SetConsoleCursorPosition(crt_state.h_stdout, coord);
}

void crt_textcolor(crt_color_t color) {
	crt_state.text_color = color;
	SetConsoleTextAttribute(crt_state.h_stdout, color);
}

int crt_kbhit(void) { return _kbhit(); }
int crt_getch(void) { return _getch(); }
void crt_delay(int milliseconds) { Sleep(milliseconds); }
#endif
