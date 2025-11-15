/* Unix/POSIX Terminal Implementation (ANSI escape codes) */
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>

static struct crt_state_struct {
	int use_ansi;
	crt_color_t text_color;
	crt_color_t back_color;
	struct termios orig_termios;
} crt_state = { .use_ansi = 1, .text_color = CRT_LIGHTGRAY, .back_color = CRT_BLACK };

void crt_init(void) {
	tcgetattr(STDIN_FILENO, &crt_state.orig_termios);
}

void crt_done(void) {
	tcsetattr(STDIN_FILENO, TCSANOW, &crt_state.orig_termios);
}

void crt_clrscr(void) { printf("\033[2J\033[H"); fflush(stdout); }
void crt_clreol(void) { printf("\033[K"); fflush(stdout); }

void crt_gotoxy(int x, int y) { printf("\033[%d;%dH", y, x); fflush(stdout); }

int crt_wherex(void) { return 1; } /* Simplified */
int crt_wherey(void) { return 1; }

void crt_textcolor(crt_color_t color) {
	crt_state.text_color = color;
	int ansi_colors[] = {30,34,32,36,31,35,33,37,90,94,92,96,91,95,93,97};
	printf("\033[%dm", ansi_colors[color & 15]);
	fflush(stdout);
}

void crt_textbackground(crt_color_t color) {
	crt_state.back_color = color;
	int ansi_colors[] = {40,44,42,46,41,45,43,47,100,104,102,106,101,105,103,107};
	printf("\033[%dm", ansi_colors[color & 15]);
	fflush(stdout);
}

int crt_kbhit(void) {
	struct termios term;
	tcgetattr(STDIN_FILENO, &term);
	term.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &term);
	
	int bytes_waiting;
	ioctl(STDIN_FILENO, FIONREAD, &bytes_waiting);
	
	tcsetattr(STDIN_FILENO, TCSANOW, &crt_state.orig_termios);
	return bytes_waiting > 0;
}

int crt_getch(void) {
	struct termios term;
	tcgetattr(STDIN_FILENO, &term);
	term.c_lflag &= ~(ICANON | ECHO);
	tcsetattr(STDIN_FILENO, TCSANOW, &term);
	
	int c = getchar();
	
	tcsetattr(STDIN_FILENO, TCSANOW, &crt_state.orig_termios);
	return c;
}

void crt_delay(int milliseconds) { usleep(milliseconds * 1000); }
