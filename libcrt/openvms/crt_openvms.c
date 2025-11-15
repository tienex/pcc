/* openvms Console Stub */
#include <stdio.h>
void crt_init(void) {}
void crt_done(void) {}
void crt_clrscr(void) { printf("\n"); }
void crt_gotoxy(int x, int y) { (void)x; (void)y; }
void crt_textcolor(crt_color_t color) { (void)color; }
int crt_kbhit(void) { return 0; }
int crt_getch(void) { return getchar(); }
void crt_delay(int milliseconds) { (void)milliseconds; }
