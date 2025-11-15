/* DOS Console Implementation (uses BIOS/DOS interrupts) */
#ifdef __DOS__
#include <conio.h>
#include <dos.h>

void crt_init(void) { textmode(C80); }
void crt_done(void) {}
void crt_clrscr(void) { clrscr(); }
void crt_clreol(void) { clreol(); }
void crt_gotoxy(int x, int y) { gotoxy(x, y); }
int crt_wherex(void) { return wherex(); }
int crt_wherey(void) { return wherey(); }
void crt_textcolor(crt_color_t color) { textcolor(color); }
void crt_textbackground(crt_color_t color) { textbackground(color); }
int crt_kbhit(void) { return kbhit(); }
int crt_getch(void) { return getch(); }
void crt_delay(int milliseconds) { delay(milliseconds); }
#endif
