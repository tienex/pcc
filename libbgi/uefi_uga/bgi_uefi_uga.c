/*	$Id$	*/
/*
 * Copyright (c) 2025 PCC Project.
 * BGI Implementation using UEFI UGA (Universal Graphics Adapter)
 *
 * Legacy UEFI graphics protocol (pre-GOP)
 * Works on: UEFI 1.x firmware
 * Features: Framebuffer access, legacy UEFI support
 * Note: Replaced by GOP in UEFI 2.x, but still supported
 */

#include "../graphics.h"
#include <efi.h>
#include <efilib.h>

static struct {
	EFI_UGA_DRAW_PROTOCOL *uga;
	UINT32 width, height;
	int current_color, current_bkcolor;
	int current_x, current_y;
	int line_style;
	unsigned short line_pattern;
	int fill_style, fill_color;
	int text_justify_h, text_justify_v;
	EFI_UGA_PIXEL colors[16];
	int initialized;
} uga_state;

static const EFI_UGA_PIXEL ega_uga[16] = {
	{0x00, 0x00, 0x00, 0x00}, {0xAA, 0x00, 0x00, 0x00},
	{0x00, 0xAA, 0x00, 0x00}, {0xAA, 0xAA, 0x00, 0x00},
	{0x00, 0x00, 0xAA, 0x00}, {0xAA, 0x00, 0xAA, 0x00},
	{0x00, 0x55, 0xAA, 0x00}, {0xAA, 0xAA, 0xAA, 0x00},
	{0x55, 0x55, 0x55, 0x00}, {0xFF, 0x55, 0x55, 0x00},
	{0x55, 0xFF, 0x55, 0x00}, {0xFF, 0xFF, 0x55, 0x00},
	{0x55, 0x55, 0xFF, 0x00}, {0xFF, 0x55, 0xFF, 0x00},
	{0x55, 0xFF, 0xFF, 0x00}, {0xFF, 0xFF, 0xFF, 0x00}
};

extern EFI_SYSTEM_TABLE *ST;
extern EFI_BOOT_SERVICES *BS;

void initgraph(int *driver, int *mode, const char *path) {
	EFI_STATUS status;
	EFI_GUID uga_guid = EFI_UGA_DRAW_PROTOCOL_GUID;
	UINT32 width = 640, height = 480, depth, refresh, i;

	if (*driver == DETECT) { *driver = VGA; *mode = VGAHI; }
	switch (*mode) {
	case VGAHI: width = 640; height = 480; break;
	case SVGA_800_600: width = 800; height = 600; break;
	case SVGA_1024_768: width = 1024; height = 768; break;
	}

	status = BS->LocateProtocol(&uga_guid, NULL, (void **)&uga_state.uga);
	if (EFI_ERROR(status)) {
		*driver = grNotDetected;
		return;
	}

	status = uga_state.uga->SetMode(uga_state.uga, width, height, 32, 60);
	if (EFI_ERROR(status)) {
		*driver = grNotDetected;
		return;
	}

	uga_state.uga->GetMode(uga_state.uga, &uga_state.width, &uga_state.height, &depth, &refresh);
	for (i = 0; i < 16; i++) uga_state.colors[i] = ega_uga[i];

	uga_state.current_color = WHITE;
	uga_state.current_bkcolor = BLACK;
	uga_state.line_style = SOLID_LINE;
	uga_state.line_pattern = 0xFFFF;
	uga_state.initialized = 1;

	cleardevice();
	*driver = 0; *mode = 0;
}

void closegraph(void) {
	memset(&uga_state, 0, sizeof(uga_state));
}

int getmaxx(void) { return uga_state.width - 1; }
int getmaxy(void) { return uga_state.height - 1; }
void setcolor(int color) { uga_state.current_color = color & 0x0F; }
int getcolor(void) { return uga_state.current_color; }

void cleardevice(void) {
	uga_state.uga->Blt(uga_state.uga, &uga_state.colors[uga_state.current_bkcolor],
	                    EfiUgaVideoFill, 0, 0, 0, 0, uga_state.width, uga_state.height, 0);
}

void putpixel(int x, int y, int color) {
	if (x >= 0 && x < (int)uga_state.width && y >= 0 && y < (int)uga_state.height)
		uga_state.uga->Blt(uga_state.uga, &uga_state.colors[color & 0x0F],
		                    EfiUgaVideoFill, 0, 0, x, y, 1, 1, 0);
}

const char *grapherrormsg(int errorcode) {
	switch (errorcode) {
	case grOk: return "No error";
	case grNoInitGraph: return "Graphics not initialized";
	default: return "Unknown error";
	}
}

int graphresult(void) { return uga_state.initialized ? grOk : grNoInitGraph; }
