/*
 * Copyright (c) 2025 PCC Project
 * BGI Font Driver Stubs
 */

#include "../../bgi_driver.h"

/* Stub implementations for other fonts */
/* These would contain full bitmap data in production */

bgi_font_driver_t bgi_font_8x16 = {
	.name = "8x16",
	.id = 1
};

bgi_font_driver_t bgi_font_triplex = {
	.name = "triplex",
	.id = 2
};

bgi_font_driver_t bgi_font_small = {
	.name = "small",
	.id = 3
};

bgi_font_driver_t bgi_font_sansserif = {
	.name = "sans-serif",
	.id = 4
};

bgi_font_driver_t bgi_font_gothic = {
	.name = "gothic",
	.id = 5
};
