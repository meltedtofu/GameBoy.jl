/* Copyright (C) 2014-2018 Thomas Spurden <thomas@spurden.name>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

/* Enable use of minizip library to load ROMs directly from zip files */
#ifndef WANT_MINIZIP
#define WANT_MINIZIP 0
#endif

#include <string.h>
#include <strings.h>
#include <stdio.h>
#include "gameboy.h"

#if WANT_MINIZIP
#include <minizip/unzip.h>
#endif

static int SaveRAM(struct Gameboy* gb, char const* filename)
{
	FILE* file = fopen(filename, "wb");
	if(!file) {
		return 1;
	}
	if(fwrite(gb->mem.CartRAM, 1, gb->mem.CartRAMSize, file) != gb->mem.CartRAMSize) {
		fclose(file);
		return 1;
	}
	fclose(file);
	return 0;
}

static int LoadRAM(struct Gameboy* gb, char const* filename)
{
	FILE* file = fopen(filename, "rb");
	if(!file) {
		return 1;
	}
	if(fread(gb->mem.CartRAM, 1, gb->mem.CartRAMSize, file) != gb->mem.CartRAMSize) {
		fclose(file);
		return 1;
	}
	fclose(file);
	return 0;
}

