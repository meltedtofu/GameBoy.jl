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
#include "gameboy.h"
#include "utils.h"

#include <assert.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <stdio.h> // So that I can debug with terrible tooling...

enum IORegisters {
    /* Addresses relative to 0xFF00 */
    IO_Joypad = 0x00,
    IO_SerialData = 0x01,
    IO_SerialControl = 0x02,
    IO_Divider = 0x04,
    IO_TimerCounter = 0x05,
    IO_TimerModulo = 0x06,
    IO_TimerControl = 0x07,
    IO_InterruptFlag = 0x0F,

    IO_Sound1Sweep = 0x10,
    IO_Sound1Mode = 0x11,
    IO_Sound1Envelope = 0x12,
    IO_Sound1FreqLo = 0x13,
    IO_Sound1FreqHi = 0x14,

    IO_Sound2Mode = 0x16,
    IO_Sound2Envelope = 0x17,
    IO_Sound2FreqLo = 0x18,
    IO_Sound2FreqHi = 0x19,

    IO_Sound3Enable = 0x1A,
    IO_Sound3Length = 0x1B,
    IO_Sound3Level = 0x1C,
    IO_Sound3FreqLo = 0x1D,
    IO_Sound3FreqHi = 0x1E,

    IO_Sound4Length = 0x20,
    IO_Sound4Envelope = 0x21,
    IO_Sound4Poly = 0x22,
    IO_Sound4Counter = 0x23,

    IO_SoundChannels = 0x24,
    IO_SoundOutput = 0x25,
    IO_SoundControl = 0x26,

    /* 0x30 - 0x3F wave RAM */

    IO_LCDControl = 0x40,
    IO_LCDStat = 0x41,
    IO_ScrollY = 0x42,
    IO_ScrollX = 0x43,
    IO_LCDY = 0x44,
    IO_LCDYCompare = 0x45,
    IO_OAMDMA = 0x46,
    IO_BackgroundPalette = 0x47,
    IO_ObjectPalette0 = 0x48,
    IO_ObjectPalette1 = 0x49,
    IO_WindowY = 0x4A,
    IO_WindowX = 0x4B,

    IO_BootROMDisable = 0x50,
};
static uint8_t const IOUnusedBits[128] = {
    [IO_Joypad] = 0xC0,
    // ...
};
static void clock_countChange(struct Clock* clock, struct Memory* mem, uint16_t new_value);
static void dma_update(struct DMA*, struct Memory*);
void video_update(struct Gameboy* gb, uint8_t scanline);
void input_setUp(struct Buttons*, int button);
void input_setDown(struct Buttons*, struct Memory*, int button);

void clock_increment(struct Gameboy* gb)
{
    struct Clock* clock = &(gb->clock);
    struct Memory* mem = &(gb->mem);
    struct DMA* dma = &(gb->dma);
    struct LCD* lcd = &(gb->lcd);

    clock->TimerLoading = false;
    if(clock->TimerOverflow) {
        /* Delayed overflow effects */
        mem->IO[IO_InterruptFlag] |= Interrupt_TIMA;
        clock->TimerOverflow = false;

        /* In the next machine cycle the modulo is being loaded */
        mem->IO[IO_TimerCounter] = mem->IO[IO_TimerModulo];
        clock->TimerLoading = true;
    }
    clock_countChange(clock, mem, clock->CycleCount + 4);
    dma_update(dma, mem);
}

static bool clock_getTimerBit(uint8_t control, uint16_t cycles)
{
    switch(control & 0x03) { /* Timer clock select */
        case 0: /* 4.096 KHz (1024 cycles) */
            return cycles & (1u << 9);
        case 1: /* 262.144 KHz (16 cycles) */
            return cycles & (1u << 3);
        case 2: /* 65.536 KHz (64 cycles) */
            return cycles & (1u << 5);
        case 3: /* 16.384 KHz (256 cycles) */
            return cycles & (1u << 7);
    }
    assert(0);
}

static void clock_timerIncrement(struct Clock* clock, struct Memory* mem)
{
    uint8_t timer = mem->IO[IO_TimerCounter];
    if(timer == 0xFF) {
        clock->TimerOverflow = true;
    }
    mem->IO[IO_TimerCounter] = timer + 1;
}

static void clock_updateTimerControl(struct Clock* clock, struct Memory* mem, uint8_t val)
{
    uint8_t old = mem->IO[IO_TimerControl];
    mem->IO[IO_TimerControl] = val;

    /* When disabled the bit to the falling edge detector is zero */
    bool const oldBit = (old & 0x04) && clock_getTimerBit(old, clock->CycleCount);
    bool const newBit = (val & 0x04) && clock_getTimerBit(val, clock->CycleCount);

    /* Check for falling edge */
    if(oldBit && !newBit) {
        clock_timerIncrement(clock, mem);
    }
}

static void clock_countChange(struct Clock *clock, struct Memory *mem, uint16_t new_value)
{
    uint8_t tac = mem->IO[IO_TimerControl];
    if(tac & 0x04) { /* Timer enable */
        if(!clock_getTimerBit(tac, new_value) && clock_getTimerBit(tac, clock->CycleCount)) {
            clock_timerIncrement(clock, mem);
        }
    }
    clock->CycleCount = new_value;
    mem->IO[IO_Divider] = new_value >> 8u;
}

uint8_t mmu_readDirect(struct Memory* mem, uint16_t addr);

static void mmu_updateRTC(struct RTC* rtc)
{
    time_t now = time(NULL);
    time_t new_time = 0;
    if((rtc->BaseReg[4] & 0x40) == 0 && now > rtc->BaseTime) {
        new_time = now - rtc->BaseTime;
    }
    new_time += (time_t)rtc->BaseReg[0];
    new_time += (time_t)rtc->BaseReg[1] * 60;
    new_time += (time_t)rtc->BaseReg[2] * 60 * 60;
    new_time += (time_t)rtc->BaseReg[3] * 60 * 60 * 24;
    new_time += (time_t)(rtc->BaseReg[4] & 1u) * 60 * 60 * 24 * 256;

    rtc->BaseReg[0] = new_time % 60;
    new_time /= 60;
    rtc->BaseReg[1] = new_time % 60;
    new_time /= 60;
    rtc->BaseReg[2] = new_time % 24;
    new_time /= 24;
    rtc->BaseReg[3] = new_time % 256;
    new_time /= 256;
    /* Top bit of 9-bit day counter */
    rtc->BaseReg[4] = (rtc->BaseReg[4] & 0xFE) | (new_time % 2);
    new_time /= 2;
    /* Days overflow bit (sticky) */
    rtc->BaseReg[4] |= (new_time > 0? 0x80 : 0);

    rtc->BaseTime = now;
}

static uint8_t mmu_readRTC(struct RTC* rtc, uint8_t reg)
{
    // TODO: Re-enable non-rtc games?
    //if(reg > 0x0C || !gb->info.HasRTC) {
    //    return 0xFF;
    //}
    //else {
        if(rtc->Latched) {
            return rtc->LatchedReg[reg - 0x08];
        }
        else {
            mmu_updateRTC(rtc);
            return rtc->BaseReg[reg - 0x08];
        }
    //}
}

static void mmu_writeRTC(struct Memory* mem, uint8_t reg, uint8_t val)
{
    if(reg <= 0x0C) {
        mmu_updateRTC(&(mem->rtc));
        mem->rtc.BaseReg[reg - 0x08] = val;
    }
}
static uint8_t mmu_readBankedROM(struct Memory* mem, unsigned int relativeAddress)
{
    unsigned int cartAddr = (mem->MBCROMBank * 16384) + relativeAddress;
    return mem->CartROM[cartAddr % mem->CartROMSize];
}

static uint8_t mmu_readBankedRAM(struct Memory* mem, unsigned int relativeAddress)
{
    if(mem->MBCModel == Cart_MBC3 && mem->MBCRAMBank >= Cart_MBC3_RTCBase) {
        return mmu_readRTC(&(mem->rtc), mem->MBCRAMBank);
    }
    else {
        unsigned int cartAddr = (mem->MBCRAMBank * 8192) + relativeAddress;
        if(mem->CartRAMSize && mem->CartRAMBankEnabled) {
            return mem->CartRAM[cartAddr % mem->CartRAMSize];
        }
        else {
            return 0xFF;
        }
    }
}
uint8_t mmu_readDirect(struct Memory* mem, uint16_t addr) {
    if (addr < 0x4000) {
        /* 16K - ROM Bank #0 (fixed) */
        return mem->CartROM[addr];
    } else if (addr < 0x8000) {
        /* 16K - Banked ROM area */
        return mmu_readBankedROM(mem, addr - 0x4000);
    } else if (addr < 0xA000) {
        /* Video RAM */
        return mem->VideoRAM[addr - 0x8000];
    } else if (addr < 0xC000) {
        /* 8K - Banked RAM Area */
        return mmu_readBankedRAM(mem, addr - 0xA000);
    } else if (addr < 0xE000) {
        assert(false);
    } else if (addr < 0xFE00) {
        assert(false);
    } else if (addr < 0xFE9F) {
        /* OAM */
        return mem->OAM[addr - 0xFE00];
    } else if (addr < 0xFF00) {
        /* Empty */
        assert(false);
    } else if (addr < 0xFF80) {
        /* IO registers */
        return mem->IO[addr - 0xFF00];// | IOUnusedBits[addr - 0xFF00];
    } else if(addr < 0xFFFF) {
        assert(false);
    } else {
        assert(false);
    }
}

static void mmu_writeBankedRAM(struct Memory* mem, unsigned int relativeAddress, uint8_t data)
{
    if(mem->MBCModel == Cart_MBC3 && mem->MBCRAMBank >= Cart_MBC3_RTCBase) {
        mmu_writeRTC(mem, mem->MBCRAMBank, data);
    }
    else if(mem->CartRAMBankEnabled) {
        unsigned int cartAddr = (mem->MBCRAMBank * 8192) + relativeAddress;
        if(cartAddr < mem->CartRAMSize) {
            if(mem->MBCModel == Cart_MBC2) {
                // MBC2 internal RAM is 4bit
                data &= 0x0F;
            }
            mem->CartRAM[cartAddr] = data;
        }
    }
}

static void mmu_setROMBank(struct Memory* mem, unsigned int addr, uint8_t data)
{
    switch(mem->MBCModel) {
        case Cart_MBC1_16_8:
        case Cart_MBC1_4_32:
            {
                /* Bottom 5 bits of ROM Bank number */
                unsigned int bankNo = data & 0x1F;
                /* Zero in this register always maps to 1 */
                if(bankNo == 0) {
                    bankNo = 1;
                }
                mem->MBCROMBank = (mem->MBCROMBank & 0xE0) | bankNo;
            }
            break;
        case Cart_MBC2:
            {
                unsigned int bankNo = data & 0x0F;
                mem->MBCROMBank = bankNo? bankNo : 1;
            }
            break;
        case Cart_MBC3:
            {
                unsigned int bankNo = data & 0x7F;
                mem->MBCROMBank = bankNo? bankNo : 1;
            }
            break;
        case Cart_MBC5:
            if(addr < 0x3000) {
                mem->MBCROMBank = ((mem->MBCROMBank & ~0xFF) | data);
            }
            else {
                mem->MBCROMBank = ((mem->MBCROMBank & 0xFF) | ((data & 1u) << 9u));
            }
            break;

        default:
            break;
    }
}

static void mmu_setRAMBank(struct Memory* mem, unsigned int addr, uint8_t data)
{
    switch(mem->MBCModel) {
        case Cart_MBC1_16_8:
            mem->MBCROMBank = ((mem->MBCROMBank & 0x1F) | ((data & 0x3) << 5u));
            break;
        case Cart_MBC1_4_32:
            mem->MBCRAMBank = (data & 0x3);
            break;
        case Cart_MBC3:
            mem->MBCRAMBank = data;
            break;
        case Cart_MBC5:
            /* TODO Rumble is controlled by bit 4 */
            mem->MBCRAMBank = (data & 0x0F);
            break;

        default:
            break;
    }
}

void mmu_writeDirect(struct Memory* mem, struct Clock* clock, struct DMA* dma, uint16_t addr, uint8_t value)
{
    if (addr < 0x2000) {
        /* Cart RAM enable */
        mem->CartRAMBankEnabled = (value & 0xF) == 0xA;
    }
    else if(addr < 0x4000) {
        /* ROM Bank select */
        mmu_setROMBank(mem, addr, value);
    }
    else if(addr < 0x6000) {
        /* RAM Bank select (or high bits of ROM Bank for MBC1 mode 16/8) */
        mmu_setRAMBank(mem, addr, value);
    }
    else if(addr < 0x8000) {
        /* MBC1 Mode selection or MBC3 RTC latching */
        if(mem->MBCModel == Cart_MBC1_16_8 || mem->MBCModel == Cart_MBC1_4_32) {
            //<< mbc1 model selection >>
        }
        else if(mem->MBCModel == Cart_MBC3) {
            if(value == 0x00 && mem->rtc.Latched) {
                mem->rtc.Latched = false;
            }
            else if(value == 0x01 && !mem->rtc.Latched) {
                mmu_updateRTC(&(mem->rtc));
                for(unsigned i = 0; i < 5; i += 1) {
                    mem->rtc.LatchedReg[i] = mem->rtc.BaseReg[i];
                }
                mem->rtc.Latched = true;
            }
        }
    } else if (addr < 0xA000) {
        /* Video RAM */
        // TODO: Writes to VRAM should be ignored when the LCD is being redrawn
        mem->VideoRAM[addr - 0x8000] = value;
    } else if (addr < 0xC000) {
        assert(false);
        /* Banked RAM Area */
    } else if (addr < 0xE000) {
        assert(false);
        /* Internal RAM */
    } else if (addr < 0xFE00) {
        /* Mirror of internal RAM */
        assert(false);
    } else if (addr < 0xFE9F) {
        /* OAM */
        mem->OAM[addr - 0xFE00] = value;
    } else if (addr < 0xFF00) {
        assert(false);
        /* Empty */
    } else if (addr < 0xFF80) {
        /* IO registers */
        switch(addr - 0xFF00) {
            case IO_TimerCounter:
                /* Writes to the timer counter whilst it is loading are ignored */
                if(clock->TimerLoading) {
                    mem->IO[IO_TimerCounter] = value;
                    /* Writing to timer counter suppresses any pending overflow effects */
                    clock->TimerOverflow = false;
                }
                break;
            case IO_TimerModulo:
                mem->IO[IO_TimerModulo] = value;
                /* Whilst the modulo is being loaded any writes are effective immediately */
                if(clock->TimerLoading) {
                    mem->IO[IO_TimerCounter] = value;
                }
            case IO_TimerControl:
                clock_updateTimerControl(clock, mem, value);
                break;
            case IO_Divider:
                clock_countChange(clock, mem, 0);
                break;
            case IO_InterruptFlag:
                /* Top 5 bits of IF always read 1s */
                mem->IO[IO_InterruptFlag] = value | 0xE0;
                break;
            case IO_BootROMDisable: /* Writing to this address disables the boot ROM */
                {
                    mem->BootROMEnabled = false;
                }
                break;
            case IO_OAMDMA: /* LCD OAM DMA transfer */
                {
                    if(value <= 0xF1) {
                        dma->PendingSource = value;
                        dma->DelayStart = true;
                    } else {
                        assert(false && "Invalid LCD OAM transfer range");
                    }
                }
                break;
            case IO_LCDStat:
                {
                    uint8_t cur = mem->IO[IO_LCDStat];
                    mem->IO[IO_LCDStat] = (cur & 0x3) | (value & ~0x3);
                }
                break;

            case IO_LCDY: /* Current scanline -> writing resets it to zero */
                {
                    mem->IO[IO_LCDY] = 0;
                }
                break;

            default: mem->IO[addr - 0xFF00] = value; break;
        }
    } else if (addr < 0xFFFF) {
        assert(false);
    } else {
        assert(false);
    }
}

//TODO: Remove. this is only a shim for julia to read an arbitrary memory address
uint8_t gameboy_read(struct Gameboy* gb, uint16_t addr)
{
    return mmu_readDirect(&(gb->mem), addr);
}

static void dma_update(struct DMA* dma, struct Memory* mem)
{
    if(dma->PendingSource) {
        if(!dma->DelayStart) {
            dma->Source = dma->PendingSource << 8;
            dma->PendingSource = 0;
        }
        dma->DelayStart = false;
    }

    if(dma->Source && (dma->Source & 0xFF) < 160) {
        dma->Active = true;
        mem->OAM[dma->Source & 0xFF] = mmu_readDirect(mem, dma->Source);
        dma->Source += 1;
    }
    else {
        dma->Active = false;
    }
}

static uint8_t video_linePixel(uint8_t const line[2], unsigned x)
{
    return (((line[0] << x) & 0x80) >> 7) | (((line[1] << x) & 0x80) >> 6);
}
static uint16_t video_tileLineAddress(uint8_t index, unsigned y, bool lowBank)
{
    /* These addresses are relative to VRAM base address (0x8000) */
    uint16_t addr;
    if(lowBank) {
        addr = index * 16;
    }
    else {
        addr = 0x1000 + ((int8_t)index * 16);
    }
    return addr + (y * 2);
}
static uint8_t video_mapPixel(struct Gameboy* gb, bool hiMap, bool loTiles, unsigned int x, unsigned int y)
{
    uint8_t tileIndex = gb->mem.VideoRAM[(hiMap? 0x1C00 : 0x1800) + ((y / 8) * 32) + (x / 8)];
    uint16_t addr = video_tileLineAddress(tileIndex, (y % 8), loTiles);
    return video_linePixel(&gb->mem.VideoRAM[addr], x % 8);
}
uint8_t video_paletteLookup(uint8_t pixel, uint8_t palette)
{
    assert(pixel <= 3);
    return (palette >> (pixel * 2)) & 0x03;
}
static void video_readSprites(struct Gameboy* gb, int scanlineNum)
{
    /* sprites can be 8x8 or 8x16 */
    unsigned int spriteHeight = (gb->mem.IO[IO_LCDControl] & 0x04)? 16 : 8;

    /* Collect all the sprites on the current scanline */
    unsigned int numSprites = 0;
    struct GameboySprite* sprites = gb->lcd.ScanlineSprites;

    for(unsigned int i = 0; i < 160; i += 4) {
        /* Position of top-left corner of sprite, offset by (8,16)
         * i.e. top left corner of display is (8,16)
         */
        uint8_t ypos = gb->mem.OAM[i];
        uint8_t xpos = gb->mem.OAM[i + 1];
        if(ypos > 0 && ypos < 160 && xpos < 168) { /* on screen */
            if(scanlineNum + 16 >= ypos && scanlineNum + 16 < ypos + spriteHeight) { /* in scanline */
                /* Insert the sprite into the list, keeping the list in priority order */
                assert(numSprites <= 10);
                unsigned int insPos = numSprites;
                while(insPos > 0 && sprites[insPos - 1].x > xpos) {
                    if(insPos < 10) {
                        sprites[insPos] = sprites[insPos - 1];
                    }
                    insPos -= 1;
                }
                if(insPos < 10) {
                    uint8_t tile = gb->mem.OAM[i + 2];
                    uint8_t attr = gb->mem.OAM[i + 3];
                    if(spriteHeight == 16) {
                        tile &= 0xFE;
                    }

                    unsigned tileY = scanlineNum + 16 - ypos;
                    if(attr & 0x40) { /* Y Flip */
                        tileY = (spriteHeight - 1) - tileY;
                    }

                    uint16_t tileAddr = video_tileLineAddress(tile, tileY, true);

                    sprites[insPos].x = xpos;
                    sprites[insPos].pixels[0] = gb->mem.VideoRAM[tileAddr];
                    sprites[insPos].pixels[1] = gb->mem.VideoRAM[tileAddr + 1];
                    sprites[insPos].attrs = attr;

                    if(numSprites < 10) {
                        numSprites += 1;
                    }
                }
            }
        }
    }

    gb->lcd.NumSprites = numSprites;
}
static void video_drawPixel(struct Gameboy* gb, unsigned int scanlineNum, unsigned int x)
{
    uint8_t lcdc = gb->mem.IO[IO_LCDControl];
    bool hiMapBG = (lcdc & 0x08);
    bool hiMapWin = (lcdc & 0x40);
    bool bgEnable = (lcdc & 0x01);
    bool winEnable = (lcdc & 0x20);
    bool spriteEnable = (lcdc & 0x02);
    bool loTiles = (lcdc & 0x10);

    uint8_t wy = gb->mem.IO[IO_WindowY];
    uint8_t wx = gb->mem.IO[IO_WindowX];

    winEnable = winEnable && wx < 167 && wy < 144 && wy <= scanlineNum;
    spriteEnable = spriteEnable && gb->lcd.NumSprites > 0;

    if(winEnable || bgEnable || spriteEnable) {
        uint8_t scy = gb->mem.IO[IO_ScrollY];
        uint8_t scx = gb->mem.IO[IO_ScrollX];

        uint8_t bgPixel = 0;
        if(winEnable && x + 7 >= wx) {
            bgPixel = video_mapPixel(gb, hiMapWin, loTiles, x + 7 - wx, scanlineNum - wy);
        }
        else if(bgEnable) {
            bgPixel = video_mapPixel(gb, hiMapBG, loTiles, (x + scx) % 256, (scanlineNum + scy) % 256);
        }
        uint8_t finalColour = video_paletteLookup(bgPixel, gb->mem.IO[IO_BackgroundPalette]);

        if(spriteEnable) {
            uint8_t obp[2] = { gb->mem.IO[IO_ObjectPalette0], gb->mem.IO[IO_ObjectPalette1] };
            struct GameboySprite const* sprites = gb->lcd.ScanlineSprites;

            for(unsigned int n = 0; n < gb->lcd.NumSprites; n += 1) {
                if(x + 8 >= sprites[n].x && x + 8 < sprites[n].x + 8) {
                    unsigned int tileX = x + 8 - sprites[n].x;
                    bool const mirrored = (sprites[n].attrs & 0x20);
                    uint8_t pixel = video_linePixel(sprites[n].pixels, mirrored? (7 - tileX) : tileX);

                    if(pixel) {
                        bool hasPriority = (sprites[n].attrs & 0x80) == 0;
                        if(finalColour == 0 || hasPriority) {
                            uint8_t palette = obp[(sprites[n].attrs & 0x10)? 1 : 0];
                            finalColour = video_paletteLookup(pixel, palette);
                        }
                        /* Only draw first non-zero sprite pixel */
                        break;
                    }
                }
            }
        }

        gb->lcd.Buffer[x][scanlineNum] = finalColour;
    }
}
void video_update(struct Gameboy* gb, uint8_t scanline) {
    /* Each scanline takes 456 cycles to draw */
    //uint8_t scanline = gb->lcd.FrameProgress / 456;
    assert(scanline <= 154);

    bool lcdOn = (gb->mem.IO[IO_LCDControl] & 0x80);
    gb->mem.IO[IO_LCDY] = scanline;

    uint8_t stat = gb->mem.IO[IO_LCDStat];

    /* Handle LCDY compare - a bit in the STAT register is set and optionally an
     * interrupt is fired when the LCDY == LCDYCompare */
    if(lcdOn) {
        if(scanline == gb->mem.IO[IO_LCDYCompare]) {
            if((stat & 0x04) == 0) {
                /* Set coincidence bit */
                gb->mem.IO[IO_LCDStat] |= 0x04;
                /* Fire interrupt if enabled */
                if(stat & 0x40) {
                    gb->mem.IO[IO_InterruptFlag] |= Interrupt_LCDC;
                }
            }
        }
        else {
            gb->mem.IO[IO_LCDStat] &= ~0x04;
        }
    }

    unsigned int lcdMode = (stat & 0x03);

    /* The last 10 scanlines are the VBlank - nothing is actually drawn */
    if(scanline >= 144) {
        if(lcdMode != 1) {
            /* Entering VBlank - trigger interrupt */
            gb->mem.IO[IO_LCDStat] = (stat & ~0x03) | 1;
            gb->mem.IO[IO_InterruptFlag] |= Interrupt_VBlank;
            if((gb->mem.IO[IO_LCDControl] & 0x80) == 0) {
                memset(gb->lcd.Buffer, 0, sizeof(gb->lcd.Buffer));
            }
            gb->lcd.NewFrame = true;
        }
    }
    else {
        /* During each scanline the LCD mode cycles through 3 states:
         * 92clks - mode 2 (reading OAM)
         * 160clks - mode 3 (reading OAM & VRAM)
         * 204clks - mode 0 (HBlank)
         * = 456clks total
         */
        uint64_t scanlineProgress = gb->lcd.FrameProgress % 456;

        if(scanlineProgress < 92) {
            if(lcdMode != 2) { /* Entering mode 2 */
                gb->mem.IO[IO_LCDStat] = (stat & ~0x03) | 2;
                if(stat & 0x20) {
                    gb->mem.IO[IO_InterruptFlag] |= Interrupt_LCDC;
                }
                video_readSprites(gb, scanline);
                gb->lcd.CurX = 0;
            }
        }
        else if(scanlineProgress < (160 + 92)) {
            gb->mem.IO[IO_LCDStat] = (stat & ~0x03) | 3;
            if(lcdOn)
            {
                for(; gb->lcd.CurX < (scanlineProgress - 92); gb->lcd.CurX += 1) {
                    video_drawPixel(gb, scanline, gb->lcd.CurX);
                }
            }
        }
        else {
            if(lcdMode!= 0) { /* Entering mode 0 */
                if(lcdOn) {
                    for(; gb->lcd.CurX < 160; gb->lcd.CurX += 1) {
                        video_drawPixel(gb, scanline, gb->lcd.CurX);
                    }
                }
                gb->mem.IO[IO_LCDStat] = (stat & ~0x03);
                if(stat & 0x08) {
                    gb->mem.IO[IO_InterruptFlag] |= Interrupt_LCDC;
                }
            }
        }
    }

    gb->lcd.FrameProgress = (gb->lcd.FrameProgress + 1) % 70224;
}
void input_setUp(struct Buttons* buttons, int button)
{
    buttons->Pressed &= ~button;
}
void input_setDown(struct Buttons* buttons, struct Memory* mem, int button)
{
    if((buttons->Pressed & button) == 0) {
        buttons->Pressed |= button;
        mem->IO[IO_InterruptFlag] |= Interrupt_Joypad;
    }
}
void input_update(struct Memory* mem, struct Buttons* buttons)
{
    uint8_t invButtons = ~buttons->Pressed;
    uint8_t joyReg = mem->IO[IO_Joypad];
    if((joyReg & 0x20) != 0) { /* Directional keys */
        mem->IO[IO_Joypad] = ((joyReg & 0xF0) | ((invButtons >> 4u) & 0x0F));
    }
    else if((joyReg & 0x10) != 0) { /* Buttons */
        mem->IO[IO_Joypad] = ((joyReg & 0xF0) | (invButtons & 0x0F));
    }
    else if(joyReg == 3) { /* Model check - 0xFX == classic gameboy */
        mem->IO[IO_Joypad] = 0xFF;
    }
}
void gameboy_setButtonState(struct Gameboy* gb, int button, bool down)
{
    if(down) {
        input_setDown(&(gb->buttons), &(gb->mem), button);
    }
    else {
        input_setUp(&(gb->buttons), button);
    }
}
char const* gameboy_load(struct Gameboy* gb, bool skipChecksum)
{
    /* Reset state */
    memset(&gb->info.Title[0], 0, sizeof(gb->info.Title));
    gb->info.HasRTC = false;
    gb->info.HasBattery = false;
    gb->info.HasRumble = false;
    gb->mem.CartRAMSize = 0;
    gb->mem.CartROMSize = 0;
    gb->mem.rtc.BaseTime = time(NULL);
    for(unsigned i = 0; i < 5; i += 1) {
        gb->mem.rtc.BaseReg[i] = 0x00;
    }
    {
        uint8_t headerChecksum = 0;
        for(unsigned int i = 0x134; i < 0x14D; i += 1) {
            headerChecksum = headerChecksum - gb->mem.CartROM[i] - 1;
        }
        if(headerChecksum != gb->mem.CartROM[0x14D] && !skipChecksum) {
            return "Header checksum incorrect";
        }
    }
    /* Copy ROM title */
    for(unsigned int i = 0; i < 16; i += 1) {
        uint8_t x = gb->mem.CartROM[0x134 + i];
        /* High bytes are part of new-style licences */
        if(x <= 127) {
            gb->info.Title[i] = x;
        }
    }
    switch (gb->mem.CartROM[0x147]) {
        case 0x00:
            gb->mem.MBCModel = Cart_MBC_None;
            break;
        case 0x01:
            gb->mem.MBCModel = Cart_MBC1_16_8;
            break;
        case 0x02:
            gb->mem.MBCModel = Cart_MBC1_16_8;
            break;
        case 0x03:
            gb->mem.MBCModel = Cart_MBC1_16_8;
            gb->info.HasBattery = true;
            break;
        case 0x05:
            gb->mem.MBCModel = Cart_MBC2;
            break;
        case 0x06:
            gb->mem.MBCModel = Cart_MBC2;
            gb->info.HasBattery = true;
            break;
        case 0x08:
            gb->mem.MBCModel = Cart_MBC_None;
            break;
        case 0x09:
            gb->mem.MBCModel = Cart_MBC_None;
            gb->info.HasBattery = true;
            break;
        case 0x0F:
            gb->mem.MBCModel = Cart_MBC3;
            gb->info.HasBattery = true;
            gb->info.HasRTC = true;
            break;
        case 0x10:
            gb->mem.MBCModel = Cart_MBC3;
            gb->info.HasBattery = true;
            gb->info.HasRTC = true;
            break;
        case 0x11:
            gb->mem.MBCModel = Cart_MBC3;
            break;
        case 0x12:
            gb->mem.MBCModel = Cart_MBC3;
            break;
        case 0x13:
            gb->mem.MBCModel = Cart_MBC3;
            gb->info.HasBattery = true;
            break;
        case 0x19:
            gb->mem.MBCModel = Cart_MBC5;
            break;
        case 0x1A:
            gb->mem.MBCModel = Cart_MBC5;
            break;
        case 0x1B:
            gb->mem.MBCModel = Cart_MBC5;
            gb->info.HasBattery = true;
            break;
        case 0x1C:
            gb->mem.MBCModel = Cart_MBC5;
            gb->info.HasRumble = true;
            break;
        case 0x1D:
            gb->mem.MBCModel = Cart_MBC5;
            gb->info.HasRumble = true;
            break;
        case 0x1E:
            gb->mem.MBCModel = Cart_MBC5;
            gb->info.HasBattery = true;
            gb->info.HasRumble = true;
            break;
        case 0x1F: return "Pocket Camera not supported";
        case 0xFD: return "Bandai TAMA5 not supported";
        case 0xFE: return "Hudson HuC-3 not supported";
        case 0xFF: return "Hudson HuC-1 not supported";
        case 0x0B:
        case 0x0C:
        case 0x0D:
            return "MMM01 not supported";
            break;
        default: return "Unknown ROM type";
    }
    switch (gb->mem.CartROM[0x148]) {
        case 0x00: /* 256 Kbit */
            gb->mem.CartROMSize = 32768;
            break;
        case 0x01: /* 512 Kbit */
            gb->mem.CartROMSize = 65536;
            break;
        case 0x02: /* 1 Mbit */
            gb->mem.CartROMSize = 131072;
            break;
        case 0x03: /* 2 Mbit */
            gb->mem.CartROMSize = 262144;
            break;
        case 0x04: /* 4 Mbit */
            gb->mem.CartROMSize = 524288;
            break;
        case 0x05: /* 8 Mbit */
            gb->mem.CartROMSize = 1048576;
            break;
        case 0x06: /* 16 Mbit */
            gb->mem.CartROMSize = 2097152;
            break;
        case 0x52: /* 9 Mbit */
            gb->mem.CartROMSize = 1179648;
            break;
        case 0x53: /* 10 Mbit */
            gb->mem.CartROMSize = 1310720;
            break;
        case 0x54: /* 12 Mbit */
            gb->mem.CartROMSize = 1572864;
            break;
    }
    switch (gb->mem.CartROM[0x149]) {
        case 0: /* no RAM */
            gb->mem.CartRAMSize = 0;
            break;
        case 1: /* 16 kBit */
            gb->mem.CartRAMSize = 2048;
            break;
        case 2: /* 64 kBit */
            gb->mem.CartRAMSize = 8192;
            break;
        case 3: /* 256 kBit */
            gb->mem.CartRAMSize = 32768;
            break;
        case 4: /* 1 MBit */
            gb->mem.CartRAMSize = 131072;
            break;
    }
    /* All MBC2 chips contain 512x4bits RAM even though ROM[0x149] == 0 */
    if(gb->mem.MBCModel == Cart_MBC2) {
        gb->mem.CartRAMSize = 512;
    }
    {
        uint16_t romChecksum = 0;
        for(unsigned int i = 0; i < gb->mem.CartROMSize; i+= 1) {
            romChecksum += gb->mem.CartROM[i];
        }
        /* ROM Checksum does not include the checksum bytes */
        romChecksum -= gb->mem.CartROM[0x14E];
        romChecksum -= gb->mem.CartROM[0x14F];

        if(!skipChecksum && (((uint16_t)gb->mem.CartROM[0x14E] << 8u) | gb->mem.CartROM[0x14F]) != romChecksum) {
            return "ROM Checksum incorrect";
        }
    }

#if GAMEBOY_DEBUG
    gb->debug.Context = NULL;
    gb->debug.MemoryReadHook = NULL;
    gb->debug.MemoryWriteHook = NULL;
#endif

    return NULL;
}
int gameboy_reset(struct Gameboy* gb, bool enableBootROM)
{
    gb->clock.CycleCount = 0;
    gb->clock.TimerOverflow = false;
    gb->clock.TimerLoading = false;
    gb->mem.BootROMEnabled = enableBootROM;
    /* Clear all VRAM - the bootrom does this. */
    memset(gb->mem.VideoRAM, 0, sizeof(gb->mem.VideoRAM));
    /* Initialise required IO registers */
    gb->mem.IO[IO_Joypad] = 0xCF;
    gb->mem.IO[IO_SerialControl] = 0x7E;
    gb->mem.IO[IO_TimerCounter] = 0x00;
    gb->mem.IO[IO_TimerModulo] = 0x00;
    gb->mem.IO[IO_TimerControl] = 0x00;
    gb->mem.IO[IO_LCDControl] = 0x91;
    gb->mem.IO[IO_ScrollY] = 0x00;
    gb->mem.IO[IO_ScrollX] = 0x00;
    gb->mem.IO[IO_LCDYCompare] = 0x00;
    gb->mem.IO[IO_BackgroundPalette] = 0xFC;
    gb->mem.IO[IO_ObjectPalette0] = 0xFF;
    gb->mem.IO[IO_ObjectPalette1] = 0xFF;
    gb->mem.IO[IO_WindowX] = 0x00;
    gb->mem.IO[IO_WindowY] = 0x00;

    /* Initialise sound IO registers */
    //gb->mem.IO[0xFF10] = 0x80;
    //gb->mem.IO[0xFF11] = 0xBF;
    //gb->mem.IO[0xFF12] = 0xF3;
    //gb->mem.IO[0xFF14] = 0xBF;
    //gb->mem.IO[0xFF16] = 0x3F;
    //gb->mem.IO[0xFF17] = 0x00;
    //gb->mem.IO[0xFF19] = 0xBF;
    //gb->mem.IO[0xFF1A] = 0x7F;
    //gb->mem.IO[0xFF1B] = 0xFF;
    //gb->mem.IO[0xFF1C] = 0x9F;
    //gb->mem.IO[0xFF1E] = 0xBF;
    //gb->mem.IO[0xFF20] = 0xFF;
    //gb->mem.IO[0xFF21] = 0x00;
    //gb->mem.IO[0xFF22] = 0x00;
    //gb->mem.IO[0xFF23] = 0xBF;
    //gb->mem.IO[0xFF24] = 0x77;
    //gb->mem.IO[0xFF25] = 0xF3;
    //gb->mem.IO[0xFF26] = 0xF1;
    gb->mem.MBCRAMBank = 0;
    gb->mem.CartRAMBankEnabled = false;

    /* MBC1 always starts up in 16/8 mode */
    if(gb->mem.MBCModel == Cart_MBC1_4_32) {
        gb->mem.MBCModel = Cart_MBC1_16_8;
    }

    gb->mem.MBCROMBank = 1;
    gb->buttons.Pressed = 0;

    gb->lcd.NewFrame = false;
    gb->dma.PendingSource = 0;
    gb->dma.DelayStart = false;
    gb->dma.Source = 0;
    gb->dma.Active = false;
    gb->lcd.FrameProgress = 0;
    return 0;
}

void gameboy_free(struct Gameboy* gb) {
  free(gb);
}

struct Gameboy* gameboy_alloc() {
  struct Gameboy* gb = (struct Gameboy*) malloc(sizeof(struct Gameboy));
  memset(gb, 0, sizeof(struct Gameboy));
  return gb;
}

void gameboy_load_rom(struct Gameboy* gb, char* rom_path) {
  LoadROM(gb, rom_path);
}

void gameboy_load_ram(struct Gameboy* gb, unsigned int ram_size, uint8_t* bytes) {
  gb->mem.CartRAMSize = ram_size;
  memcpy(gb->mem.CartRAM, bytes, ram_size);
}

// uint32_t palette[4] = { 0xFFFFFFFF, 0xFFAAAAAA, 0xFF555555, 0xFF000000 }; // Grayscale
uint32_t palette[4] = { 0xFFC4CFA1, 0xFF8B956D, 0xFF4D543C, 0xFF1F1F1F}; // Pocket
//uint32_t palette[4] = { 0xFF9BBC0F, 0xFF8BAC0F, 0xFF306230, 0xFF0F380F}; // DMG

int gameboy_frame_stride() {
      return 160*sizeof(uint32_t);
}

void* updateFrameBuffer(struct LCD* lcd) {
  if(lcd->NewFrame) {
    lcd->NewFrame = false;
    for(unsigned int y = 0; y < 144; y += 1) {
      for(unsigned int x = 0; x < 160; x += 1) {
        uint8_t pixel = lcd->Buffer[x][y];
        lcd->Frame[y][x] = palette[pixel & 0x03];
      }
    }
    return lcd->Frame;
  }

  return NULL;
}

struct Memory* getMemory(struct Gameboy* gb) {
  return &(gb->mem);
}

struct DMA* getDma(struct Gameboy* gb) {
  return &(gb->dma);
}

struct Clock* getClock(struct Gameboy* gb) {
  return &(gb->clock);
}

uint8_t getIflag(struct Memory* mem) {
  return mem->IO[IO_InterruptFlag];
}

void setIF(struct Memory* mem, uint8_t iflag) {
  mem->IO[IO_InterruptFlag] = iflag;
}

bool bootRomEnabled(struct Memory* mem) {
  return mem->BootROMEnabled;
}

struct Buttons* getButtons(struct Gameboy* gb) {
  return &(gb->buttons);
}

struct LCD* getLCD(struct Gameboy* gb) {
  return &(gb->lcd);
}

// Returns size of cartridge RAM
unsigned int gameboy_cart_ram_size(struct Gameboy* gb) {
  return gb->mem.CartRAMSize;
}

// Returns the cartridge RAM
uint8_t* gameboy_cart_ram(struct Gameboy* gb) {
  return gb->mem.CartRAM;
}

// Returns 0x42 as a sanity check
uint8_t gameboy_sanity_check() {
  return 0x42;
}
