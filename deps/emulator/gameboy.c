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
uint8_t mmu_read(struct Gameboy*, int);
void mmu_write(struct Gameboy*, int, uint8_t);
static void dma_update(struct DMA*, struct Memory*);
void video_update(struct Gameboy* gb);
void input_setUp(struct Gameboy*, int button);
void input_setDown(struct Gameboy*, int button);

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
    /* Video runs at 1 pixel per clock (4 per machine cycle) */
    video_update(gb);
    video_update(gb);
    video_update(gb);
    video_update(gb);
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
uint8_t Imm8(struct Gameboy* gb)
{
    gb->cpu.PC += 1;
    return mmu_read(gb, gb->cpu.PC - 1);
}

static inline int8_t Imm8i(struct Gameboy* gb)
{
    return (int8_t)Imm8(gb);
}

uint16_t Imm16(struct Gameboy* gb)
{
    uint8_t const lo = Imm8(gb);
    uint8_t const hi = Imm8(gb);
    return (hi << 8u) | lo;
}
uint16_t ReadAF(struct Cpu* cpu) { return ((cpu->A) << 8u) | (cpu->F); }
uint16_t ReadBC(struct Cpu* cpu) { return ((cpu->B) << 8u) | (cpu->C); }
uint16_t ReadDE(struct Cpu* cpu) { return ((cpu->D) << 8u) | (cpu->E); }
uint16_t ReadHL(struct Cpu* cpu) { return ((cpu->H) << 8u) | (cpu->L); }

void WriteAF(struct Cpu* cpu, uint16_t af) { cpu->A = (af >> 8u); cpu->F = (af & 0xF0); }
void WriteBC(struct Cpu* cpu, uint16_t bc) { cpu->B = (bc >> 8u); cpu->C = (bc & 0xFF); }
void WriteDE(struct Cpu* cpu, uint16_t de) { cpu->D = (de >> 8u); cpu->E = (de & 0xFF); }
void WriteHL(struct Cpu* cpu, uint16_t hl) { cpu->H = (hl >> 8u); cpu->L = (hl & 0xFF); }
void UpdateZ(struct Cpu* cpu, bool set) { if(set) { cpu->F |= 0x80; } else { cpu->F &= ~0x80; } }
void UpdateN(struct Cpu* cpu, bool set) { if(set) { cpu->F |= 0x40; } else { cpu->F &= ~0x40; } }
void UpdateH(struct Cpu* cpu, bool set) { if(set) { cpu->F |= 0x20; } else { cpu->F &= ~0x20; } }
void UpdateC(struct Cpu* cpu, bool set) { if(set) { cpu->F |= 0x10; } else { cpu->F &= ~0x10; } }

bool ReadZ(struct Cpu* cpu) { return cpu->F & 0x80; }
bool ReadN(struct Cpu* cpu) { return cpu->F & 0x40; }
bool ReadH(struct Cpu* cpu) { return cpu->F & 0x20; }
bool ReadC(struct Cpu* cpu) { return cpu->F & 0x10; }

void UpdateZNHC(struct Cpu* cpu, bool z, bool n, bool h, bool c)
{
    UpdateZ(cpu, z);
    UpdateN(cpu, n);
    UpdateH(cpu, h);
    UpdateC(cpu, c);
}
void Push16(struct Gameboy* gb, uint16_t val)
{
    mmu_write(gb, gb->cpu.SP - 2, (uint8_t)(val));
    mmu_write(gb, gb->cpu.SP - 1, (uint8_t)(val >> 8u));
    gb->cpu.SP -= 2;
}

uint16_t Pop16(struct Gameboy* gb)
{
    gb->cpu.SP += 2;
    uint16_t val = mmu_read(gb, gb->cpu.SP - 2);
    val |= mmu_read(gb, gb->cpu.SP - 1) << 8;
    return val;
}
uint8_t Add8(struct Cpu* cpu, uint8_t val0, uint8_t val1, bool carry)
{
    unsigned int sum = val0 + val1 + carry;
    unsigned int halfSum = (val0 & 0xF) + (val1 & 0xF) + carry;
    UpdateZNHC(cpu, (sum & 0xFF) == 0, false, halfSum > 0xF, sum > 0xFF);
    return sum & 0xFF;
}
uint8_t Sub8(struct Cpu* cpu, uint8_t val0, uint8_t val1, bool carry)
{
    unsigned int sum = val0 - val1 - carry;
    unsigned int halfSum = (val0 & 0xF) - (val1 & 0xF) - carry;
    UpdateZNHC(cpu, (sum & 0xFF) == 0, true, halfSum > 0xF, sum > 0xFF);
    return sum;
}
uint8_t Inc8(struct Cpu* cpu, uint8_t val)
{
    UpdateZ(cpu, val == 0xFF);
    UpdateN(cpu, false);
    UpdateH(cpu, (val & 0xF) == 0xF);
    return val + 1;
}
uint8_t Dec8(struct Cpu* cpu, int8_t val)
{
    UpdateZ(cpu, val == 0x01);
    UpdateN(cpu, true);
    UpdateH(cpu, (val & 0xF) == 0x0);
    return val - 1;
}
void BitAnd(struct Cpu* cpu, uint8_t value)
{
    cpu->A &= value;
    UpdateZNHC(cpu, cpu->A == 0, false, true, false);
}

void BitOr(struct Cpu* cpu, uint8_t value)
{
    cpu->A |= value;
    UpdateZNHC(cpu, cpu->A == 0, false, false, false);
}

void BitXor(struct Cpu* cpu, uint8_t value)
{
    cpu->A ^= value;
    UpdateZNHC(cpu, cpu->A == 0, false, false, false);
}
uint8_t ReadRegN(struct Gameboy* gb, unsigned int regNum)
{
    switch(regNum) {
        case 0: return gb->cpu.B;
        case 1: return gb->cpu.C;
        case 2: return gb->cpu.D;
        case 3: return gb->cpu.E;
        case 4: return gb->cpu.H;
        case 5: return gb->cpu.L;
        case 6: return mmu_read(gb, ReadHL(&(gb->cpu)));
        case 7: return gb->cpu.A;
    }
    assert(false);
}

void WriteRegN(struct Gameboy* gb, unsigned int regNum, uint8_t newVal)
{
    switch(regNum) {
        case 0: gb->cpu.B = newVal; break;
        case 1: gb->cpu.C = newVal; break;
        case 2: gb->cpu.D = newVal; break;
        case 3: gb->cpu.E = newVal; break;
        case 4: gb->cpu.H = newVal; break;
        case 5: gb->cpu.L = newVal; break;
        case 6: mmu_write(gb, ReadHL(&(gb->cpu)), newVal); break;
        case 7: gb->cpu.A = newVal; break;
    }
}
void cpu_cb_op(struct Gameboy* gb, struct Cpu* cpu)
{
    uint8_t const cb_opcode = Imm8(gb);

    // XX XXX XXX
    // ~~ operation
    //    ~~~ bit index or sub-operation
    //        ~~~ register
    uint8_t const op = cb_opcode >> 6u;
    uint8_t const bit = (cb_opcode >> 3u) & 0x07;
    uint8_t const reg = cb_opcode & 0x07;

    switch(op) {
        case 0: { // shift/rotate & swap
            switch(bit) {
                case 0: { // rlc rN
                    uint8_t val = ReadRegN(gb, reg);
                    UpdateZNHC(cpu, val == 0, false, false, (val & 0x80));
                    val = (val << 1u) | (val >> 7u);
                    WriteRegN(gb, reg, val);
                } break;
                case 1: { // rrc rN
                    uint8_t val = ReadRegN(gb, reg);
                    UpdateZNHC(cpu, val == 0, false, false, (val & 0x01));
                    val = (val >> 1u) | (val << 7u);
                    WriteRegN(gb, reg, val);
                } break;
                case 2: { // rl rN
                    uint8_t val = ReadRegN(gb, reg);
                    uint8_t rotated = (val << 1u) | (ReadC(cpu)? 1 : 0);
                    UpdateZNHC(cpu, rotated == 0, false, false, (val & 0x80));
                    WriteRegN(gb, reg, rotated);
                } break;
                case 3: { // rr rN
                    uint8_t val = ReadRegN(gb, reg);
                    uint8_t rotated = (val >> 1u) | (ReadC(cpu)? 0x80 : 0);
                    UpdateZNHC(cpu, rotated == 0, false, false, (val & 0x01));
                    WriteRegN(gb, reg, rotated);
                } break;
                case 4: { // sla rN
                    uint8_t val = ReadRegN(gb, reg);
                    uint8_t shifted = val << 1u;
                    UpdateZNHC(cpu, shifted == 0, false, false, (val & 0x80));
                    WriteRegN(gb, reg, shifted);
                } break;
                case 5: { // sra rN
                    uint8_t val = ReadRegN(gb, reg);
                    uint8_t shifted = (val >> 1u) | (val & 0x80);
                    UpdateZNHC(cpu, (shifted == 0), false, false, (val & 0x01));
                    WriteRegN(gb, reg, shifted);
                } break;
                case 6: { // swap rN
                    uint8_t val = ReadRegN(gb, reg);
                    val = (val >> 4u) | (val << 4u);
                    UpdateZNHC(cpu, val == 0, false, false, false);
                    WriteRegN(gb, reg, val);
                } break;
                case 7: { // srl rN
                    uint8_t val = ReadRegN(gb, reg);
                    uint8_t shifted = val >> 1u;
                    UpdateZNHC(cpu, shifted == 0, false, false, (val & 0x01));
                    WriteRegN(gb, reg, shifted);
                } break;
            }
        }
        break;
        case 1: { // bit n, rN
            uint8_t val = ReadRegN(gb, reg);
            UpdateZ(cpu, (val & (1u << bit)) == 0);
            UpdateN(cpu, false);
            UpdateH(cpu, true);
        }
        break;
        case 2: { // res n, rN
            uint8_t val = ReadRegN(gb, reg);
            WriteRegN(gb, reg, (val & ~(1u << bit)));
        }
        break;
        case 3: { // set n, rN
            uint8_t val = ReadRegN(gb, reg);
            WriteRegN(gb, reg, (val | (1u << bit)));
        }
        break;
    }
}
uint16_t Add16(struct Cpu* cpu, uint16_t val0, uint16_t val1)
{
    unsigned int sum = val0 + val1;
    unsigned int halfSum = (val0 & 0xFFF) + (val1 & 0xFFF);
    UpdateN(cpu, false);
    UpdateH(cpu, halfSum > 0xFFF);
    UpdateC(cpu, sum > 0xFFFF);
    return sum & 0xFFFF;
}
void Jump(struct Cpu* cpu, uint16_t addr)
{
    cpu->PC = addr;
}

void JumpRel(struct Cpu* cpu, int8_t offset)
{
    cpu->PC += offset;
}

void Call(struct Gameboy* gb, uint16_t addr)
{
    Push16(gb, gb->cpu.PC);
    gb->cpu.PC = addr;
}

void Ret(struct Gameboy* gb)
{
    Jump(&(gb->cpu), Pop16(gb));
}

static uint8_t mmu_readDirect(struct Memory* mem, uint16_t addr);
void cpu_step(struct Gameboy* gb, struct Cpu* cpu, uint8_t opcode)
{

    switch(opcode) {
    // ld $reg16, imm16
    case 0x01: WriteBC(cpu, Imm16(gb)); break;
    case 0x11: WriteDE(cpu, Imm16(gb)); break;
    case 0x21: WriteHL(cpu, Imm16(gb)); break;
    case 0x31: cpu->SP = Imm16(gb); break;
    case 0x08: { // ld (imm16), $sp
        uint16_t addr = Imm16(gb);
        mmu_write(gb, addr, (cpu->SP & 0xFF));
        mmu_write(gb, addr + 1, (cpu->SP >> 8u));
    } break;
    case 0xF9: // ld $sp, $hl
        clock_increment(gb);
        cpu->SP = ReadHL(cpu);
        break;
    case 0xF8: { // ld $hl, $sp + imm8
        uint16_t ea = cpu->SP + Imm8i(gb);
        clock_increment(gb);
        WriteHL(cpu, ea);
        UpdateZNHC(cpu, false, false, (ea & 0xF) < (cpu->SP & 0xF), (ea & 0xFF) < (cpu->SP & 0xFF));
    } break;
    // pop $reg16
    case 0xC1: WriteBC(cpu, Pop16(gb)); break;
    case 0xD1: WriteDE(cpu, Pop16(gb)); break;
    case 0xE1: WriteHL(cpu, Pop16(gb)); break;
    case 0xF1: WriteAF(cpu, Pop16(gb)); break;

    // push $reg16
    case 0xC5:
        clock_increment(gb);
        Push16(gb, ReadBC(cpu));
        break;
    case 0xD5:
        clock_increment(gb);
        Push16(gb, ReadDE(cpu));
        break;
    case 0xE5:
        clock_increment(gb);
        Push16(gb, ReadHL(cpu));
        break;
    case 0xF5:
        clock_increment(gb);
        Push16(gb, ReadAF(cpu));
        break;
    // add $a, reg8
    case 0x80: cpu->A = Add8(cpu, cpu->A, cpu->B, false); break;
    case 0x81: cpu->A = Add8(cpu, cpu->A, cpu->C, false); break;
    case 0x82: cpu->A = Add8(cpu, cpu->A, cpu->D, false); break;
    case 0x83: cpu->A = Add8(cpu, cpu->A, cpu->E, false); break;
    case 0x84: cpu->A = Add8(cpu, cpu->A, cpu->H, false); break;
    case 0x85: cpu->A = Add8(cpu, cpu->A, cpu->L, false); break;
    case 0x87: cpu->A = Add8(cpu, cpu->A, cpu->A, false); break;
    // add $a, ($hl)
    case 0x86: cpu->A = Add8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
    // add $a, imm8
    case 0xC6: cpu->A = Add8(cpu, cpu->A, Imm8(gb), false); break;
    // adc $a, reg8
    case 0x88: cpu->A = Add8(cpu, cpu->A, cpu->B, ReadC(cpu)); break;
    case 0x89: cpu->A = Add8(cpu, cpu->A, cpu->C, ReadC(cpu)); break;
    case 0x8A: cpu->A = Add8(cpu, cpu->A, cpu->D, ReadC(cpu)); break;
    case 0x8B: cpu->A = Add8(cpu, cpu->A, cpu->E, ReadC(cpu)); break;
    case 0x8C: cpu->A = Add8(cpu, cpu->A, cpu->H, ReadC(cpu)); break;
    case 0x8D: cpu->A = Add8(cpu, cpu->A, cpu->L, ReadC(cpu)); break;
    case 0x8F: cpu->A = Add8(cpu, cpu->A, cpu->A, ReadC(cpu)); break;
    // adc $a, ($hl)
    case 0x8E: cpu->A = Add8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), ReadC(cpu)); break;
    // adc $a, imm8
    case 0xCE: cpu->A = Add8(cpu, cpu->A, Imm8(gb), ReadC(cpu)); break;
    // sub $a, reg8
    case 0x90: cpu->A = Sub8(cpu, cpu->A, cpu->B, false); break;
    case 0x91: cpu->A = Sub8(cpu, cpu->A, cpu->C, false); break;
    case 0x92: cpu->A = Sub8(cpu, cpu->A, cpu->D, false); break;
    case 0x93: cpu->A = Sub8(cpu, cpu->A, cpu->E, false); break;
    case 0x94: cpu->A = Sub8(cpu, cpu->A, cpu->H, false); break;
    case 0x95: cpu->A = Sub8(cpu, cpu->A, cpu->L, false); break;
    case 0x97: cpu->A = Sub8(cpu, cpu->A, cpu->A, false); break;
    // sub $a, ($hl)
    case 0x96: cpu->A = Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
    // sub $a, imm8
    case 0xD6: cpu->A = Sub8(cpu, cpu->A, Imm8(gb), false); break;
    // sbc $a, reg8
    case 0x98: cpu->A = Sub8(cpu, cpu->A, cpu->B, ReadC(cpu)); break;
    case 0x99: cpu->A = Sub8(cpu, cpu->A, cpu->C, ReadC(cpu)); break;
    case 0x9A: cpu->A = Sub8(cpu, cpu->A, cpu->D, ReadC(cpu)); break;
    case 0x9B: cpu->A = Sub8(cpu, cpu->A, cpu->E, ReadC(cpu)); break;
    case 0x9C: cpu->A = Sub8(cpu, cpu->A, cpu->H, ReadC(cpu)); break;
    case 0x9D: cpu->A = Sub8(cpu, cpu->A, cpu->L, ReadC(cpu)); break;
    case 0x9F: cpu->A = Sub8(cpu, cpu->A, cpu->A, ReadC(cpu)); break;
    // sbc $a, ($hl)
    case 0x9E: cpu->A = Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), ReadC(cpu)); break;
    // sbc $a, imm8
    case 0xDE: cpu->A = Sub8(cpu, cpu->A, Imm8(gb), ReadC(cpu)); break;
    // inc reg8
    case 0x04: cpu->B = Inc8(cpu, cpu->B); break;
    case 0x0C: cpu->C = Inc8(cpu, cpu->C); break;
    case 0x14: cpu->D = Inc8(cpu, cpu->D); break;
    case 0x1C: cpu->E = Inc8(cpu, cpu->E); break;
    case 0x24: cpu->H = Inc8(cpu, cpu->H); break;
    case 0x2C: cpu->L = Inc8(cpu, cpu->L); break;
    case 0x3C: cpu->A = Inc8(cpu, cpu->A); break;
    // dec reg8
    case 0x05: cpu->B = Dec8(cpu, cpu->B); break;
    case 0x0D: cpu->C = Dec8(cpu, cpu->C); break;
    case 0x15: cpu->D = Dec8(cpu, cpu->D); break;
    case 0x1D: cpu->E = Dec8(cpu, cpu->E); break;
    case 0x25: cpu->H = Dec8(cpu, cpu->H); break;
    case 0x2D: cpu->L = Dec8(cpu, cpu->L); break;
    case 0x3D: cpu->A = Dec8(cpu, cpu->A); break;
    // cp $a, reg8
    case 0xB8: Sub8(cpu, cpu->A, cpu->B, false); break;
    case 0xB9: Sub8(cpu, cpu->A, cpu->C, false); break;
    case 0xBA: Sub8(cpu, cpu->A, cpu->D, false); break;
    case 0xBB: Sub8(cpu, cpu->A, cpu->E, false); break;
    case 0xBC: Sub8(cpu, cpu->A, cpu->H, false); break;
    case 0xBD: Sub8(cpu, cpu->A, cpu->L, false); break;
    case 0xBF: Sub8(cpu, cpu->A, cpu->A, false); break;
    // cp $a, ($hl)
    case 0xBE: Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
    // cp $a, imm8
    case 0xFE: Sub8(cpu, cpu->A, Imm8(gb), false); break;
    // and/or/xor $a, $reg8
    case 0xA0: BitAnd(cpu, cpu->B); break;
    case 0xA1: BitAnd(cpu, cpu->C); break;
    case 0xA2: BitAnd(cpu, cpu->D); break;
    case 0xA3: BitAnd(cpu, cpu->E); break;
    case 0xA4: BitAnd(cpu, cpu->H); break;
    case 0xA5: BitAnd(cpu, cpu->L); break;
    case 0xA7: BitAnd(cpu, cpu->A); break;
    case 0xB0: BitOr(cpu, cpu->B); break;
    case 0xB1: BitOr(cpu, cpu->C); break;
    case 0xB2: BitOr(cpu, cpu->D); break;
    case 0xB3: BitOr(cpu, cpu->E); break;
    case 0xB4: BitOr(cpu, cpu->H); break;
    case 0xB5: BitOr(cpu, cpu->L); break;
    case 0xB7: BitOr(cpu, cpu->A); break;
    case 0xA8: BitXor(cpu, cpu->B); break;
    case 0xA9: BitXor(cpu, cpu->C); break;
    case 0xAA: BitXor(cpu, cpu->D); break;
    case 0xAB: BitXor(cpu, cpu->E); break;
    case 0xAC: BitXor(cpu, cpu->H); break;
    case 0xAD: BitXor(cpu, cpu->L); break;
    case 0xAF: BitXor(cpu, cpu->A); break;

    // and/or/xor $a, ($hl)
    case 0xA6: BitAnd(cpu, mmu_read(gb, ReadHL(cpu))); break;
    case 0xB6: BitOr(cpu, mmu_read(gb, ReadHL(cpu))); break;
    case 0xAE: BitXor(cpu, mmu_read(gb, ReadHL(cpu))); break;

    // and/or/xor $a, imm8
    case 0xE6: BitAnd(cpu, Imm8(gb)); break;
    case 0xF6: BitOr(cpu, Imm8(gb)); break;
    case 0xEE: BitXor(cpu, Imm8(gb)); break;
    case 0x07: { // rlca
        UpdateZNHC(cpu, false, false, false, (cpu->A & 0x80));
        cpu->A = (cpu->A << 1u) | (cpu->A >> 7u);
    } break;
    case 0x0F: { // rrca
        UpdateZNHC(cpu, false, false, false, (cpu->A & 0x01));
        cpu->A = (cpu->A >> 1u) | (cpu->A << 7u);
    } break;
    case 0x17: { // rla
        bool c = ReadC(cpu);
        UpdateZNHC(cpu, false, false, false, (cpu->A & 0x80));
        cpu->A = (cpu->A << 1u) | (c? 1 : 0);
    } break;
    case 0x1F: { // rra
        bool c = ReadC(cpu);
        UpdateZNHC(cpu, false, false, false, (cpu->A & 0x01));
        cpu->A = (cpu->A >> 1u) | (c? 0x80 : 0x00);
    } break;
    case 0x2F: { // cpl
        UpdateN(cpu, true);
        UpdateH(cpu, true);
        cpu->A ^= UINT8_MAX;
    } break;
    case 0xCB: {
        cpu_cb_op(gb, cpu);
    } break;
    case 0x03: { // inc $bc
        clock_increment(gb);
        WriteBC(cpu, ReadBC(cpu) + 1);
    } break;
    case 0x13: { // inc $de
        clock_increment(gb);
        WriteDE(cpu, ReadDE(cpu) + 1);
    } break;
    case 0x23: { // inc $hl
        clock_increment(gb);
        WriteHL(cpu, ReadHL(cpu) + 1);
    } break;
    case 0x33: { // inc $sp
        clock_increment(gb);
        cpu->SP += 1;
    } break;
    case 0x0B: { // dec $bc
        clock_increment(gb);
        WriteBC(cpu, ReadBC(cpu) - 1);
    } break;
    case 0x1B: { // dec $de
        clock_increment(gb);
        WriteDE(cpu, ReadDE(cpu) - 1);
    } break;
    case 0x2B: { // dec $hl
        clock_increment(gb);
        WriteHL(cpu, ReadHL(cpu) - 1);
    } break;
    case 0x3B: { // dec $sp
        clock_increment(gb);
        cpu->SP -= 1;
    } break;
    case 0x09: { // add $hl, $bc
        clock_increment(gb);
        WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadBC(cpu)));
    } break;
    case 0x19: { // add $hl, $de
        clock_increment(gb);
        WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadDE(cpu)));
    } break;
    case 0x29: { // add $hl, $hl
        clock_increment(gb);
        WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadHL(cpu)));
    } break;
    case 0x39: { // add $hl, $sp
        clock_increment(gb);
        WriteHL(cpu, Add16(cpu, ReadHL(cpu), cpu->SP));
    } break;
    case 0xE8: { // add $sp, imm8i
        uint16_t ea = cpu->SP + Imm8i(gb);
        clock_increment(gb);
        clock_increment(gb);
        UpdateZNHC(cpu, false, false, (ea & 0xF) < (cpu->SP & 0xF), (ea & 0xFF) < (cpu->SP & 0xFF));
        cpu->SP = ea;
    } break;
    case 0x34: { // inc ($hl)
        uint16_t addr = ReadHL(cpu);
        mmu_write(gb, addr, Inc8(cpu, mmu_read(gb, addr)));
    } break;
    case 0x35: { // dec ($hl)
        uint16_t addr = ReadHL(cpu);
        mmu_write(gb, addr, Dec8(cpu, mmu_read(gb, addr)));
    } break;
    case 0xE9: cpu->PC = ReadHL(cpu); break; // jp $hl
    case 0xC3: {// jp imm16
     clock_increment(gb);
     Jump(cpu, Imm16(gb));
    } break;
    case 0xC2: {// jp nz, imm16
        uint16_t addr = Imm16(gb);
        clock_increment(gb);
        if(!ReadZ(cpu)) {
          Jump(cpu, addr);
        }
    } break;
    case 0xCA: {// jp z, imm16
        uint16_t addr = Imm16(gb);
        clock_increment(gb);
        if(ReadZ(cpu)) {
          Jump(cpu, addr);
        }
    } break;
    case 0xD2: {// jp nc, imm16
        uint16_t addr = Imm16(gb);
        clock_increment(gb);
        if(!ReadC(cpu)) {
          Jump(cpu, addr);
        }
    } break;
    case 0xDA: {// jp c, imm16
        uint16_t addr = Imm16(gb);
        clock_increment(gb);
        if(ReadC(cpu)) {
          Jump(cpu, addr);
        }
    } break;
    case 0x18: {// jr imm8
      clock_increment(gb);
      JumpRel(cpu, Imm8i(gb));
    } break;
    case 0x20: {// jr nz, imm8
        uint16_t off = Imm8i(gb);
        clock_increment(gb);
        if(!ReadZ(cpu)) {
          JumpRel(cpu, off);
        }
    } break;
    case 0x28: {// jr z, imm8i
        uint16_t off = Imm8i(gb);
        clock_increment(gb);
        if(ReadZ(cpu)) {
          JumpRel(cpu, off);
        }
    } break;
    case 0x30: {// jr nc, imm8i
        uint16_t off = Imm8i(gb);
        clock_increment(gb);
        if(!ReadC(cpu)) {
          JumpRel(cpu, off);
        }
    } break;
    case 0x38: {// jr c, imm8i
        uint16_t off = Imm8i(gb);
        clock_increment(gb);
        if(ReadC(cpu)) {
          JumpRel(cpu, off);
        }
    } break;
    case 0xC4: { // call nz, imm16
          uint16_t addr = Imm16(gb);
          clock_increment(gb);
        if(!ReadZ(cpu)) {
          Call(gb, addr);
        }
    }  break;
    case 0xCC: { // call z, imm16
          uint16_t addr = Imm16(gb);
          clock_increment(gb);
        if(ReadZ(cpu)) {
          Call(gb, addr);
        }
    }  break;
    case 0xD4: { // call nc, imm16
          uint16_t addr = Imm16(gb);
          clock_increment(gb);
        if(!ReadC(cpu)) {
          Call(gb, addr);
        }
    }  break;
    case 0xDC: { // call c, imm16
          uint16_t addr = Imm16(gb);
          clock_increment(gb);
        if(ReadC(cpu)) {
          Call(gb, addr);
        }
    }  break;

    case 0xCD: { // call imm16
        clock_increment(gb);
        Call(gb, Imm16(gb));
    }  break;
    case 0xC7: { // rst 0x00
        clock_increment(gb);
        Call(gb, 0x00);
    } break;
    case 0xCF: { // rst 0x08
        clock_increment(gb);
        Call(gb, 0x08);
    }  break;
    case 0xD7: { // rst 0x10
        clock_increment(gb);
        Call(gb, 0x10);
    }  break;
    case 0xDF: { // rst 0x18
        clock_increment(gb);
        Call(gb, 0x18);
    }  break;
    case 0xE7: { // rst 0x20
        clock_increment(gb);
        Call(gb, 0x20);
    }  break;
    case 0xEF: { // rst 0x28
        clock_increment(gb);
        Call(gb, 0x28);
    }  break;
    case 0xF7: { // rst 0x30
        clock_increment(gb);
        Call(gb, 0x30);
    }  break;
    case 0xFF: { // rst 0x38
        clock_increment(gb);
        Call(gb, 0x38);
    }  break;
    case 0xC9: { // ret
        clock_increment(gb);
        Ret(gb);
    }  break;
    case 0xC0: { // ret nz
        clock_increment(gb);
        if(!ReadZ(cpu)) {
          clock_increment(gb);
          Ret(gb);
        }
    }  break;
    case 0xC8: { // ret z
        clock_increment(gb);
        if(ReadZ(cpu)) {
          clock_increment(gb);
          Ret(gb);
        }
    }  break;
    case 0xD0: { // ret nc
        clock_increment(gb);
        if(!ReadC(cpu)) {
          clock_increment(gb);
          Ret(gb);
        }
    }  break;
    case 0xD8: { // ret c
        clock_increment(gb);
        if(ReadC(cpu)) {
          clock_increment(gb);
          Ret(gb);
        }
    }  break;
    case 0xD9: { // reti
        clock_increment(gb);
        Ret(gb);
        cpu->InterruptsEnabled = true;
    } break;
    case 0x00: break; // nop
    case 0xF3: {
        cpu->InterruptsEnabled = false;
        cpu->InterruptEnablePending = false;
    }
    break; // di
    case 0xFB: cpu->InterruptEnablePending = true; break; // ei
    case 0x76: { // halt
        cpu->Halted = true;
        if(cpu->InterruptsEnabled == 0) {
            cpu->HaltBug = true;
        }
    } break;
    case 0x27: { // daa
        uint16_t a = cpu->A;
        if(ReadN(cpu)) {
            if(ReadH(cpu)) {
                a -= 0x06;
                a &= 0xFF;
            }
            if(ReadC(cpu)) {
                a -= 0x60;
            }
        }
        else {
            if((a & 0x0F) > 0x09 || ReadH(cpu)) {
                a += 0x06;
            }
            if(a > 0x9F || ReadC(cpu)) {
                a += 0x60;
            }
        }
        UpdateZ(cpu, (a & 0xFF) == 0);
        UpdateH(cpu, false);
        if(a & 0x100) { UpdateC(cpu, true); }
        cpu->A = a;
    } break;
    case 0x10: { // stop 0
        // TODO STOP
    } break;
    case 0x37: { // scf
        UpdateN(cpu, false);
        UpdateH(cpu, false);
        UpdateC(cpu, true);
    } break;
    case 0x3F: { // ccf
        UpdateN(cpu, false);
        UpdateH(cpu, false);
        UpdateC(cpu, !ReadC(cpu));
    } break;

    }
}
uint8_t const BootROM[256] = {
    0x31,0xFE,0xFF,0xAF,0x21,0xFF,0x9F,0x32,0xCB,0x7C,0x20,0xFB,0x21,0x26,0xFF,0x0E,
    0x11,0x3E,0x80,0x32,0xE2,0x0C,0x3E,0xF3,0xE2,0x32,0x3E,0x77,0x77,0x3E,0xFC,0xE0,
    0x47,0x11,0x04,0x01,0x21,0x10,0x80,0x1A,0xCD,0x95,0x00,0xCD,0x96,0x00,0x13,0x7B,
    0xFE,0x34,0x20,0xF3,0x11,0xD8,0x00,0x06,0x08,0x1A,0x13,0x22,0x23,0x05,0x20,0xF9,
    0x3E,0x19,0xEA,0x10,0x99,0x21,0x2F,0x99,0x0E,0x0C,0x3D,0x28,0x08,0x32,0x0D,0x20,
    0xF9,0x2E,0x0F,0x18,0xF3,0x67,0x3E,0x64,0x57,0xE0,0x42,0x3E,0x91,0xE0,0x40,0x04,
    0x1E,0x02,0x0E,0x0C,0xF0,0x44,0xFE,0x90,0x20,0xFA,0x0D,0x20,0xF7,0x1D,0x20,0xF2,
    0x0E,0x13,0x24,0x7C,0x1E,0x83,0xFE,0x62,0x28,0x06,0x1E,0xC1,0xFE,0x64,0x20,0x06,
    0x7B,0xE2,0x0C,0x3E,0x87,0xE2,0xF0,0x42,0x90,0xE0,0x42,0x15,0x20,0xD2,0x05,0x20,
    0x4F,0x16,0x20,0x18,0xCB,0x4F,0x06,0x04,0xC5,0xCB,0x11,0x17,0xC1,0xCB,0x11,0x17,
    0x05,0x20,0xF5,0x22,0x23,0x22,0x23,0xC9,0xCE,0xED,0x66,0x66,0xCC,0x0D,0x00,0x0B,
    0x03,0x73,0x00,0x83,0x00,0x0C,0x00,0x0D,0x00,0x08,0x11,0x1F,0x88,0x89,0x00,0x0E,
    0xDC,0xCC,0x6E,0xE6,0xDD,0xDD,0xD9,0x99,0xBB,0xBB,0x67,0x63,0x6E,0x0E,0xEC,0xCC,
    0xDD,0xDC,0x99,0x9F,0xBB,0xB9,0x33,0x3E,0x3C,0x42,0xB9,0xA5,0xB9,0xA5,0x42,0x3C,
    0x21,0x04,0x01,0x11,0xA8,0x00,0x1A,0x13,0xBE,0x20,0xFE,0x23,0x7D,0xFE,0x34,0x20,
    0xF5,0x06,0x19,0x78,0x86,0x23,0x05,0x20,0xFB,0x86,0x20,0xFE,0x3E,0x01,0xE0,0x50
};
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

static void mmu_writeRTC(struct Gameboy* gb, uint8_t reg, uint8_t val)
{
    if(reg <= 0x0C) {
        mmu_updateRTC(&(gb->mem.rtc));
        gb->mem.rtc.BaseReg[reg - 0x08] = val;
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
static uint8_t mmu_readDirect(struct Memory* mem, uint16_t addr) {
    if ((addr <= 0x00FF) && mem->BootROMEnabled) {
        return BootROM[addr];
    } else if (addr < 0x4000) {
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
        /* 8K - Internal RAM */
        return mem->WorkRAM[addr - 0xC000];
    } else if (addr < 0xFE00) {
        /* Mirror of internal RAM */
        return mem->WorkRAM[addr - 0xE000];
    } else if (addr < 0xFE9F) {
        /* OAM */
        return mem->OAM[addr - 0xFE00];
    } else if (addr < 0xFF00) {
        /* Empty */
        return 0x00;
    } else if (addr < 0xFF80) {
        /* IO registers */
        return mem->IO[addr - 0xFF00];// | IOUnusedBits[addr - 0xFF00];
    } else if(addr < 0xFFFF) {
        return mem->HighRAM[addr - 0xFF80];
    } else {
        return mem->InterruptEnable;
    }
}

static void mmu_writeBankedRAM(struct Gameboy* gb, unsigned int relativeAddress, uint8_t data)
{
    if(gb->mem.MBCModel == Cart_MBC3 && gb->mem.MBCRAMBank >= Cart_MBC3_RTCBase) {
        mmu_writeRTC(gb, gb->mem.MBCRAMBank, data);
    }
    else if(gb->mem.CartRAMBankEnabled) {
        unsigned int cartAddr = (gb->mem.MBCRAMBank * 8192) + relativeAddress;
        if(cartAddr < gb->mem.CartRAMSize) {
            if(gb->mem.MBCModel == Cart_MBC2) {
                // MBC2 internal RAM is 4bit
                data &= 0x0F;
            }
            gb->mem.CartRAM[cartAddr] = data;
        }
    }
}

static void mmu_setROMBank(struct Gameboy* gb, unsigned int addr, uint8_t data)
{
    switch(gb->mem.MBCModel) {
        case Cart_MBC1_16_8:
        case Cart_MBC1_4_32:
            {
                /* Bottom 5 bits of ROM Bank number */
                unsigned int bankNo = data & 0x1F;
                /* Zero in this register always maps to 1 */
                if(bankNo == 0) {
                    bankNo = 1;
                }
                gb->mem.MBCROMBank = (gb->mem.MBCROMBank & 0xE0) | bankNo;
            }
            break;
        case Cart_MBC2:
            {
                unsigned int bankNo = data & 0x0F;
                gb->mem.MBCROMBank = bankNo? bankNo : 1;
            }
            break;
        case Cart_MBC3:
            {
                unsigned int bankNo = data & 0x7F;
                gb->mem.MBCROMBank = bankNo? bankNo : 1;
            }
            break;
        case Cart_MBC5:
            if(addr < 0x3000) {
                gb->mem.MBCROMBank = ((gb->mem.MBCROMBank & ~0xFF) | data);
            }
            else {
                gb->mem.MBCROMBank = ((gb->mem.MBCROMBank & 0xFF) | ((data & 1u) << 9u));
            }
            break;

        default:
            break;
    }
}

static void mmu_setRAMBank(struct Gameboy* gb, unsigned int addr, uint8_t data)
{
    switch(gb->mem.MBCModel) {
        case Cart_MBC1_16_8:
            gb->mem.MBCROMBank = ((gb->mem.MBCROMBank & 0x1F) | ((data & 0x3) << 5u));
            break;
        case Cart_MBC1_4_32:
            gb->mem.MBCRAMBank = (data & 0x3);
            break;
        case Cart_MBC3:
            gb->mem.MBCRAMBank = data;
            break;
        case Cart_MBC5:
            /* TODO Rumble is controlled by bit 4 */
            gb->mem.MBCRAMBank = (data & 0x0F);
            break;

        default:
            break;
    }
}

uint8_t mmu_read(struct Gameboy* gb, int addr) {
    clock_increment(gb);

    if(gb->dma.Active && addr < 0xFF80) {
        /* When OAM DMA is in progress any memory accesses outside of
         * high RAM (0xFF80 - 0xFFFE) will return 0xFF */
        return 0xFF;
    }
    else {
        return mmu_readDirect(&(gb->mem), addr);
    }
}

static void mmu_writeDirect(struct Gameboy* gb, uint16_t addr, uint8_t value)
{
    if (addr < 0x2000) {
        /* Cart RAM enable */
        gb->mem.CartRAMBankEnabled = (value & 0xF) == 0xA;
    }
    else if(addr < 0x4000) {
        /* ROM Bank select */
        mmu_setROMBank(gb, addr, value);
    }
    else if(addr < 0x6000) {
        /* RAM Bank select (or high bits of ROM Bank for MBC1 mode 16/8) */
        mmu_setRAMBank(gb, addr, value);
    }
    else if(addr < 0x8000) {
        /* MBC1 Mode selection or MBC3 RTC latching */
        if(gb->mem.MBCModel == Cart_MBC1_16_8 || gb->mem.MBCModel == Cart_MBC1_4_32) {
            //<< mbc1 model selection >>
        }
        else if(gb->mem.MBCModel == Cart_MBC3) {
            if(value == 0x00 && gb->mem.rtc.Latched) {
                gb->mem.rtc.Latched = false;
            }
            else if(value == 0x01 && !gb->mem.rtc.Latched) {
                mmu_updateRTC(&(gb->mem.rtc));
                for(unsigned i = 0; i < 5; i += 1) {
                    gb->mem.rtc.LatchedReg[i] = gb->mem.rtc.BaseReg[i];
                }
                gb->mem.rtc.Latched = true;
            }
        }
    } else if (addr < 0xA000) {
        /* Video RAM */
        // TODO: Writes to VRAM should be ignored when the LCD is being redrawn
        gb->mem.VideoRAM[addr - 0x8000] = value;
    } else if (addr < 0xC000) {
        /* Banked RAM Area */
        mmu_writeBankedRAM(gb, addr - 0xA000, value);
    } else if (addr < 0xE000) {
        /* Internal RAM */
        gb->mem.WorkRAM[addr - 0xC000] = value;
    } else if (addr < 0xFE00) {
        /* Mirror of internal RAM */
        gb->mem.WorkRAM[addr - 0xE000] = value;
    } else if (addr < 0xFE9F) {
        /* OAM */
        gb->mem.OAM[addr - 0xFE00] = value;
    } else if (addr < 0xFF00) {
        /* Empty */
    } else if (addr < 0xFF80) {
        /* IO registers */
        switch(addr - 0xFF00) {
            case IO_TimerCounter:
                /* Writes to the timer counter whilst it is loading are ignored */
                if(!gb->clock.TimerLoading) {
                    gb->mem.IO[IO_TimerCounter] = value;
                    /* Writing to timer counter suppresses any pending overflow effects */
                    gb->clock.TimerOverflow = false;
                }
                break;
            case IO_TimerModulo:
                gb->mem.IO[IO_TimerModulo] = value;
                /* Whilst the modulo is being loaded any writes are effective immediately */
                if(gb->clock.TimerLoading) {
                    gb->mem.IO[IO_TimerCounter] = value;
                }
            case IO_TimerControl:
                clock_updateTimerControl(&(gb->clock), &(gb->mem), value);
                break;
            case IO_Divider:
                clock_countChange(&(gb->clock), &(gb->mem), 0);
                break;
            case IO_InterruptFlag:
                /* Top 5 bits of IF always read 1s */
                gb->mem.IO[IO_InterruptFlag] = value | 0xE0;
                break;
            case IO_BootROMDisable: /* Writing to this address disables the boot ROM */
                {
                    gb->mem.BootROMEnabled = false;
                }
                break;
            case IO_OAMDMA: /* LCD OAM DMA transfer */
                {
                    if(value <= 0xF1) {
                        gb->dma.PendingSource = value;
                        gb->dma.DelayStart = true;
                    } else {
                        assert(false && "Invalid LCD OAM transfer range");
                    }
                }
                break;
            case IO_LCDStat:
                {
                    uint8_t cur = gb->mem.IO[IO_LCDStat];
                    gb->mem.IO[IO_LCDStat] = (cur & 0x3) | (value & ~0x3);
                }
                break;

            case IO_LCDY: /* Current scanline -> writing resets it to zero */
                {
                    gb->mem.IO[IO_LCDY] = 0;
                }
                break;

            default: gb->mem.IO[addr - 0xFF00] = value; break;
        }
    } else if (addr < 0xFFFF) {
        gb->mem.HighRAM[addr - 0xFF80] = value;
    } else {
        gb->mem.InterruptEnable = value;
    }
}

void mmu_write(struct Gameboy* gb, int addr, uint8_t value) {
    clock_increment(gb);

    /* TODO is access to IO space (0xFF00 - 0xFF7F) OK? */
    if(gb->dma.Active && addr < 0xFF00) {
        /* When OAM DMA is in progress any memory writes outside of
         * high RAM (0xFF80 - 0xFFFE) will be ignored */
    }
    else {
        mmu_writeDirect(gb, addr, value);
    }
}

uint8_t gameboy_read(struct Gameboy* gb, uint16_t addr)
{
    return mmu_readDirect(&(gb->mem), addr);
}

void gameboy_write(struct Gameboy* gb, uint16_t addr, uint8_t value)
{
    mmu_writeDirect(gb, addr, value);
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
void video_update(struct Gameboy* gb) {
    /* Each scanline takes 456 cycles to draw */
    uint8_t scanline = gb->lcd.FrameProgress / 456;
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
void input_setUp(struct Gameboy* gb, int button)
{
    gb->buttons.Pressed &= ~button;
}
void input_setDown(struct Gameboy* gb, int button)
{
    if((gb->buttons.Pressed & button) == 0) {
        gb->buttons.Pressed |= button;
        gb->mem.IO[IO_InterruptFlag] |= Interrupt_Joypad;
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
        input_setDown(gb, button);
    }
    else {
        input_setUp(gb, button);
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
    /* Either start executing the boot ROM or the Cart code. */
    if(gb->mem.BootROMEnabled == 1) {
        gb->cpu.PC = 0;
    } else {
        gb->cpu.PC = 0x100;
    }
    gb->cpu.SP = 0xFFFE;

    // Taken from The Cycle Accurate GB Doc
    gb->cpu.A = 0x01;
    gb->cpu.F = 0xB0;
    gb->cpu.B = 0x00;
    gb->cpu.C = 0x13;
    gb->cpu.D = 0x00;
    gb->cpu.E = 0xD8;
    gb->cpu.H = 0x01;
    gb->cpu.L = 0x4D;

    gb->cpu.InterruptsEnabled = false;
    gb->cpu.InterruptEnablePending = false;
    gb->cpu.Halted = false;
    gb->cpu.HaltBug = false;
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
    gb->mem.InterruptEnable = 0x00;

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

uint8_t getIflag(struct Memory* mem) {
  return mem->IO[IO_InterruptFlag];
}

uint8_t getInterruptEnable(struct Memory* mem) {
  return mem->InterruptEnable;
}

void setIF(struct Memory* mem, uint8_t iflag) {
  mem->IO[IO_InterruptFlag] = iflag;
}

struct Buttons* getButtons(struct Gameboy* gb) {
  return &(gb->buttons);
}

struct Cpu* getCpu(struct Gameboy* gb) {
  return &(gb->cpu);
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
