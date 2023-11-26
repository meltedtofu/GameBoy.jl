module GameBoy

const gblib = abspath(joinpath(@__DIR__,
                               "..",
                               "deps",
                               "emulator",
                               Sys.iswindows() ? "gameboy.dll"
                                               : (Sys.isapple() ? "libgameboy.dylib"
                                                                : "libgameboy.so")))

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

"""
Main processor
"""
mutable struct Cpu
    A::UInt8
    F::UInt8
    B::UInt8
    C::UInt8
    D::UInt8
    E::UInt8
    H::UInt8
    L::UInt8
    SP::UInt16
    PC::UInt16
    InterruptsEnabled::Bool
    InterruptEnablePending::Bool
    Halted::Bool
    HaltBug::Bool
end

"""
The state of an emulator.
"""
struct Emulator
    g::Ptr{Cvoid}
    frameStride::Int

    Emulator() = new(ccall((:gameboy_alloc, gblib), Ptr{Cvoid}, ()),
                     ccall((:gameboy_frame_stride, gblib), Int, ()),
                    )
end

"""
All of the buttons that can be pressed
"""
@enum Button ButtonDown=0x80 ButtonUp=0x40 ButtonLeft=0x20 ButtonRight=0x10 ButtonStart=0x08 ButtonSelect=0x04 ButtonB=0x02 ButtonA=0x01 NoOp=0x00
export Button
@exportinstances Button

# Guarantee that a, b, start, select button presses are always released on the next frame.
# Without this toggle button presses can result in a stuck emulator.
function execute(suggested::Button, gb::Emulator, release::Ref{Bool})
    if suggested == ButtonA || suggested == ButtonB || suggested == ButtonStart || suggested == ButtonSelect
        execute(release[] ? NoOp : suggested, gb)
        release[] = !release[]
    else
        execute(suggested, gb)
    end
end

"""
Press a button in the emulator.
"""
function execute(suggested::Button, gb::Emulator)
    buttonstate!(gb, ButtonUp, false)
    buttonstate!(gb, ButtonDown, false)
    buttonstate!(gb, ButtonLeft, false)
    buttonstate!(gb, ButtonRight, false)
    buttonstate!(gb, ButtonA, false)
    buttonstate!(gb, ButtonB, false)
    buttonstate!(gb, ButtonStart, false)
    buttonstate!(gb, ButtonSelect, false)

    if suggested == NoOp
        return
    end

    buttonstate!(gb, suggested, true)
end

export execute


"""
Release the memory backing the Emulator.
"""
function free!(gb::Emulator)
    ccall((:gameboy_free, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
end

"""
Open a file on disk as a ROM.
"""
function loadrom!(gb::Emulator, path::String; skip_checksum::Bool=false)
    ccall((:gameboy_load_rom, gblib), Cvoid, (Ptr{Cvoid}, Cstring), gb.g, path)
    result = ccall((:gameboy_load, gblib), Cstring, (Ptr{Cvoid},Cint), gb.g, skip_checksum)
    if result != C_NULL
        free!(gb)
        error(unsafe_string(result))
    end
    nothing
end

# TODO: Use `unsafe_load` to directly deal with types from julia instead of requiring helper functions in the emulator library.

"""
Grab a copy of the cartridge RAM.
This captures the save state.
Use `ram!` to restore this state.
"""
function ram(gb::Emulator)::Vector{UInt8}
    len = ccall((:gameboy_cart_ram_size, gblib), UInt, (Ptr{Cvoid},), gb.g)
    cbytes = ccall((:gameboy_cart_ram, gblib), Ptr{UInt8}, (Ptr{Cvoid},), gb.g)
    deepcopy(unsafe_wrap(Vector{UInt8}, cbytes, (len)))
end

"""
Restore cartridge RAM.
Use `ram` to capture the state.
"""
function ram!(gb::Emulator, data::Vector{UInt8})
    ccall((:gameboy_load_ram, gblib), Cvoid, (Ptr{Cvoid}, UInt, Ptr{UInt8}), gb.g, length(data), data)
    nothing
end

"""
Power cycle the emulator
"""
function reset!(gb::Emulator)
    ccall((:gameboy_reset, gblib), Cvoid, (Ptr{Cvoid}, Bool), gb.g, true)
    nothing
end

function handleInterrupts(gb::Emulator, cpup::Ptr{Cpu}, mem::Ptr{Cvoid})
    iflag = ccall((:getIflag, gblib), UInt8, (Ptr{Cvoid},), mem)
    ie = ccall((:getInterruptEnable, gblib), UInt8, (Ptr{Cvoid},), mem)
    mask = 0x1f
    irqs = ie & iflag & mask
    if irqs > 0
        cpu = unsafe_load(cpup)
        cpu.Halted = false
        unsafe_store!(cpup, cpu)
        if cpu.InterruptsEnabled
            for i ∈ 0:5
                bit = 0x01 << i
                if irqs & bit > 0
                    cpu = unsafe_load(cpup)
                    cpu.InterruptsEnabled = false
                    unsafe_store!(cpup, cpu)
                    ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
                    ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
                    ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
                    ccall((:Call, gblib), Cvoid, (Ptr{Cvoid}, UInt16), gb.g, 0x40 + i*8)
                    iflag &= ~bit
                    break
                end
            end
            ccall((:setIF, gblib), Cvoid, (Ptr{Cvoid}, UInt8), mem, iflag)
        end
    end
end

function cpu_step(gb::Emulator, cpup::Ptr{Cpu})
    # NOTE: the mo for this port is top down. move it over section by section and then ccall the library with the rest of the lines.

    cpu = unsafe_load(cpup)

    if cpu.Halted
        ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
        return
    end

    if cpu.InterruptEnablePending
        cpu.InterruptsEnabled = true
        cpu.InterruptEnablePending = false
        unsafe_store!(cpup, cpu)
    end
#
#     uint8_t const opcode = Imm8(gb);
    opcode = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
    cpu = unsafe_load(cpup)

    if cpu.HaltBug
        cpu.HaltBug = false
        cpu.PC -= 1
        unsafe_store!(cpup, cpu)
    end

    if opcode == 0x00
        # nop
    elseif opcode == 0x40
        cpu.B = cpu.B
    elseif opcode == 0x41
        cpu.B = cpu.C
    elseif opcode == 0x42
        cpu.B = cpu.D
    elseif opcode == 0x43
        cpu.B = cpu.E
    elseif opcode == 0x44
        cpu.B = cpu.H
    elseif opcode == 0x45
        cpu.B = cpu.L
    elseif opcode == 0x47
        cpu.B = cpu.A
    elseif opcode == 0x48
        cpu.C = cpu.B
    elseif opcode == 0x49
        cpu.C = cpu.C
    elseif opcode == 0x4a
        cpu.C = cpu.D
    elseif opcode == 0x4b
        cpu.C = cpu.E
    elseif opcode == 0x4c
        cpu.C = cpu.H
    elseif opcode == 0x4d
        cpu.C = cpu.L
    elseif opcode == 0x4f
        cpu.C = cpu.A
    elseif opcode == 0x50
        cpu.D = cpu.B
    elseif opcode == 0x51
        cpu.D = cpu.C
    elseif opcode == 0x52
        cpu.D = cpu.D
    elseif opcode == 0x53
        cpu.D = cpu.E
    elseif opcode == 0x54
        cpu.D = cpu.H
    elseif opcode == 0x55
        cpu.D = cpu.L
    elseif opcode == 0x57
        cpu.D = cpu.A
    elseif opcode == 0x58
        cpu.E = cpu.B
    elseif opcode == 0x59
        cpu.E = cpu.C
    elseif opcode == 0x5a
        cpu.E = cpu.D
    elseif opcode == 0x5b
        cpu.E = cpu.E
    elseif opcode == 0x5c
        cpu.E = cpu.H
    elseif opcode == 0x5d
        cpu.E = cpu.L
    elseif opcode == 0x5f
        cpu.E = cpu.A
    elseif opcode == 0x60
        cpu.H = cpu.B
    elseif opcode == 0x61
        cpu.H = cpu.C
    elseif opcode == 0x62
        cpu.H = cpu.D
    elseif opcode == 0x63
        cpu.H = cpu.E
    elseif opcode == 0x64
        cpu.H = cpu.H
    elseif opcode == 0x65
        cpu.H = cpu.L
    elseif opcode == 0x67
        cpu.H = cpu.A
    elseif opcode == 0x68
        cpu.L = cpu.B
    elseif opcode == 0x69
        cpu.L = cpu.C
    elseif opcode == 0x6a
        cpu.L = cpu.D
    elseif opcode == 0x6b
        cpu.L = cpu.E
    elseif opcode == 0x6c
        cpu.L = cpu.H
    elseif opcode == 0x6d
        cpu.L = cpu.L
    elseif opcode == 0x6f
        cpu.L = cpu.A
    elseif opcode == 0x78
        cpu.A = cpu.B
    elseif opcode == 0x79
        cpu.A = cpu.C
    elseif opcode == 0x7a
        cpu.A = cpu.D
    elseif opcode == 0x7b
        cpu.A = cpu.E
    elseif opcode == 0x7c
        cpu.A = cpu.H
    elseif opcode == 0x7d
        cpu.A = cpu.L
    elseif opcode == 0x7f
        cpu.A = cpu.A
    end

    unsafe_store!(cpup, cpu)
#
#     // ld $reg8, ($hl)
#     case 0x46: cpu->B = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x4E: cpu->C = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x56: cpu->D = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x5E: cpu->E = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x66: cpu->H = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x6E: cpu->L = mmu_read(gb, ReadHL(cpu)); break;
#     case 0x7E: cpu->A = mmu_read(gb, ReadHL(cpu)); break;
#
#     // ld ($hl), $reg8
#     case 0x70: mmu_write(gb, ReadHL(cpu), cpu->B); break;
#     case 0x71: mmu_write(gb, ReadHL(cpu), cpu->C); break;
#     case 0x72: mmu_write(gb, ReadHL(cpu), cpu->D); break;
#     case 0x73: mmu_write(gb, ReadHL(cpu), cpu->E); break;
#     case 0x74: mmu_write(gb, ReadHL(cpu), cpu->H); break;
#     case 0x75: mmu_write(gb, ReadHL(cpu), cpu->L); break;
#     case 0x77: mmu_write(gb, ReadHL(cpu), cpu->A); break;
#     case 0x36: mmu_write(gb, ReadHL(cpu), Imm8(gb)); break; // ld ($hl), imm8
#     // ld $a, ($reg16)
#     case 0x0A: cpu->A = mmu_read(gb, ReadBC(cpu)); break;
#     case 0x1A: cpu->A = mmu_read(gb, ReadDE(cpu)); break;
#
#     // ld ($reg16), $a
#     case 0x02: mmu_write(gb, ReadBC(cpu), cpu->A); break;
#     case 0x12: mmu_write(gb, ReadDE(cpu), cpu->A); break;
#     case 0xEA: mmu_write(gb, Imm16(gb), cpu->A); break; // ld (imm16), $a
#     case 0xFA: cpu->A = mmu_read(gb, Imm16(gb)); break; // ld $a, (imm16)
#     case 0x22: // ld ($hl+), $a
#         mmu_write(gb, ReadHL(cpu), cpu->A);
#         WriteHL(cpu, ReadHL(cpu) + 1);
#         break;
#     case 0x2A: // ld $a, ($hl+)
#         cpu->A = mmu_read(gb, ReadHL(cpu));
#         WriteHL(cpu, ReadHL(cpu) + 1);
#         break;
#     case 0x32: // ld ($hl-), $a
#         mmu_write(gb, ReadHL(cpu), cpu->A);
#         WriteHL(cpu, ReadHL(cpu) - 1);
#         break;
#     case 0x3A: // ld $a, ($hl-)
#         cpu->A = mmu_read(gb, ReadHL(cpu));
#         WriteHL(cpu, ReadHL(cpu) - 1);
#         break;
#     case 0xE0: mmu_write(gb, 0xFF00 + Imm8(gb), cpu->A); break; // ld (0xFF00 + imm8), $a
#     case 0xE2: mmu_write(gb, 0xFF00 + cpu->C, cpu->A); break; // ld (0xFF00 + $c), $a
#     case 0xF0: cpu->A = mmu_read(gb, 0xFF00 + Imm8(gb)); break; // ld $a, (0xFF00 + imm8)
#     case 0xF2: cpu->A = mmu_read(gb, 0xFF00 + cpu->C); break; // ld $a, (0xFF00 + $c)
#     // ld $reg8, imm8
#     case 0x06: cpu->B = Imm8(gb); break;
#     case 0x0E: cpu->C = Imm8(gb); break;
#     case 0x16: cpu->D = Imm8(gb); break;
#     case 0x1E: cpu->E = Imm8(gb); break;
#     case 0x26: cpu->H = Imm8(gb); break;
#     case 0x2E: cpu->L = Imm8(gb); break;
#     case 0x3E: cpu->A = Imm8(gb); break;
#     // ld $reg16, imm16
#     case 0x01: WriteBC(cpu, Imm16(gb)); break;
#     case 0x11: WriteDE(cpu, Imm16(gb)); break;
#     case 0x21: WriteHL(cpu, Imm16(gb)); break;
#     case 0x31: cpu->SP = Imm16(gb); break;
#     case 0x08: { // ld (imm16), $sp
#         uint16_t addr = Imm16(gb);
#         mmu_write(gb, addr, (cpu->SP & 0xFF));
#         mmu_write(gb, addr + 1, (cpu->SP >> 8u));
#     } break;
#     case 0xF9: // ld $sp, $hl
#         clock_increment(gb);
#         cpu->SP = ReadHL(cpu);
#         break;
#     case 0xF8: { // ld $hl, $sp + imm8
#         uint16_t ea = cpu->SP + Imm8i(gb);
#         clock_increment(gb);
#         WriteHL(cpu, ea);
#         UpdateZNHC(cpu, false, false, (ea & 0xF) < (cpu->SP & 0xF), (ea & 0xFF) < (cpu->SP & 0xFF));
#     } break;
#     // pop $reg16
#     case 0xC1: WriteBC(cpu, Pop16(gb)); break;
#     case 0xD1: WriteDE(cpu, Pop16(gb)); break;
#     case 0xE1: WriteHL(cpu, Pop16(gb)); break;
#     case 0xF1: WriteAF(cpu, Pop16(gb)); break;
#
#     // push $reg16
#     case 0xC5:
#         clock_increment(gb);
#         Push16(gb, ReadBC(cpu));
#         break;
#     case 0xD5:
#         clock_increment(gb);
#         Push16(gb, ReadDE(cpu));
#         break;
#     case 0xE5:
#         clock_increment(gb);
#         Push16(gb, ReadHL(cpu));
#         break;
#     case 0xF5:
#         clock_increment(gb);
#         Push16(gb, ReadAF(cpu));
#         break;
#     // add $a, reg8
#     case 0x80: cpu->A = Add8(cpu, cpu->A, cpu->B, false); break;
#     case 0x81: cpu->A = Add8(cpu, cpu->A, cpu->C, false); break;
#     case 0x82: cpu->A = Add8(cpu, cpu->A, cpu->D, false); break;
#     case 0x83: cpu->A = Add8(cpu, cpu->A, cpu->E, false); break;
#     case 0x84: cpu->A = Add8(cpu, cpu->A, cpu->H, false); break;
#     case 0x85: cpu->A = Add8(cpu, cpu->A, cpu->L, false); break;
#     case 0x87: cpu->A = Add8(cpu, cpu->A, cpu->A, false); break;
#     // add $a, ($hl)
#     case 0x86: cpu->A = Add8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
#     // add $a, imm8
#     case 0xC6: cpu->A = Add8(cpu, cpu->A, Imm8(gb), false); break;
#     // adc $a, reg8
#     case 0x88: cpu->A = Add8(cpu, cpu->A, cpu->B, ReadC(cpu)); break;
#     case 0x89: cpu->A = Add8(cpu, cpu->A, cpu->C, ReadC(cpu)); break;
#     case 0x8A: cpu->A = Add8(cpu, cpu->A, cpu->D, ReadC(cpu)); break;
#     case 0x8B: cpu->A = Add8(cpu, cpu->A, cpu->E, ReadC(cpu)); break;
#     case 0x8C: cpu->A = Add8(cpu, cpu->A, cpu->H, ReadC(cpu)); break;
#     case 0x8D: cpu->A = Add8(cpu, cpu->A, cpu->L, ReadC(cpu)); break;
#     case 0x8F: cpu->A = Add8(cpu, cpu->A, cpu->A, ReadC(cpu)); break;
#     // adc $a, ($hl)
#     case 0x8E: cpu->A = Add8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), ReadC(cpu)); break;
#     // adc $a, imm8
#     case 0xCE: cpu->A = Add8(cpu, cpu->A, Imm8(gb), ReadC(cpu)); break;
#     // sub $a, reg8
#     case 0x90: cpu->A = Sub8(cpu, cpu->A, cpu->B, false); break;
#     case 0x91: cpu->A = Sub8(cpu, cpu->A, cpu->C, false); break;
#     case 0x92: cpu->A = Sub8(cpu, cpu->A, cpu->D, false); break;
#     case 0x93: cpu->A = Sub8(cpu, cpu->A, cpu->E, false); break;
#     case 0x94: cpu->A = Sub8(cpu, cpu->A, cpu->H, false); break;
#     case 0x95: cpu->A = Sub8(cpu, cpu->A, cpu->L, false); break;
#     case 0x97: cpu->A = Sub8(cpu, cpu->A, cpu->A, false); break;
#     // sub $a, ($hl)
#     case 0x96: cpu->A = Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
#     // sub $a, imm8
#     case 0xD6: cpu->A = Sub8(cpu, cpu->A, Imm8(gb), false); break;
#     // sbc $a, reg8
#     case 0x98: cpu->A = Sub8(cpu, cpu->A, cpu->B, ReadC(cpu)); break;
#     case 0x99: cpu->A = Sub8(cpu, cpu->A, cpu->C, ReadC(cpu)); break;
#     case 0x9A: cpu->A = Sub8(cpu, cpu->A, cpu->D, ReadC(cpu)); break;
#     case 0x9B: cpu->A = Sub8(cpu, cpu->A, cpu->E, ReadC(cpu)); break;
#     case 0x9C: cpu->A = Sub8(cpu, cpu->A, cpu->H, ReadC(cpu)); break;
#     case 0x9D: cpu->A = Sub8(cpu, cpu->A, cpu->L, ReadC(cpu)); break;
#     case 0x9F: cpu->A = Sub8(cpu, cpu->A, cpu->A, ReadC(cpu)); break;
#     // sbc $a, ($hl)
#     case 0x9E: cpu->A = Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), ReadC(cpu)); break;
#     // sbc $a, imm8
#     case 0xDE: cpu->A = Sub8(cpu, cpu->A, Imm8(gb), ReadC(cpu)); break;
#     // inc reg8
#     case 0x04: cpu->B = Inc8(cpu, cpu->B); break;
#     case 0x0C: cpu->C = Inc8(cpu, cpu->C); break;
#     case 0x14: cpu->D = Inc8(cpu, cpu->D); break;
#     case 0x1C: cpu->E = Inc8(cpu, cpu->E); break;
#     case 0x24: cpu->H = Inc8(cpu, cpu->H); break;
#     case 0x2C: cpu->L = Inc8(cpu, cpu->L); break;
#     case 0x3C: cpu->A = Inc8(cpu, cpu->A); break;
#     // dec reg8
#     case 0x05: cpu->B = Dec8(cpu, cpu->B); break;
#     case 0x0D: cpu->C = Dec8(cpu, cpu->C); break;
#     case 0x15: cpu->D = Dec8(cpu, cpu->D); break;
#     case 0x1D: cpu->E = Dec8(cpu, cpu->E); break;
#     case 0x25: cpu->H = Dec8(cpu, cpu->H); break;
#     case 0x2D: cpu->L = Dec8(cpu, cpu->L); break;
#     case 0x3D: cpu->A = Dec8(cpu, cpu->A); break;
#     // cp $a, reg8
#     case 0xB8: Sub8(cpu, cpu->A, cpu->B, false); break;
#     case 0xB9: Sub8(cpu, cpu->A, cpu->C, false); break;
#     case 0xBA: Sub8(cpu, cpu->A, cpu->D, false); break;
#     case 0xBB: Sub8(cpu, cpu->A, cpu->E, false); break;
#     case 0xBC: Sub8(cpu, cpu->A, cpu->H, false); break;
#     case 0xBD: Sub8(cpu, cpu->A, cpu->L, false); break;
#     case 0xBF: Sub8(cpu, cpu->A, cpu->A, false); break;
#     // cp $a, ($hl)
#     case 0xBE: Sub8(cpu, cpu->A, mmu_read(gb, ReadHL(cpu)), false); break;
#     // cp $a, imm8
#     case 0xFE: Sub8(cpu, cpu->A, Imm8(gb), false); break;
#     // and/or/xor $a, $reg8
#     case 0xA0: BitAnd(cpu, cpu->B); break;
#     case 0xA1: BitAnd(cpu, cpu->C); break;
#     case 0xA2: BitAnd(cpu, cpu->D); break;
#     case 0xA3: BitAnd(cpu, cpu->E); break;
#     case 0xA4: BitAnd(cpu, cpu->H); break;
#     case 0xA5: BitAnd(cpu, cpu->L); break;
#     case 0xA7: BitAnd(cpu, cpu->A); break;
#     case 0xB0: BitOr(cpu, cpu->B); break;
#     case 0xB1: BitOr(cpu, cpu->C); break;
#     case 0xB2: BitOr(cpu, cpu->D); break;
#     case 0xB3: BitOr(cpu, cpu->E); break;
#     case 0xB4: BitOr(cpu, cpu->H); break;
#     case 0xB5: BitOr(cpu, cpu->L); break;
#     case 0xB7: BitOr(cpu, cpu->A); break;
#     case 0xA8: BitXor(cpu, cpu->B); break;
#     case 0xA9: BitXor(cpu, cpu->C); break;
#     case 0xAA: BitXor(cpu, cpu->D); break;
#     case 0xAB: BitXor(cpu, cpu->E); break;
#     case 0xAC: BitXor(cpu, cpu->H); break;
#     case 0xAD: BitXor(cpu, cpu->L); break;
#     case 0xAF: BitXor(cpu, cpu->A); break;
#
#     // and/or/xor $a, ($hl)
#     case 0xA6: BitAnd(cpu, mmu_read(gb, ReadHL(cpu))); break;
#     case 0xB6: BitOr(cpu, mmu_read(gb, ReadHL(cpu))); break;
#     case 0xAE: BitXor(cpu, mmu_read(gb, ReadHL(cpu))); break;
#
#     // and/or/xor $a, imm8
#     case 0xE6: BitAnd(cpu, Imm8(gb)); break;
#     case 0xF6: BitOr(cpu, Imm8(gb)); break;
#     case 0xEE: BitXor(cpu, Imm8(gb)); break;
#     case 0x07: { // rlca
#         UpdateZNHC(cpu, false, false, false, (cpu->A & 0x80));
#         cpu->A = (cpu->A << 1u) | (cpu->A >> 7u);
#     } break;
#     case 0x0F: { // rrca
#         UpdateZNHC(cpu, false, false, false, (cpu->A & 0x01));
#         cpu->A = (cpu->A >> 1u) | (cpu->A << 7u);
#     } break;
#     case 0x17: { // rla
#         bool c = ReadC(cpu);
#         UpdateZNHC(cpu, false, false, false, (cpu->A & 0x80));
#         cpu->A = (cpu->A << 1u) | (c? 1 : 0);
#     } break;
#     case 0x1F: { // rra
#         bool c = ReadC(cpu);
#         UpdateZNHC(cpu, false, false, false, (cpu->A & 0x01));
#         cpu->A = (cpu->A >> 1u) | (c? 0x80 : 0x00);
#     } break;
#     case 0x2F: { // cpl
#         UpdateN(cpu, true);
#         UpdateH(cpu, true);
#         cpu->A ^= UINT8_MAX;
#     } break;
#     case 0xCB: {
#         cpu_cb_op(gb, cpu);
#     } break;
#     case 0x03: { // inc $bc
#         clock_increment(gb);
#         WriteBC(cpu, ReadBC(cpu) + 1);
#     } break;
#     case 0x13: { // inc $de
#         clock_increment(gb);
#         WriteDE(cpu, ReadDE(cpu) + 1);
#     } break;
#     case 0x23: { // inc $hl
#         clock_increment(gb);
#         WriteHL(cpu, ReadHL(cpu) + 1);
#     } break;
#     case 0x33: { // inc $sp
#         clock_increment(gb);
#         cpu->SP += 1;
#     } break;
#     case 0x0B: { // dec $bc
#         clock_increment(gb);
#         WriteBC(cpu, ReadBC(cpu) - 1);
#     } break;
#     case 0x1B: { // dec $de
#         clock_increment(gb);
#         WriteDE(cpu, ReadDE(cpu) - 1);
#     } break;
#     case 0x2B: { // dec $hl
#         clock_increment(gb);
#         WriteHL(cpu, ReadHL(cpu) - 1);
#     } break;
#     case 0x3B: { // dec $sp
#         clock_increment(gb);
#         cpu->SP -= 1;
#     } break;
#     case 0x09: { // add $hl, $bc
#         clock_increment(gb);
#         WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadBC(cpu)));
#     } break;
#     case 0x19: { // add $hl, $de
#         clock_increment(gb);
#         WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadDE(cpu)));
#     } break;
#     case 0x29: { // add $hl, $hl
#         clock_increment(gb);
#         WriteHL(cpu, Add16(cpu, ReadHL(cpu), ReadHL(cpu)));
#     } break;
#     case 0x39: { // add $hl, $sp
#         clock_increment(gb);
#         WriteHL(cpu, Add16(cpu, ReadHL(cpu), cpu->SP));
#     } break;
#     case 0xE8: { // add $sp, imm8i
#         uint16_t ea = cpu->SP + Imm8i(gb);
#         clock_increment(gb);
#         clock_increment(gb);
#         UpdateZNHC(cpu, false, false, (ea & 0xF) < (cpu->SP & 0xF), (ea & 0xFF) < (cpu->SP & 0xFF));
#         cpu->SP = ea;
#     } break;
#     case 0x34: { // inc ($hl)
#         uint16_t addr = ReadHL(cpu);
#         mmu_write(gb, addr, Inc8(cpu, mmu_read(gb, addr)));
#     } break;
#     case 0x35: { // dec ($hl)
#         uint16_t addr = ReadHL(cpu);
#         mmu_write(gb, addr, Dec8(cpu, mmu_read(gb, addr)));
#     } break;
#     case 0xE9: cpu->PC = ReadHL(cpu); break; // jp $hl
#     case 0xC3: {// jp imm16
#      clock_increment(gb);
#      Jump(cpu, Imm16(gb));
#     } break;
#     case 0xC2: {// jp nz, imm16
#         uint16_t addr = Imm16(gb);
#         clock_increment(gb);
#         if(!ReadZ(cpu)) {
#           Jump(cpu, addr);
#         }
#     } break;
#     case 0xCA: {// jp z, imm16
#         uint16_t addr = Imm16(gb);
#         clock_increment(gb);
#         if(ReadZ(cpu)) {
#           Jump(cpu, addr);
#         }
#     } break;
#     case 0xD2: {// jp nc, imm16
#         uint16_t addr = Imm16(gb);
#         clock_increment(gb);
#         if(!ReadC(cpu)) {
#           Jump(cpu, addr);
#         }
#     } break;
#     case 0xDA: {// jp c, imm16
#         uint16_t addr = Imm16(gb);
#         clock_increment(gb);
#         if(ReadC(cpu)) {
#           Jump(cpu, addr);
#         }
#     } break;
#     case 0x18: {// jr imm8
#       clock_increment(gb);
#       JumpRel(cpu, Imm8i(gb));
#     } break;
#     case 0x20: {// jr nz, imm8
#         uint16_t off = Imm8i(gb);
#         clock_increment(gb);
#         if(!ReadZ(cpu)) {
#           JumpRel(cpu, off);
#         }
#     } break;
#     case 0x28: {// jr z, imm8i
#         uint16_t off = Imm8i(gb);
#         clock_increment(gb);
#         if(ReadZ(cpu)) {
#           JumpRel(cpu, off);
#         }
#     } break;
#     case 0x30: {// jr nc, imm8i
#         uint16_t off = Imm8i(gb);
#         clock_increment(gb);
#         if(!ReadC(cpu)) {
#           JumpRel(cpu, off);
#         }
#     } break;
#     case 0x38: {// jr c, imm8i
#         uint16_t off = Imm8i(gb);
#         clock_increment(gb);
#         if(ReadC(cpu)) {
#           JumpRel(cpu, off);
#         }
#     } break;
#     case 0xC4: { // call nz, imm16
#           uint16_t addr = Imm16(gb);
#           clock_increment(gb);
#         if(!ReadZ(cpu)) {
#           Call(gb, addr);
#         }
#     }  break;
#     case 0xCC: { // call z, imm16
#           uint16_t addr = Imm16(gb);
#           clock_increment(gb);
#         if(ReadZ(cpu)) {
#           Call(gb, addr);
#         }
#     }  break;
#     case 0xD4: { // call nc, imm16
#           uint16_t addr = Imm16(gb);
#           clock_increment(gb);
#         if(!ReadC(cpu)) {
#           Call(gb, addr);
#         }
#     }  break;
#     case 0xDC: { // call c, imm16
#           uint16_t addr = Imm16(gb);
#           clock_increment(gb);
#         if(ReadC(cpu)) {
#           Call(gb, addr);
#         }
#     }  break;
#
#     case 0xCD: { // call imm16
#         clock_increment(gb);
#         Call(gb, Imm16(gb));
#     }  break;
#     case 0xC7: { // rst 0x00
#         clock_increment(gb);
#         Call(gb, 0x00);
#     } break;
#     case 0xCF: { // rst 0x08
#         clock_increment(gb);
#         Call(gb, 0x08);
#     }  break;
#     case 0xD7: { // rst 0x10
#         clock_increment(gb);
#         Call(gb, 0x10);
#     }  break;
#     case 0xDF: { // rst 0x18
#         clock_increment(gb);
#         Call(gb, 0x18);
#     }  break;
#     case 0xE7: { // rst 0x20
#         clock_increment(gb);
#         Call(gb, 0x20);
#     }  break;
#     case 0xEF: { // rst 0x28
#         clock_increment(gb);
#         Call(gb, 0x28);
#     }  break;
#     case 0xF7: { // rst 0x30
#         clock_increment(gb);
#         Call(gb, 0x30);
#     }  break;
#     case 0xFF: { // rst 0x38
#         clock_increment(gb);
#         Call(gb, 0x38);
#     }  break;
#     case 0xC9: { // ret
#         clock_increment(gb);
#         Ret(gb);
#     }  break;
#     case 0xC0: { // ret nz
#         clock_increment(gb);
#         if(!ReadZ(cpu)) {
#           clock_increment(gb);
#           Ret(gb);
#         }
#     }  break;
#     case 0xC8: { // ret z
#         clock_increment(gb);
#         if(ReadZ(cpu)) {
#           clock_increment(gb);
#           Ret(gb);
#         }
#     }  break;
#     case 0xD0: { // ret nc
#         clock_increment(gb);
#         if(!ReadC(cpu)) {
#           clock_increment(gb);
#           Ret(gb);
#         }
#     }  break;
#     case 0xD8: { // ret c
#         clock_increment(gb);
#         if(ReadC(cpu)) {
#           clock_increment(gb);
#           Ret(gb);
#         }
#     }  break;
#     case 0xD9: { // reti
#         clock_increment(gb);
#         Ret(gb);
#         cpu->InterruptsEnabled = true;
#     } break;
#     case 0x00: break; // nop
#     case 0xF3: {
#         cpu->InterruptsEnabled = false;
#         cpu->InterruptEnablePending = false;
#     }
#     break; // di
#     case 0xFB: cpu->InterruptEnablePending = true; break; // ei
#     case 0x76: { // halt
#         cpu->Halted = true;
#         if(cpu->InterruptsEnabled == 0) {
#             cpu->HaltBug = true;
#         }
#     } break;
#     case 0x27: { // daa
#         uint16_t a = cpu->A;
#         if(ReadN(cpu)) {
#             if(ReadH(cpu)) {
#                 a -= 0x06;
#                 a &= 0xFF;
#             }
#             if(ReadC(cpu)) {
#                 a -= 0x60;
#             }
#         }
#         else {
#             if((a & 0x0F) > 0x09 || ReadH(cpu)) {
#                 a += 0x06;
#             }
#             if(a > 0x9F || ReadC(cpu)) {
#                 a += 0x60;
#             }
#         }
#         UpdateZ(cpu, (a & 0xFF) == 0);
#         UpdateH(cpu, false);
#         if(a & 0x100) { UpdateC(cpu, true); }
#         cpu->A = a;
#     } break;
#     case 0x10: { // stop 0
#         // TODO STOP
#     } break;
#     case 0x37: { // scf
#         UpdateN(cpu, false);
#         UpdateH(cpu, false);
#         UpdateC(cpu, true);
#     } break;
#     case 0x3F: { // ccf
#         UpdateN(cpu, false);
#         UpdateH(cpu, false);
#         UpdateC(cpu, !ReadC(cpu));
#     } break;
#
#     }


    # temporarily ccall the library for the missing functionality while porting is in progress
    ccall((:cpu_step, gblib), Cvoid, (Ptr{Cvoid}, Ptr{Cpu}, UInt8), gb.g, cpup, opcode)
end

"""
Run one frame of emulation.
"""
function doframe!(gb::Emulator)::Ptr{Cvoid}
    while true
        mem = ccall((:getMemory, gblib), Ptr{Cvoid}, (Ptr{Cvoid},), gb.g)
        buttons = ccall((:getButtons, gblib), Ptr{Cvoid}, (Ptr{Cvoid},), gb.g)
        lcd = ccall((:getLCD, gblib), Ptr{Cvoid}, (Ptr{Cvoid},), gb.g)
        cpu = ccall((:getCpu, gblib), Ptr{Cpu}, (Ptr{Cvoid},), gb.g)

        ccall((:input_update, gblib), Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), mem, buttons)
        handleInterrupts(gb, cpu, mem)
        cpu_step(gb, cpu)

        pixels = ccall((:updateFrameBuffer, gblib), Ptr{Cvoid}, (Ptr{Cvoid},), lcd);
        if pixels != C_NULL
            return pixels
        end
    end
end

"""
Directly set the state of a button.
"""
function buttonstate!(gb::Emulator, b::Button, pressed::Bool)
    ccall((:gameboy_setButtonState, gblib), Cvoid, (Ptr{Cvoid}, Cint, Bool), gb.g, b, pressed)
    nothing
end

"""
Read an arbitrary byte of memory
"""
function read(gb::Emulator, addr::UInt16)::UInt8
    ccall((:gameboy_read, gblib), UInt8, (Ptr{Cvoid}, Cint), gb.g, addr)
end

"""
Check that the shared library is compiled and callable.
"""
function sanity_check()::UInt8
    ccall((:gameboy_sanity_check, gblib), UInt8, ())
end

export Emulator, free!, loadrom!, reset!, doframe!, buttonstate!, read, ram, ram!

export gblib

end # module GameBoy
