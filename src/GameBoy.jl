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

AF(cpu::Cpu)::UInt16 = UInt16(cpu.H) << 8 | UInt16(cpu.F)
BC(cpu::Cpu)::UInt16 = UInt16(cpu.B) << 8 | UInt16(cpu.C)
DE(cpu::Cpu)::UInt16 = UInt16(cpu.D) << 8 | UInt16(cpu.E)
HL(cpu::Cpu)::UInt16 = UInt16(cpu.H) << 8 | UInt16(cpu.L)
AF!(cpu::Cpu, v::UInt16)::Nothing = begin
    cpu.A = UInt8(v >> 8)
    cpu.F = UInt8(v & 0xf0)
    nothing
end
BC!(cpu::Cpu, v::UInt16)::Nothing = begin
    cpu.B = UInt8(v >> 8)
    cpu.C = UInt8(v & 0xff)
    nothing
end
DE!(cpu::Cpu, v::UInt16)::Nothing = begin
    cpu.D = UInt8(v >> 8)
    cpu.E = UInt8(v & 0xff)
    nothing
end
HL!(cpu::Cpu, v::UInt16)::Nothing = begin
    cpu.H = UInt8(v >> 8)
    cpu.L = UInt8(v & 0xff)
    nothing
end

Z(cpu::Cpu)::Bool = cpu.F & 0x80
N(cpu::Cpu)::Bool = cpu.F & 0x40
H(cpu::Cpu)::Bool = cpu.F & 0x20
C(cpu::Cpu)::Bool = cpu.F & 0x10
Z!(cpu::Cpu, set::Bool)::Nothing = begin
    if set
        cpu.F |= 0x80
    else
        cpu.F &= ~0x80
    end
    nothing
end
N!(cpu::Cpu, set::Bool)::Nothing = begin
    if set
        cpu.F |= 0x40
    else
        cpu.F &= ~0x40
    end
    nothing
end
H!(cpu::Cpu, set::Bool)::Nothing = begin
    if set
        cpu.F |= 0x20
    else
        cpu.F &= ~0x20
    end
    nothing
end
C!(cpu::Cpu, set::Bool)::Nothing = begin
    if set
        cpu.F |= 0x10
    else
        cpu.F &= ~0x10
    end
    nothing
end
ZNHC!(cpu::Cpu, z::Bool, n::Bool, h::Bool, c::Bool)::Nothing = begin
    Z!(cpu, z)
    N!(cpu, n)
    H!(cpu, h)
    C!(cpu, c)
    nothing
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

    opcode = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
    cpu = unsafe_load(cpup)

    if cpu.HaltBug
        cpu.HaltBug = false
        cpu.PC -= 1
        unsafe_store!(cpup, cpu)
    end

    if opcode == 0x00
        # nop
    elseif opcode == 0x01
        val = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        BC!(cpu, val)
    elseif opcode == 0x02
        addr = BC(cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, cpu.A)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x06
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.B = val
    elseif opcode == 0x08
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        val₁ = UInt8(cpu.SP & 0xff)
        val₂ = UInt8(cpu.SP >> 8)
        unsafe_store!(cpup, cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val₁)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr+0x01, val₂)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x0a
        addr = BC(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x0e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.C = val
    elseif opcode == 0x11
        val = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        DE!(cpu, val)
    elseif opcode == 0x12
        addr = DE(cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, cpu.A)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x16
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.D = val
    elseif opcode == 0x1a
        addr = DE(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x1e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.E = val
    elseif opcode == 0x21
        val = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        HL!(cpu, val)
    elseif opcode == 0x22
        addr = HL(cpu)
        val = cpu.A
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
        val = HL(cpu) + 0x01
        HL!(cpu, val)
    elseif opcode == 0x26
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.H = val
    elseif opcode == 0x2a
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
        HL!(cpu, HL(cpu)+0x01)
    elseif opcode == 0x2e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.L = val
    elseif opcode == 0x31
        val = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.SP = val
    elseif opcode == 0x32
        addr = HL(cpu)
        val = cpu.A
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
        HL!(cpu, HL(cpu)-0x01)
    elseif opcode == 0x36
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x3a
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x3e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = val
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
    elseif opcode == 0x46
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.B = val
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
    elseif opcode == 0x4e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.C = val
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
    elseif opcode == 0x56
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.D = val
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
    elseif opcode == 0x5e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.E = val
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
    elseif opcode == 0x66
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.H = val
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
    elseif opcode == 0x6e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.L = val
    elseif opcode == 0x6f
        cpu.L = cpu.A
    elseif opcode == 0x70
        addr = HL(cpu)
        val = cpu.B
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x71
        addr = HL(cpu)
        val = cpu.C
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x72
        addr = HL(cpu)
        val = cpu.D
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x73
        addr = HL(cpu)
        val = cpu.E
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x74
        addr = HL(cpu)
        val = cpu.H
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x75
        addr = HL(cpu)
        val = cpu.L
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x77
        addr = HL(cpu)
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
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
    elseif opcode == 0x7e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x7f
        cpu.A = cpu.A
    elseif opcode == 0xe0
        offset = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        addr = 0xff00 + offset
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xe2
        addr = 0xff00 + cpu.C
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xea
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xf0
        offset = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        addr = 0xff00 + offset
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0xf2
        addr = 0xff00 + cpu.C
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0xf8
        base = cpu.SP
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        addr = base + offset
        ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        HL!(cpu, addr)
        ZNHC!(cpu,
              false,
              false,
              addr & 0xf < cpu.SP & 0xf,
              addr & 0xff < cpu.SP & 0xff)
    elseif opcode == 0xf9
        ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.SP = HL(cpu)
    elseif opcode == 0xfa
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    end

    unsafe_store!(cpup, cpu)

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
