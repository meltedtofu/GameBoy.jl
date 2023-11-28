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

clock_increment(gb::Emulator) = ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)

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
                    clock_increment(gb)
                    clock_increment(gb)
                    clock_increment(gb)
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

Z(cpu::Cpu)::Bool = (cpu.F & 0x80) >> 7
N(cpu::Cpu)::Bool = (cpu.F & 0x40) >> 6
H(cpu::Cpu)::Bool = (cpu.F & 0x20) >> 5
C(cpu::Cpu)::Bool = (cpu.F & 0x10) >> 4
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

function Push16!(gb::Emulator, cpup::Ptr{Cpu}, val::UInt16)::Nothing
    cpu = unsafe_load(cpup)
    lowbyteaddr = cpu.SP - 2
    highbyteaddr = cpu.SP - 1
    ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, lowbyteaddr, UInt8(val&0xff))
    ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, highbyteaddr, UInt8(val >> 8))
    cpu = unsafe_load(cpup)
    cpu.SP -= 2
    unsafe_store!(cpup, cpu)
    nothing
end

function Pop16!(gb::Emulator, cpup::Ptr{Cpu})::UInt16
    cpu = unsafe_load(cpup)
    cpu.SP += 2
    lowbyteaddr = cpu.SP - 2
    highbyteaddr = cpu.SP - 1
    unsafe_store!(cpup, cpu)
    val = UInt16(ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, lowbyteaddr))
    val |= UInt16(ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, highbyteaddr)) << 8
    val
end

function Add8!(cpu::Cpu, v₀::UInt8, v₁::UInt8, carry::Bool)::UInt8
    sum::UInt32 = UInt32(v₀) + UInt32(v₁) + UInt32(carry)
    half::UInt32 = UInt32(v₀ & 0xf) + UInt32(v₁ & 0xf) + UInt32(carry)
    ZNHC!(cpu,
          sum & 0xff == 0,
          false,
          half > 0xf,
          sum > 0xff)
    UInt8(sum & 0xff)
end

function Add16!(cpu::Cpu, v₀::UInt16, v₁::UInt16)::UInt16
    sum = UInt32(v₀) + UInt32(v₁)
    half = (v₀ & 0xfff) + (v₁ & 0xfff)
    N!(cpu, false)
    H!(cpu, half > 0xfff)
    C!(cpu, sum > 0xffff)
    sum & 0xffff
end

function Sub8!(cpu::Cpu, v₀::UInt8, v₁::UInt8, carry::Bool)::UInt8
    res = Int16(v₀) - Int16(v₁) - Int16(carry)
    if res < 0
        res += 0x100
        C!(cpu, true)
    else
        C!(cpu, false)
    end
    Z!(cpu, res == 0)
    N!(cpu, true)
    H!(cpu, (v₀ & 0xf) - (v₁ & 0xf) - carry > 0xf)
    res
end

function Cp!(cpu::Cpu, v₀::UInt8, v₁::UInt8)::Nothing
    Z!(cpu, v₀ == v₁)
    N!(cpu, true)
    H!(cpu, (v₀ & 0x0f) - (v₁ & 0x0f) > 0x0f)
    C!(cpu, v₀ < v₁)
    nothing
end

function Inc8!(cpu::Cpu, v::UInt8)::UInt8
    N!(cpu, false)
    H!(cpu, (v & 0x0f) == 0x0f)
    if v == 0xff
        Z!(cpu, true)
        0x00
    else
        Z!(cpu, false)
        v + 0x01
    end
end

function Dec8!(cpu::Cpu, v::UInt8)::UInt8
    N!(cpu, true)
    H!(cpu, (v & 0x0f) == 0x00)
    Z!(cpu, v == 0x01)

    v == 0x00 ? 0xff : v - 0x01
end

function And!(cpu::Cpu, v::UInt8)::Nothing
    cpu.A &= v
    ZNHC!(cpu,
          cpu.A == 0,
          false,
          true,
          false)
    nothing
end

function Or!(cpu::Cpu, v::UInt8)::Nothing
    cpu.A |= v
    ZNHC!(cpu,
          cpu.A == 0,
          false,
          false,
          false)
    nothing
end

function Xor!(cpu::Cpu, v::UInt8)::Nothing
    cpu.A = cpu.A ⊻ v
    ZNHC!(cpu,
          cpu.A == 0,
          false,
          false,
          false)
    nothing
end

function Daa!(cpu::Cpu)::Nothing
    a = UInt16(cpu.A)
    if N(cpu)
        if H(cpu)
            a -= 0x06
            a &= 0xff
        end
        if C(cpu)
            a -= 0x60
        end
    else
        if a & 0x0f > 0x09 || H(cpu)
            a += 0x06
        end
        if a > 0x9f || C(cpu)
            a += 0x60
        end
    end
    Z!(cpu, (a & 0xff) == 0)
    H!(cpu, false)
    if a & 0x100 > 0
        C!(cpu, true)
        a &= 0xff
    end
    cpu.A = a
    nothing
end

macro rlc!(register::String)
    reg = Symbol(register)
    return quote
        function(c::Cpu)
            top = (c.$reg & 0x80) >> 7
            c.$reg = (c.$reg << 1) | top
            ZNHC!(c, c.$reg == 0, false, false, top > 0)
            nothing
        end
    end
end
const rlca! = @rlc! "A"

macro rl!(register::String)
    reg = Symbol(register)
    return quote
        function(c::Cpu)
            bottom = C(c)
            C!(c, (c.$reg & 0x80 > 0))
            c.$reg = (c.$reg << 1) | bottom
            Z!(c, c.$reg == 0)
            N!(c, false)
            H!(c, false)
            nothing
        end
    end
end
const rla! = @rl! "A"

macro rrc!(register::String)
    reg = Symbol(register)
    return quote
        function(c::Cpu)
            bottom = c.$reg & 0b1
            c.$reg = (c.$reg >> 1) | (bottom << 7)
            ZNHC!(c, c.$reg == 0, false, false, bottom > 0)
            nothing
        end
    end
end
const rrca! = @rrc! "A"

macro rr!(register::String)
    reg = Symbol(register)
    return quote
        function(c::Cpu)
            top = C(c)
            C!(c, (c.$reg & 0b1 > 0))
            c.$reg = (top << 7) | (c.$reg >> 1)
            Z!(c, c.$reg == 0)
            N!(c, false)
            H!(c, false)
            nothing
        end
    end
end
const rra! = @rr! "A"

function jump!(cpu::Cpu, addr::UInt16)::Nothing
    cpu.PC = addr
    nothing
end

function jumprel!(cpu::Cpu, offset::Int8)::Nothing
    cpu.PC += offset
    nothing
end

function cpu_step(gb::Emulator, cpup::Ptr{Cpu})
    # NOTE: the mo for this port is top down. move it over section by section and then ccall the library with the rest of the lines.

    cpu = unsafe_load(cpup)

    if cpu.Halted
        clock_increment(gb)
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
    elseif opcode == 0x03
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        BC!(cpu, BC(cpu) + 0x01)
    elseif opcode == 0x04
        cpu.B = Inc8!(cpu, cpu.B)
    elseif opcode == 0x05
        cpu.B = Dec8!(cpu, cpu.B)
    elseif opcode == 0x06
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.B = val
    elseif opcode == 0x07
        rlca!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x08
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        val₁ = UInt8(cpu.SP & 0xff)
        val₂ = UInt8(cpu.SP >> 8)
        unsafe_store!(cpup, cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val₁)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr+0x01, val₂)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x09
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, Add16!(cpu, HL(cpu), BC(cpu)))
    elseif opcode == 0x0a
        addr = BC(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x0b
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        BC!(cpu, BC(cpu) - 0x01)
    elseif opcode == 0x0c
        cpu.C = Inc8!(cpu, cpu.C)
    elseif opcode == 0x0d
        cpu.C = Dec8!(cpu, cpu.C)
    elseif opcode == 0x0e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.C = val
    elseif opcode == 0x0f
        rrca!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x10
        # TODO: Stop
    elseif opcode == 0x11
        val = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        DE!(cpu, val)
    elseif opcode == 0x12
        addr = DE(cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, cpu.A)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x13
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        DE!(cpu, DE(cpu) + 0x01)
    elseif opcode == 0x14
        cpu.D = Inc8!(cpu, cpu.D)
    elseif opcode == 0x15
        cpu.D = Dec8!(cpu, cpu.D)
    elseif opcode == 0x16
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.D = val
    elseif opcode == 0x17
        rla!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x18
        clock_increment(gb)
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        jumprel!(cpu, offset)
    elseif opcode == 0x19
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, Add16!(cpu, HL(cpu), DE(cpu)))
    elseif opcode == 0x1a
        addr = DE(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x1b
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        DE!(cpu, DE(cpu) - 0x01)
    elseif opcode == 0x1c
        cpu.E = Inc8!(cpu, cpu.E)
    elseif opcode == 0x1d
        cpu.E = Dec8!(cpu, cpu.E)
    elseif opcode == 0x1e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.E = val
    elseif opcode == 0x1f
        rra!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x20
        clock_increment(gb)
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        if !Z(cpu)
            jumprel!(cpu, offset)
        end
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
    elseif opcode == 0x23
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, HL(cpu) + 0x01)
    elseif opcode == 0x24
        cpu.H = Inc8!(cpu, cpu.H)
    elseif opcode == 0x25
        cpu.H = Dec8!(cpu, cpu.H)
    elseif opcode == 0x26
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.H = val
    elseif opcode == 0x27
        Daa!(cpu)
    elseif opcode == 0x28
        clock_increment(gb)
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        if Z(cpu)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x29
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, Add16!(cpu, HL(cpu), HL(cpu)))
    elseif opcode == 0x2a
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
        HL!(cpu, HL(cpu)+0x01)
    elseif opcode == 0x2b
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x2c
        cpu.L = Inc8!(cpu, cpu.L)
    elseif opcode == 0x2d
        cpu.L = Dec8!(cpu, cpu.L)
    elseif opcode == 0x2e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.L = val
    elseif opcode == 0x2f
        cpu.A = ~cpu.A
        N!(cpu, true)
        H!(cpu, true)
    elseif opcode == 0x30
        clock_increment(gb)
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        if !C(cpu)
            jumprel!(cpu, offset)
        end
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
    elseif opcode == 0x33
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        cpu.SP += 0x01
    elseif opcode == 0x34
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        res = Inc8!(cpu, val)
        unsafe_store!(cpup, cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, res)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x35
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        res = Dec8!(cpu, val)
        unsafe_store!(cpup, cpu)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, res)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x36
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        ccall((:mmu_write, gblib), UInt8, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0x37
        N!(cpu, false)
        H!(cpu, false)
        C!(cpu, true)
    elseif opcode == 0x38
        clock_increment(gb)
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        if C(cpu)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x39
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, Add16!(cpu, HL(cpu), cpu.SP))
    elseif opcode == 0x3a
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x3b
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        cpu.SP -= 0x01
    elseif opcode == 0x3c
        cpu.A = Inc8!(cpu, cpu.A)
    elseif opcode == 0x3d
        cpu.A = Dec8!(cpu, cpu.A)
    elseif opcode == 0x3e
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0x3f
        N!(cpu, false)
        H!(cpu, false)
        C!(cpu, !C(cpu))
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
    elseif opcode == 0x76
        cpu.Halted = true
        if !cpu.InterruptsEnabled
            cpu.HaltBug = true
        end
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
    elseif opcode == 0x80
        cpu.A = Add8!(cpu, cpu.A, cpu.B, false)
    elseif opcode == 0x81
        cpu.A = Add8!(cpu, cpu.A, cpu.C, false)
    elseif opcode == 0x82
        cpu.A = Add8!(cpu, cpu.A, cpu.D, false)
    elseif opcode == 0x83
        cpu.A = Add8!(cpu, cpu.A, cpu.E, false)
    elseif opcode == 0x84
        cpu.A = Add8!(cpu, cpu.A, cpu.H, false)
    elseif opcode == 0x85
        cpu.A = Add8!(cpu, cpu.A, cpu.L, false)
    elseif opcode == 0x86
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = Add8!(cpu, cpu.A, val, false)
    elseif opcode == 0x87
        cpu.A = Add8!(cpu, cpu.A, cpu.A, false)
    elseif opcode == 0x88
        cpu.A = Add8!(cpu, cpu.A, cpu.B, C(cpu))
    elseif opcode == 0x89
        cpu.A = Add8!(cpu, cpu.A, cpu.C, C(cpu))
    elseif opcode == 0x8a
        cpu.A = Add8!(cpu, cpu.A, cpu.D, C(cpu))
    elseif opcode == 0x8b
        cpu.A = Add8!(cpu, cpu.A, cpu.E, C(cpu))
    elseif opcode == 0x8c
        cpu.A = Add8!(cpu, cpu.A, cpu.H, C(cpu))
    elseif opcode == 0x8d
        cpu.A = Add8!(cpu, cpu.A, cpu.L, C(cpu))
    elseif opcode == 0x8e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = Add8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0x8f
        cpu.A = Add8!(cpu, cpu.A, cpu.A, C(cpu))
    elseif opcode == 0x90
        cpu.A = Sub8!(cpu, cpu.A, cpu.B, false)
    elseif opcode == 0x91
        cpu.A = Sub8!(cpu, cpu.A, cpu.C, false)
    elseif opcode == 0x92
        cpu.A = Sub8!(cpu, cpu.A, cpu.D, false)
    elseif opcode == 0x93
        cpu.A = Sub8!(cpu, cpu.A, cpu.E, false)
    elseif opcode == 0x94
        cpu.A = Sub8!(cpu, cpu.A, cpu.H, false)
    elseif opcode == 0x95
        cpu.A = Sub8!(cpu, cpu.A, cpu.L, false)
    elseif opcode == 0x96
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = Sub8!(cpu, cpu.A, val, false)
    elseif opcode == 0x97
        cpu.A = Sub8!(cpu, cpu.A, cpu.A, false)
    elseif opcode == 0x98
        cpu.A = Sub8!(cpu, cpu.A, cpu.B, C(cpu))
    elseif opcode == 0x99
        cpu.A = Sub8!(cpu, cpu.A, cpu.C, C(cpu))
    elseif opcode == 0x9a
        cpu.A = Sub8!(cpu, cpu.A, cpu.D, C(cpu))
    elseif opcode == 0x9b
        cpu.A = Sub8!(cpu, cpu.A, cpu.E, C(cpu))
    elseif opcode == 0x9c
        cpu.A = Sub8!(cpu, cpu.A, cpu.H, C(cpu))
    elseif opcode == 0x9d
        cpu.A = Sub8!(cpu, cpu.A, cpu.L, C(cpu))
    elseif opcode == 0x9e
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = Sub8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0x9f
        cpu.A = Sub8!(cpu, cpu.A, cpu.A, C(cpu))
    elseif opcode == 0xa0
        And!(cpu, cpu.B)
    elseif opcode == 0xa1
        And!(cpu, cpu.C)
    elseif opcode == 0xa2
        And!(cpu, cpu.D)
    elseif opcode == 0xa3
        And!(cpu, cpu.E)
    elseif opcode == 0xa4
        And!(cpu, cpu.H)
    elseif opcode == 0xa5
        And!(cpu, cpu.L)
    elseif opcode == 0xa6
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        And!(cpu, val)
    elseif opcode == 0xa7
        And!(cpu, cpu.A)
    elseif opcode == 0xa8
        Xor!(cpu, cpu.B)
    elseif opcode == 0xa9
        Xor!(cpu, cpu.C)
    elseif opcode == 0xaa
        Xor!(cpu, cpu.D)
    elseif opcode == 0xab
        Xor!(cpu, cpu.E)
    elseif opcode == 0xac
        Xor!(cpu, cpu.H)
    elseif opcode == 0xad
        Xor!(cpu, cpu.L)
    elseif opcode == 0xae
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        Xor!(cpu, val)
    elseif opcode == 0xaf
        Xor!(cpu, cpu.A)
    elseif opcode == 0xb0
        Or!(cpu, cpu.B)
    elseif opcode == 0xb1
        Or!(cpu, cpu.C)
    elseif opcode == 0xb2
        Or!(cpu, cpu.D)
    elseif opcode == 0xb3
        Or!(cpu, cpu.E)
    elseif opcode == 0xb4
        Or!(cpu, cpu.H)
    elseif opcode == 0xb5
        Or!(cpu, cpu.L)
    elseif opcode == 0xb6
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        Or!(cpu, val)
    elseif opcode == 0xb7
        Or!(cpu, cpu.A)
    elseif opcode == 0xb8
        Cp!(cpu, cpu.A, cpu.B)
    elseif opcode == 0xb9
        Cp!(cpu, cpu.A, cpu.C)
    elseif opcode == 0xba
        Cp!(cpu, cpu.A, cpu.D)
    elseif opcode == 0xbb
        Cp!(cpu, cpu.A, cpu.E)
    elseif opcode == 0xbc
        Cp!(cpu, cpu.A, cpu.H)
    elseif opcode == 0xbd
        Cp!(cpu, cpu.A, cpu.L)
    elseif opcode == 0xbe
        addr = HL(cpu)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        Cp!(cpu, cpu.A, val)
    elseif opcode == 0xbf
        Cp!(cpu, cpu.A, cpu.A)
    elseif opcode == 0xc1
        val = Pop16!(gb, cpup)
        cpu = unsafe_load(cpup)
        BC!(cpu, val)
    elseif opcode == 0xc2
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        if !Z(cpu)
            jump!(cpu, addr)
        end
    elseif opcode == 0xc3
        clock_increment(gb)
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        jump!(cpu, addr)
    elseif opcode == 0xc5
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        val = BC(cpu)
        unsafe_store!(cpup, cpu)
        Push16!(gb, cpup, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xc6
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = Add8!(cpu, cpu.A, val, false)
    elseif opcode == 0xca
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        if Z(cpu)
            jump!(cpu, addr)
        end
    elseif opcode == 0xce
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = Add8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0xd1
        val = Pop16!(gb, cpup)
        cpu = unsafe_load(cpup)
        DE!(cpu, val)
    elseif opcode == 0xd2
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        if !C(cpu)
            jump!(cpu, addr)
        end
    elseif opcode == 0xd5
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        val = DE(cpu)
        unsafe_store!(cpup, cpu)
        Push16!(gb, cpup, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xd6
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = Sub8!(cpu, cpu.A, val, false)
    elseif opcode == 0xda
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        if C(cpu)
            jump!(cpu, addr)
        end
    elseif opcode == 0xde
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        cpu.A = Sub8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0xe0
        offset = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        addr = 0xff00 + offset
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xe1
        val = Pop16!(gb, cpup)
        cpu = unsafe_load(cpup)
        HL!(cpu, val)
    elseif opcode == 0xe2
        addr = 0xff00 + cpu.C
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xe5
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        val = HL(cpu)
        unsafe_store!(cpup, cpu)
        Push16!(gb, cpup, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xe6
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        And!(cpu, val)
    elseif opcode == 0xe8
        base = cpu.SP
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        addr = base + offset
        clock_increment(gb)
        clock_increment(gb)
        ZNHC!(cpu,
              false,
              false,
              (addr & 0xf) < (cpu.SP & 0xf),
              (addr & 0xff) < (cpu.SP & 0xff))
        cpu.SP = addr
    elseif opcode == 0xe9
        cpu.PC = HL(cpu)
    elseif opcode == 0xea
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        val = cpu.A
        ccall((:mmu_write, gblib), Cvoid, (Ptr{Cvoid}, UInt16, UInt8), gb.g, addr, val)
        cpu = unsafe_load(cpup)
    elseif opcode == 0xee
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        Xor!(cpu, val)
    elseif opcode == 0xf0
        offset = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        addr = 0xff00 + offset
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0xf1
        val = Pop16!(gb, cpup)
        cpu = unsafe_load(cpup)
        AF!(cpu, val)
    elseif opcode == 0xf2
        addr = 0xff00 + cpu.C
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0xf3
        cpu.InterruptsEnabled = false
        cpu.InterruptEnablePending = false
    #elseif opcode == 0xf5
    #    ccall((:clock_increment, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
    #    cpu = unsafe_load(cpup)
    #    val = AF(cpu)
    #    unsafe_store!(cpup, cpu)
    #    Push16!(gb, cpup, val)
    #    cpu = unsafe_load(cpup)
    elseif opcode == 0xf6
        addr = HL(cpu)
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        Or!(cpu, val)
    elseif opcode == 0xf8
        base = cpu.SP
        offset = ccall((:Imm8i, gblib), Int8, (Ptr{Cvoid},), gb.g)
        addr = base + offset
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        HL!(cpu, addr)
        ZNHC!(cpu,
              false,
              false,
              addr & 0xf < cpu.SP & 0xf,
              addr & 0xff < cpu.SP & 0xff)
    elseif opcode == 0xf9
        clock_increment(gb)
        cpu = unsafe_load(cpup)
        cpu.SP = HL(cpu)
    elseif opcode == 0xfa
        addr = ccall((:Imm16, gblib), UInt16, (Ptr{Cvoid},), gb.g)
        val = ccall((:mmu_read, gblib), UInt8, (Ptr{Cvoid}, UInt16), gb.g, addr)
        cpu = unsafe_load(cpup)
        cpu.A = val
    elseif opcode == 0xfb
        cpu.InterruptEnablePending = true
    elseif opcode == 0xfe
        val = ccall((:Imm8, gblib), UInt8, (Ptr{Cvoid},), gb.g)
        cpu = unsafe_load(cpup)
        Cp!(cpu, cpu.A, val)
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
