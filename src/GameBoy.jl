module GameBoy

using OffsetArrays
include("Component.jl")
using .Component

include("RandomAccessMemory.jl")
using .RandomAccessMemory

include("IO.jl")
using .IO

include("Interrupts.jl")
using .Interrupts

include("Carts.jl")
using .Carts

include("Video.jl")
using .Video

include("DirectMemoryAccess.jl")
using .DirectMemoryAccess

include("Clock.jl")
using .Clock_

include("MemoryMappingUnit.jl")
using .MemoryMappingUnit


include("Processor.jl")
using .Processor


Component.readb(ram::Ram, addr::IORegisters)::UInt8 = readb(ram, UInt16(addr))
Component.write!(ram::Ram, addr::IORegisters, v::UInt8)::Nothing = write!(ram, UInt16(addr), v)

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

"""
The state of an emulator.
"""
mutable struct Emulator
    cpu::Cpu
    mmu::Mmu
    dma::DMA
    clock::Clock{Mmu}
    frameStride::Int
    cart::Cartridge
    ppu::PPU{Mmu}
    buttons::UInt8

    function Emulator(cartpath)
        dma = DMA()
        clock = Clock{Mmu}()
        ppu = PPU{Mmu}()
        cpu = Cpu()
        cart = Cartridge(cartpath; skipChecksum=true)
        mmu = Mmu(cart, clock, dma, ppu)
        clock.mmu = Ref(mmu)
        ppu.mmu = Ref(mmu)
        cpu.mmu = Ref(mmu)

        new(cpu,
            mmu,
            dma,
            clock,
            160 * 4,
            cart,
            ppu,
            0,
           )
    end
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
Grab a copy of the cartridge RAM.
This captures the save state.
Use `ram!` to restore this state.
"""
function ram(gb::Emulator)::Vector{UInt8}
    deepcopy(gb.cart.ram)
end

"""
Restore cartridge RAM.
Use `ram` to capture the state.
"""
function ram!(gb::Emulator, data::Vector{UInt8})::Nothing
    gb.cart.ram = OffsetVector(deepcopy(data), OffsetArrays.Origin(0))
    nothing
end

"""
Power cycle the emulator
"""
function Component.reset!(gb::Emulator)
    enableBootRom = true
    reset!(gb.clock)
    reset!(gb.ppu)
    reset!(gb.mmu, enableBootRom)

    write!(gb.mmu.io, IOJoypad, 0xcf)
    write!(gb.mmu.io, IOSerialControl, 0x7e)
    write!(gb.mmu.io, IOTimerCounter, 0x00)
    write!(gb.mmu.io, IOTimerModulo, 0x00)
    write!(gb.mmu.io, IOTimerControl, 0x00)
    write!(gb.mmu.io, IOLCDControl, 0x91)
    write!(gb.mmu.io, IOScrollY, 0x00)
    write!(gb.mmu.io, IOScrollX, 0x00)
    write!(gb.mmu.io, IOLCDYCompare, 0x00)
    write!(gb.mmu.io, IOBackgroundPalette, 0xfc)
    write!(gb.mmu.io, IOObjectPalette0, 0xff)
    write!(gb.mmu.io, IOObjectPalette1, 0xff)
    write!(gb.mmu.io, IOWindowY, 0x00)
    write!(gb.mmu.io, IOWindowX, 0x00)

    gb.cpu = Cpu(enableBootRom)
    Component.reset!(gb.dma)
    gb.buttons = 0

    nothing
end

function cycle!(gb::Emulator)::Nothing
    step!(gb.clock)
    step!(gb.dma, gb.mmu)

    # Video runs at 1 pixel per clock (4 per machine cycle)
    for _ ∈ 1:4
        step!(gb.ppu)
    end
end

function Component.readb(gb::Emulator, addr::UInt16)::UInt8
    cycle!(gb)

    if gb.dma.active && addr < 0xff80
        0xff
    else
        readb(gb.mmu, addr)
    end
end

function Component.write!(gb::Emulator, addr::UInt16, val::UInt8)::Nothing
    cycle!(gb)

    if gb.dma.active && addr < 0xff00
        nothing
    else
        write!(gb.mmu, addr, val)
    end
end


AF(cpu::Cpu)::UInt16 = UInt16(cpu.A) << 8 | UInt16(cpu.F)
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

function Push16!(gb::Emulator, cpu::Cpu, val::UInt16)::Nothing
    write!(gb, cpu.SP - 0x0001, UInt8(val >> 8))
    write!(gb, cpu.SP - 0x0002, UInt8(val&0xff))

    cpu.SP -= 0x0002

    nothing
end

function Pop16!(gb::Emulator, cpu::Cpu)::UInt16

    cpu.SP += 0x0002
    lowbyteaddr = cpu.SP - 0x0002
    highbyteaddr = cpu.SP - 0x0001

    val = UInt16(readb(gb, lowbyteaddr))
    val |= UInt16(readb(gb, highbyteaddr)) << 8
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

function rn(gb::Emulator, cpu::Cpu, n::UInt8)::UInt8
    if n == 0
        cpu.B
    elseif n == 1
        cpu.C
    elseif n == 2
        cpu.D
    elseif n == 3
        cpu.E
    elseif n == 4
        cpu.H
    elseif n == 5
        cpu.L
    elseif n == 6
        addr = HL(cpu)
        readb(gb, addr)
    elseif n == 7
        cpu.A
    else
        0x00
    end
end

function rn!(gb::Emulator, cpu::Cpu, n::UInt8, v::UInt8)::Nothing
    if n == 0
        cpu.B = v
    elseif n == 1
        cpu.C = v
    elseif n == 2
        cpu.D = v
    elseif n == 3
        cpu.E = v
    elseif n == 4
        cpu.H = v
    elseif n == 5
        cpu.L = v
    elseif n == 6
        addr = HL(cpu)
        write!(gb, addr, v)
    elseif n == 7
        cpu.A = v
    end
    nothing
end

function jump!(cpu::Cpu, addr::UInt16)::Nothing
    cpu.PC = addr
    nothing
end

function jumprel!(cpu::Cpu, offset::Int8)::Nothing
    cpu.PC += offset
    nothing
end

function call!(gb::Emulator, cpu::Cpu, addr::UInt16)::Nothing
    cycle!(gb)
    Push16!(gb, cpu, cpu.PC)
    cpu.PC = addr
    nothing
end

function ret!(gb::Emulator, cpu::Cpu)::Nothing
    addr = Pop16!(gb, cpu)

    cycle!(gb)
    jump!(cpu, addr)

    nothing
end

function imm8(gb::Emulator, cpu::Cpu)::UInt8
    cpu.PC += 0x0001
    readb(gb, cpu.PC - 0x0001)
end

function imm8i(gb::Emulator, cpu::Cpu)::Int8
    reinterpret(Int8, imm8(gb, cpu))
end

function imm16(gb::Emulator, cpu::Cpu)::UInt16
    low = UInt16(imm8(gb, cpu))
    high = UInt16(imm8(gb, cpu))
    (high << 8) | low
end

function cb_step(gb::Emulator, cpu::Cpu)::Nothing

    opcode = imm8(gb, cpu)


    # TODO: Move this to a docstring and better ascii art
    # XX XXX XXX
    # ~~ operation
    #    ~~~ bit index or sub-operation
    #        ~~~ register
    op = opcode >> 6
    bit = (opcode >> 3) & 0x07
    reg = opcode & 0x07

    if op == 0
        if bit == 0 # rlc rN

            val = rn(gb, cpu, reg)
            ZNHC!(cpu, val == 0, false, false, (val & 0x80) > 0)

            val = (val << 1) | (val >> 7)
            rn!(gb, cpu, reg, val)

        elseif bit == 1 # rrc rN

            val = rn(gb, cpu, reg)
            ZNHC!(cpu, val == 0, false, false, (val & 0x01) > 0)

            val = (val >> 1) | (val << 7)
            rn!(gb, cpu, reg, val)

        elseif bit == 2 # rl rN

            val = rn(gb, cpu, reg)
            rotated = (val << 1) | C(cpu)
            ZNHC!(cpu, rotated == 0, false, false, (val & 0x80) > 0)

            rn!(gb, cpu, reg, rotated)

        elseif bit == 3 # rr rN

            val = rn(gb, cpu, reg)
            rotated = (val >> 1) | (C(cpu)*0x80)
            ZNHC!(cpu, rotated == 0, false, false, (val & 0x01) > 0)

            rn!(gb, cpu, reg, rotated)

        elseif bit == 4 # sla rN

            val = rn(gb, cpu, reg)
            shifted = val << 1
            ZNHC!(cpu, shifted == 0, false, false, (val & 0x80) > 0)

            rn!(gb, cpu, reg, shifted)

        elseif bit == 5 # sra rN

            val = rn(gb, cpu, reg)
            shifted = (val >> 1) | (val & 0x80)
            ZNHC!(cpu, shifted == 0, false, false, (val & 0x01) > 0)

            rn!(gb, cpu, reg, shifted)

        elseif bit == 6 # swap rN

            val = rn(gb, cpu, reg)
            val = (val >> 4) | (val << 4)
            ZNHC!(cpu, val == 0, false, false, false)

            rn!(gb, cpu, reg, val)

        elseif bit == 7 # srl rN

            val = rn(gb, cpu, reg)
            shifted = val >> 1
            ZNHC!(cpu, shifted == 0, false, false, (val & 0x01) > 0)

            rn!(gb, cpu, reg, shifted)

        end
    elseif op == 1
        # bit n, rN

        val = rn(gb, cpu, reg)
        Z!(cpu, (val & (0x01 << bit)) == 0)
        N!(cpu, false)
        H!(cpu, true)

    elseif op == 2
        # res n, rN

        val = rn(gb, cpu, reg)
        rn!(gb, cpu, reg, (val & ~(0x01 << bit)))

    elseif op == 3
        # set n, rN

        val = rn(gb, cpu, reg)
        rn!(gb, cpu, reg, (val | (0x01 << bit)))

    end

    nothing
end

function cpu_step(gb::Emulator, cpu::Cpu)
    if cpu.Halted
        cycle!(gb)
        return
    end

    if cpu.InterruptEnablePending
        cpu.InterruptsEnabled = true
        cpu.InterruptEnablePending = false
    end

    opcode = imm8(gb, cpu)

    if cpu.HaltBug
        cpu.HaltBug = false
        cpu.PC -= 1
    end

    if opcode == 0x00
        # nop
    elseif opcode == 0x01
        val = imm16(gb, cpu)
        BC!(cpu, val)
    elseif opcode == 0x02
        addr = BC(cpu)
        write!(gb, addr, cpu.A)
    elseif opcode == 0x03
        cycle!(gb)
        BC!(cpu, BC(cpu) + 0x01)
    elseif opcode == 0x04
        cpu.B = Inc8!(cpu, cpu.B)
    elseif opcode == 0x05
        cpu.B = Dec8!(cpu, cpu.B)
    elseif opcode == 0x06
        val = imm8(gb, cpu)
        cpu.B = val
    elseif opcode == 0x07
        rlca!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x08
        addr = imm16(gb, cpu)
        val₁ = UInt8(cpu.SP & 0xff)
        val₂ = UInt8(cpu.SP >> 8)

        write!(gb, addr, val₁)
        write!(gb, addr+0x01, val₂)

    elseif opcode == 0x09
        cycle!(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), BC(cpu)))
    elseif opcode == 0x0a
        addr = BC(cpu)
        val = readb(gb, addr)

        cpu.A = val
    elseif opcode == 0x0b
        cycle!(gb)

        BC!(cpu, BC(cpu) - 0x01)
    elseif opcode == 0x0c
        cpu.C = Inc8!(cpu, cpu.C)
    elseif opcode == 0x0d
        cpu.C = Dec8!(cpu, cpu.C)
    elseif opcode == 0x0e
        val = imm8(gb, cpu)
        cpu.C = val
    elseif opcode == 0x0f
        rrca!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x10
        # TODO: Stop
    elseif opcode == 0x11
        val = imm16(gb, cpu)
        DE!(cpu, val)
    elseif opcode == 0x12
        addr = DE(cpu)
        write!(gb, addr, cpu.A)

    elseif opcode == 0x13
        cycle!(gb)

        DE!(cpu, DE(cpu) + 0x01)
    elseif opcode == 0x14
        cpu.D = Inc8!(cpu, cpu.D)
    elseif opcode == 0x15
        cpu.D = Dec8!(cpu, cpu.D)
    elseif opcode == 0x16
        val = imm8(gb, cpu)
        cpu.D = val
    elseif opcode == 0x17
        rla!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x18
        cycle!(gb)
        offset = imm8i(gb, cpu)
        jumprel!(cpu, offset)
    elseif opcode == 0x19
        cycle!(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), DE(cpu)))
    elseif opcode == 0x1a
        addr = DE(cpu)
        val = readb(gb, addr)

        cpu.A = val
    elseif opcode == 0x1b
        cycle!(gb)

        DE!(cpu, DE(cpu) - 0x01)
    elseif opcode == 0x1c
        cpu.E = Inc8!(cpu, cpu.E)
    elseif opcode == 0x1d
        cpu.E = Dec8!(cpu, cpu.E)
    elseif opcode == 0x1e
        val = imm8(gb, cpu)
        cpu.E = val
    elseif opcode == 0x1f
        rra!(cpu)
        Z!(cpu, false)
    elseif opcode == 0x20
        offset = imm8i(gb, cpu)
        if !Z(cpu)
            cycle!(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x21
        val = imm16(gb, cpu)
        HL!(cpu, val)
    elseif opcode == 0x22
        addr = HL(cpu)
        val = cpu.A
        write!(gb, addr, val)

        val = HL(cpu) + 0x01
        HL!(cpu, val)
    elseif opcode == 0x23
        cycle!(gb)

        HL!(cpu, HL(cpu) + 0x01)
    elseif opcode == 0x24
        cpu.H = Inc8!(cpu, cpu.H)
    elseif opcode == 0x25
        cpu.H = Dec8!(cpu, cpu.H)
    elseif opcode == 0x26
        val = imm8(gb, cpu)
        cpu.H = val
    elseif opcode == 0x27
        Daa!(cpu)
    elseif opcode == 0x28
        offset = imm8i(gb, cpu)
        if Z(cpu)
            cycle!(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x29
        cycle!(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), HL(cpu)))
    elseif opcode == 0x2a
        addr = HL(cpu)
        val = readb(gb, addr)

        cpu.A = val
        HL!(cpu, HL(cpu)+0x01)
    elseif opcode == 0x2b
        cycle!(gb)

        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x2c
        cpu.L = Inc8!(cpu, cpu.L)
    elseif opcode == 0x2d
        cpu.L = Dec8!(cpu, cpu.L)
    elseif opcode == 0x2e
        val = imm8(gb, cpu)
        cpu.L = val
    elseif opcode == 0x2f
        cpu.A = ~cpu.A
        N!(cpu, true)
        H!(cpu, true)
    elseif opcode == 0x30
        offset = imm8i(gb, cpu)
        if !C(cpu)
            cycle!(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x31
        val = imm16(gb, cpu)
        cpu.SP = val
    elseif opcode == 0x32
        addr = HL(cpu)
        val = cpu.A
        write!(gb, addr, val)

        HL!(cpu, HL(cpu)-0x01)
    elseif opcode == 0x33
        cycle!(gb)

        cpu.SP += 0x01
    elseif opcode == 0x34
        addr = HL(cpu)
        val = readb(gb, addr)

        res = Inc8!(cpu, val)

        write!(gb, addr, res)

    elseif opcode == 0x35
        addr = HL(cpu)
        val = readb(gb, addr)

        res = Dec8!(cpu, val)

        write!(gb, addr, res)

    elseif opcode == 0x36
        addr = HL(cpu)
        val = imm8(gb, cpu)

        write!(gb, addr, val)

    elseif opcode == 0x37
        N!(cpu, false)
        H!(cpu, false)
        C!(cpu, true)
    elseif opcode == 0x38
        offset = imm8i(gb, cpu)
        if C(cpu)
            cycle!(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x39
        cycle!(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), cpu.SP))
    elseif opcode == 0x3a
        addr = HL(cpu)
        val = readb(gb, addr)

        cpu.A = val
        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x3b
        cycle!(gb)

        cpu.SP -= 0x01
    elseif opcode == 0x3c
        cpu.A = Inc8!(cpu, cpu.A)
    elseif opcode == 0x3d
        cpu.A = Dec8!(cpu, cpu.A)
    elseif opcode == 0x3e
        val = imm8(gb, cpu)
        cpu.A = val
    elseif opcode == 0x3f
        N!(cpu, false)
        H!(cpu, false)
        C!(cpu, !C(cpu))
    elseif opcode == 0x40
        cpu.MoonEyeComplete = true
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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

        cpu.L = val
    elseif opcode == 0x6f
        cpu.L = cpu.A
    elseif opcode == 0x70
        addr = HL(cpu)
        val = cpu.B
        write!(gb, addr, val)

    elseif opcode == 0x71
        addr = HL(cpu)
        val = cpu.C
        write!(gb, addr, val)

    elseif opcode == 0x72
        addr = HL(cpu)
        val = cpu.D
        write!(gb, addr, val)

    elseif opcode == 0x73
        addr = HL(cpu)
        val = cpu.E
        write!(gb, addr, val)

    elseif opcode == 0x74
        addr = HL(cpu)
        val = cpu.H
        write!(gb, addr, val)

    elseif opcode == 0x75
        addr = HL(cpu)
        val = cpu.L
        write!(gb, addr, val)

    elseif opcode == 0x76
        cpu.Halted = true
        if !cpu.InterruptsEnabled
            cpu.HaltBug = true
        end
    elseif opcode == 0x77
        addr = HL(cpu)
        val = cpu.A
        write!(gb, addr, val)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

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
        val = readb(gb, addr)

        Cp!(cpu, cpu.A, val)
    elseif opcode == 0xbf
        Cp!(cpu, cpu.A, cpu.A)
    elseif opcode == 0xc0
        cycle!(gb)

        if !Z(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xc1
        val = Pop16!(gb, cpu)

        BC!(cpu, val)
    elseif opcode == 0xc2
        addr = imm16(gb, cpu)

        if !Z(cpu)
            cycle!(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xc3
        addr = imm16(gb, cpu)
        cycle!(gb)
        jump!(cpu, addr)
    elseif opcode == 0xc4
        addr = imm16(gb, cpu)


        if !Z(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xc5
        cycle!(gb)
        val = BC(cpu)
        Push16!(gb, cpu, val)

    elseif opcode == 0xc6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        cpu.A = Add8!(cpu, cpu.A, val, false)
    elseif opcode == 0xc7
        call!(gb, cpu, 0x0000)

    elseif opcode == 0xc8
        cycle!(gb)

        if Z(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xc9
        ret!(gb, cpu)

    elseif opcode == 0xca
        addr = imm16(gb, cpu)

        if Z(cpu)
            cycle!(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xcb
        cb_step(gb, cpu)

    elseif opcode == 0xcc
        addr = imm16(gb, cpu)

        if Z(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xcd
        call!(gb, cpu, imm16(gb, cpu))
    elseif opcode == 0xce
        val = imm8(gb, cpu)
        cpu.A = Add8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0xcf
        call!(gb, cpu, 0x0008)

    elseif opcode == 0xd0
        cycle!(gb)

        if !C(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xd1
        val = Pop16!(gb, cpu)

        DE!(cpu, val)
    elseif opcode == 0xd2
        addr = imm16(gb, cpu)

        if !C(cpu)
            cycle!(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xd4
        addr = imm16(gb, cpu)

        if !C(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xd5
        cycle!(gb)

        val = DE(cpu)

        Push16!(gb, cpu, val)

    elseif opcode == 0xd6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        cpu.A = Sub8!(cpu, cpu.A, val, false)
    elseif opcode == 0xd7
        call!(gb, cpu, 0x0010)

    elseif opcode == 0xd8
        cycle!(gb)

        if C(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xd9
        ret!(gb, cpu)

        cpu.InterruptsEnabled = true
    elseif opcode == 0xda
        addr = imm16(gb, cpu)

        if C(cpu)
            cycle!(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xdc
        addr = imm16(gb, cpu)

        if C(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xde
        val = imm8(gb, cpu)
        cpu.A = Sub8!(cpu, cpu.A, val, C(cpu))
    elseif opcode == 0xdf
        call!(gb, cpu, 0x0018)

    elseif opcode == 0xe0
        offset = imm8(gb, cpu)
        addr = 0xff00 + offset
        val = cpu.A

        write!(gb, addr, val)

    elseif opcode == 0xe1
        val = Pop16!(gb, cpu)

        HL!(cpu, val)
    elseif opcode == 0xe2
        addr = 0xff00 + cpu.C
        val = cpu.A
        write!(gb, addr, val)

    elseif opcode == 0xe5
        cycle!(gb)

        val = HL(cpu)

        Push16!(gb, cpu, val)

    elseif opcode == 0xe6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        And!(cpu, val)
    elseif opcode == 0xe7
        call!(gb, cpu, 0x0020)

    elseif opcode == 0xe8
        base = cpu.SP
        offset = imm8i(gb, cpu)
        addr = base + offset
        cycle!(gb)
        cycle!(gb)
        ZNHC!(cpu,
              false,
              false,
              (addr & 0xf) < (cpu.SP & 0xf),
              (addr & 0xff) < (cpu.SP & 0xff))
        cpu.SP = addr
    elseif opcode == 0xe9
        cpu.PC = HL(cpu)
    elseif opcode == 0xea
        addr = imm16(gb, cpu)
        val = cpu.A

        write!(gb, addr, val)

    elseif opcode == 0xee
        addr = HL(cpu)
        val = imm8(gb, cpu)
        Xor!(cpu, val)
    elseif opcode == 0xef
        call!(gb, cpu, 0x0028)

    elseif opcode == 0xf0
        offset = imm8(gb, cpu)
        addr = 0xff00 + offset

        val = readb(gb, addr)

        cpu.A = val
    elseif opcode == 0xf1
        val = Pop16!(gb, cpu)

        AF!(cpu, val)
    elseif opcode == 0xf2
        addr = 0xff00 + cpu.C
        val = readb(gb, addr)

        cpu.A = val
    elseif opcode == 0xf3
        cpu.InterruptsEnabled = false
        cpu.InterruptEnablePending = false
    elseif opcode == 0xf5
        cycle!(gb)

        val = AF(cpu)
        Push16!(gb, cpu, val)

    elseif opcode == 0xf6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        Or!(cpu, val)
    elseif opcode == 0xf7
        call!(gb, cpu, 0x0030)

    elseif opcode == 0xf8
        base = cpu.SP
        offset = imm8i(gb, cpu)
        addr = base + offset

        cycle!(gb)

        HL!(cpu, addr)
        ZNHC!(cpu,
              false,
              false,
              addr & 0xf < cpu.SP & 0xf,
              addr & 0xff < cpu.SP & 0xff)
    elseif opcode == 0xf9
        cycle!(gb)

        cpu.SP = HL(cpu)
    elseif opcode == 0xfa
        addr = imm16(gb, cpu)

        val = readb(gb, addr)

        cpu.A = val
    elseif opcode == 0xfb
        cpu.InterruptEnablePending = true
    elseif opcode == 0xfe
        val = imm8(gb, cpu)
        Cp!(cpu, cpu.A, val)
    elseif opcode == 0xff
        call!(gb, cpu, 0x0038)

    end
end

function handleInterrupts(gb::Emulator, cpu::Cpu)
    iflag = readb(gb.mmu.io, IOInterruptFlag)
    mask = 0x1f
    irqs = gb.mmu.interrupt_enable & iflag & mask
    if irqs > 0
        cpu.Halted = false
        if cpu.InterruptsEnabled
            for i ∈ 0:4
                bit = 0x01 << i
                if irqs & bit > 0
                    cpu.InterruptsEnabled = false
                    cycle!(gb)
                    cycle!(gb)
                    call!(gb, cpu, 0x40 + UInt16(i)*0x08)
                    iflag &= ~bit
                    break
                end
            end
            write!(gb.mmu.io, IOInterruptFlag, iflag)
        end
    end
end

# const palette = [0xFFFFFFFF, 0xFFAAAAAA, 0xFF555555, 0xFF000000] # Grayscale
const palette = [0xFFC4CFA1, 0xFF8B956D, 0xFF4D543C, 0xFF1F1F1F] # Pocket
# const palette = [0xFF9BBC0F, 0xFF8BAC0F, 0xFF306230, 0xFF0F380F] # DMG

function input_update!(mmu::Mmu, buttons::UInt8)::Nothing
    invButtons = ~buttons
    joyReg = readb(mmu.io, IOJoypad)
    if joyReg & 0x20 != 0 # Directional keys
        write!(mmu.io, IOJoypad, (joyReg & 0xf0) | ((invButtons >> 4) & 0x0f))
    elseif joyReg & 0x10 == 0 # Buttons
        write!(mmu.io, IOJoypad, (joyReg & 0xf0) | invButtons & 0x0f)
    elseif joyReg == 3 # Model check - 0xfx == classic gameboy
        write!(mmu.io, IOJoypad, 0xff)
    end

    nothing
end

"""
Run one frame of emulation.
"""
function doframe!(gb::Emulator)::Matrix{UInt32}
    while true
        input_update!(gb.mmu, gb.buttons)

        handleInterrupts(gb, gb.cpu)
        cpu_step(gb, gb.cpu)

        if gb.ppu.newframe
            gb.ppu.newframe = false
            for y in 1:144
                for x in 1:160
                    pixel = gb.ppu.buffer[x,y]
                    gb.ppu.frame[y, x] = palette[(pixel & 0x03 + 0x01)]
                end
            end
            return gb.ppu.frame
        end
    end
end

"""
Directly set the state of a button.
"""
function buttonstate!(gb::Emulator, b::Button, pressed::Bool)
    if pressed
        gb.buttons &= ~UInt8(b)
    else
        if gb.buttons & UInt8(b) == 0
            gb.buttons |= UInt8(b)
            write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptJoypad)
        end
    end

    nothing
end

"""
Read an arbitrary byte of memory
"""
function read(gb::Emulator, addr::UInt16)::UInt8
    readb(gb.mmu, addr)
end

export Emulator, reset!, doframe!, buttonstate!, read, ram, ram!

end # module GameBoy
