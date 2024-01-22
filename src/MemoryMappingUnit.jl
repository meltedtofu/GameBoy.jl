module MemoryMappingUnit

using ..Component
using ..RandomAccessMemory
using ..Clock_
using ..DirectMemoryAccess
using ..Video
using ..Carts

"""
Memory Mapping Unit
Read and write bytes to the correct subsystem based on address.
"""
mutable struct Mmu
    workram::Ram
    highram::Ram
    interrupt_enable::UInt8 #TODO:  model this as an offset vector with size 1. same interface as everything else attached to the mmu
    oam::Ram
    io::Ram
    cart::Base.RefValue{Cartridge}
    clock::Base.RefValue{Clock}
    dma::Base.RefValue{DMA}
    ppu::Base.RefValue{PPU}
    bootRomEnabled::Bool

    Mmu(cart, clock, dma, ppu) = new(Ram(0x2000),
                                     Ram(0x007f),
                                     0x00,
                                     Ram(0x00a0),
                                     Ram(0x0080),
                                     Ref{Cartridge}(cart),
                                     Ref{Clock}(clock),
                                     Ref{DMA}(dma),
                                     Ref{PPU}(ppu),
                                     true,
                                    )
end

function Component.reset!(mmu::Mmu, enableBootRom::Bool)::Nothing
    reset!(mmu.workram)
    reset!(mmu.highram)
    mmu.interrupt_enable = 0x00
    mmu.bootRomEnabled = enableBootRom
    nothing
end

function Component.readb(mmu::Mmu, addr::UInt16)::UInt8
    if 0x0000 <= addr < 0x0100 && mmu.bootRomEnabled
        readb(bootrom, addr)
    elseif 0x0000 <= addr < 0x8000
        readb(mmu.cart, addr)
    elseif 0x8000 <= addr < 0xa000
        readb(mmu.ppu, addr - 0x8000)
    elseif 0xa000 <= addr < 0xc000
        readb(mmu.cart, addr)
    elseif 0xc000 <= addr < 0xe000
        readb(mmu.workram, addr - 0xc000)
    elseif 0xe000 <= addr < 0xfe00
        readb(mmu.workram, addr - 0xe000)
    elseif 0xfe00 <= addr < 0xfea0
        readb(mmu.oam, addr - 0xfe00)
    elseif 0xfea0 <= addr < 0xff00
        0x00
    elseif 0xff00 <= addr < 0xff80
        readb(mmu.io, addr - 0xff00)
    elseif 0xff80 <= addr < 0xffff
        readb(mmu.highram, addr - 0xff80)
    elseif 0xffff == addr
        mmu.interrupt_enable
    end
end


const bootrom = Ram([
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
   ])

export Mmu

end # module MemoryMappingUnit