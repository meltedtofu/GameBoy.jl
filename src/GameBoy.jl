module GameBoy

using OffsetArrays
include("Component.jl")
using .Component

include("Processor.jl")
using .Processor

include("Carts.jl")
using .Carts

"""
Addresses of IO devices relative to 0xff00

Intentionally skipping sound since it is unimplemented in this emulator
"""
@enum IORegisters begin
    IOJoypad=0x10
    IOSerialData=0x01
    IOSerialControl=0x02
    IODivider=0x04
    IOTimerCounter=0x05
    IOTimerModulo=0x06
    IOTimerControl=0x07
    IOInterruptFlag=0x0f
    IOLCDControl=0x40
    IOLCDStat=0x41
    IOScrollY=0x42
    IOScrollX=0x43
    IOLCDY=0x44
    IOLCDYCompare=0x45
    IOOAMDMA=0x46
    IOBackgroundPalette=0x47
    IOObjectPalette0=0x48
    IOObjectPalette1=0x49
    IOWindowY=0x4a
    IOWindowX=0x4b
    IOBootRomDisable=0x50
end

include("DirectMemoryAccess.jl")
using .DirectMemoryAccess

include("RandomAccessMemory.jl")
using .RandomAccessMemory

Component.readb(ram::Ram, addr::IORegisters)::UInt8 = readb(ram, UInt16(addr))
Component.write!(ram::Ram, addr::IORegisters, v::UInt8)::Nothing = write!(ram, UInt16(addr), v)

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

@enum Interrupt begin
    InterruptVBlank=0x01
    InterruptLCDC=0x02
    InterruptTIMA=0x04
    InterruptSerial=0x08
    InterruptJoypad=0x10
    InterruptMask=0x1f
end

mutable struct Sprite
    x::UInt8
    pixels::OffsetVector{UInt8, Vector{UInt8}} # 2 bytes
    attrs::UInt8
    
    Sprite() = new(0, OffsetVector(zeros(UInt8, 2), OffsetArrays.Origin(0)), 0)
end

"""
Video subsystem
"""
mutable struct Video
    frameprogress::UInt
    ram::Ram

    # Bits 0-1 = color (0 = darkest, 3 = lightest)
    # Remaining Bits are zero
    buffer::Matrix{UInt8}

    # The pixels to send to SDl as AARGGBB values interpreted through the default palette
    frame::Matrix{UInt32}

    newframe::Bool

    scanline_sprites::OffsetVector{Sprite, Vector{Sprite}}
    num_sprites::UInt8
    curx::UInt8
    
    Video() = new(0,
                  Ram(0x2000),
                  Matrix{UInt8}(undef, 160, 144),
                  Matrix{UInt32}(undef, 144, 160),
                  false,
                  OffsetVector(fill(Sprite(), 10), OffsetArrays.Origin(0)),
                  0,
                  0,
                 )
end

function Component.reset!(v::Video)::Nothing
    Component.reset!(v.ram)
    v.newframe = false
    v.frameprogress = 0
    v.num_sprites = 0
    v.curx = 0
    nothing
end

Component.readb(v::Ref{Video}, addr::UInt16)::UInt8 = readb(v[], addr)
Component.readb(v::Video, addr::UInt16)::UInt8 = readb(v.ram, addr)
Component.write!(v::Ref{Video}, addr::UInt16, val::UInt8):: Nothing = write!(v[], addr, val)
Component.write!(v::Video, addr::UInt16, val::UInt8)::Nothing = write!(v.ram, addr, val)

"""
Main System Clock
"""
mutable struct Clock
    cycles::UInt16
    overflow::Bool
    loading::Bool
    
    Clock() = new(0, false, false)
end


function Component.reset!(c::Clock)
    c.cycles = 0
    c.overflow = false
    c.loading = false
end

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
    cart::Ref{Cartridge}
    clock::Ref{Clock}
    dma::Ref{DMA}
    video::Ref{Video}
    bootRomEnabled::Bool

    Mmu(cart, clock, dma, video) = new(Ram(0x2000),
                                       Ram(0x007f),
                                       0x00,
                                       Ram(0x00a0),
                                       Ram(0x0080),
                                       Ref{Cartridge}(cart),
                                       Ref{Clock}(clock),
                                       Ref{DMA}(dma),
                                       Ref{Video}(video),
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

function step!(dma::DMA, mmu::Mmu)::Nothing
    if dma.pendingsource > 0
        if !dma.delaystart
            dma.source = UInt16(dma.pendingsource) << 8
            dma.pendingsource = 0   
        end
        dma.delaystart = false
    end
    
    if dma.source > 0 && (dma.source&0xff) < 0xa0
        dma.active = true
        write!(mmu.oam, dma.source & 0xff, mmu_readDirect(mmu, dma.source))
        dma.source += 1
    else
        dma.active = false
    end
    
    nothing
end

"""
The state of an emulator.
"""
mutable struct Emulator
    cpu::Cpu
    mmu::Mmu
    dma::DMA
    clock::Clock
    frameStride::Int
    video::Video
    buttons::UInt8

    function Emulator(cartpath)
        clock = Clock()
        dma = DMA()
        video = Video()

        new(Cpu(),
            Mmu(Cartridge(cartpath; skipChecksum=true), clock, dma, video),
            dma,
            clock,
            160 * 4,
            video,
            0,
           )
    end
end

Base.:+(a::UInt16, b::IORegisters)::UInt16 = a + UInt16(b)
Base.to_index(reg::IORegisters) = Int(reg)
Base.:|(a::UInt8, b::Interrupt)::UInt8 = a | UInt8(b)
    
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
    gb.cart.ram = deepcopy(data)
    nothing
end

"""
Power cycle the emulator
"""
function Component.reset!(gb::Emulator)
    enableBootRom = true
    reset!(gb.clock)
    reset!(gb.video)
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

function video_tileLineAddress(index::UInt8, y::UInt16, lowBank::Bool)::UInt16
    # Addresses relative to video ram base address
    addr = 0x0000
    if lowBank
        addr = index * 16
    else
        addr = 0x1000 + reinterpret(Int8, index) * 16
    end
    
    addr + 2y
end

function video_readSprites!(gb::Emulator, scanline::UInt8)::Nothing
    spriteHeight = readb(gb.mmu.io, IOLCDControl) & 0x04 > 0 ? 16 : 8
    
    nsprites = 0
    i::UInt16 = 0x0000
    while i < 160
        ypos = readb(gb.mmu.oam, i)
        xpos = readb(gb.mmu.oam, i+0x01)
        if 0 < ypos < 160 && xpos < 168 # on screen
            if ypos <= scanline + 16 < ypos + spriteHeight # in scanline
                # insert the sprite into the list, keeping the list in priority order
                inspos = nsprites
                while inspos > 0 && gb.video.scanline_sprites[inspos - 1].x > xpos
                    if inspos < 10
                        gb.video.scanline_sprites[inspos] = sprites[inspos - 1]
                    end
                    inspos -= 1
                end
                if inspos < 10
                    tile = readb(gb.mmu.oam, i+0x02)
                    attr = readb(gb.mmu.oam, i+0x03)
                    if spriteHeight == 16
                        tile &= 0xfe
                    end
                    
                    tiley = scanline + 0x0016 - ypos
                    if attr & 0x40 > 0 # y flip
                        tiley = (spriteHeight - 0x0001) - tiley
                    end
                    
                    tileaddr = video_tileLineAddress(tile, tiley, true)
                    
                    gb.video.scanline_sprites[inspos].x = xpos;
                    gb.video.scanline_sprites[inspos].pixels[0] = readb(gb.video, tileaddr)
                    gb.video.scanline_sprites[inspos].pixels[1] = readb(gb.video, tileaddr + 0x01)
                    gb.video.scanline_sprites[inspos].attrs = attr
                    
                    if nsprites < 10
                        nsprites += 1
                    end
                end
            end
        end
        i += 4
    end
    
    gb.video.num_sprites = nsprites

    nothing
end

function video_linePixel(l1::UInt8, l2::UInt8, x::UInt16)::UInt8
    (((l1 << x) & 0x80) >> 7) | (((l2 << x) & 0x80) >> 6)
end

function video_mapPixel(gb::Emulator, hiMap::Bool, loTiles::Bool, x::UInt16, y::UInt16)::UInt8
    tileIndex = readb(gb.video, UInt16((hiMap ? 0x1c00 : 0x1800) + ((y ÷ 8)*32) + (x ÷ 8)))
    addr = video_tileLineAddress(tileIndex, y%0x08, loTiles)
    video_linePixel(readb(gb.video, addr), readb(gb.video, addr+0x0001), x%0x0008)
end

function video_paletteLookup(pixel::UInt8, palette::UInt8)
    (palette >> (pixel*2)) & 0x03
end

function video_drawPixel!(gb::Emulator, scanline::UInt8, x::UInt8)::Nothing
    lcdc = readb(gb.mmu.io, IOLCDControl)
    hiMapBg = lcdc & 0x08 > 0 
    hiMapWin = lcdc & 0x40 > 0
    bgEnable = lcdc & 0x01 > 0
    winEnable = lcdc & 0x20 > 0
    spriteEnable = lcdc & 0x02 > 0
    loTiles = lcdc & 0x10 > 0
    
    wy = readb(gb.mmu.io, IOWindowY)
    wx = readb(gb.mmu.io, IOWindowX)
    
    winEnable = winEnable && wx < 167 && wy < 144 && wy <= scanline
    spriteEnable = spriteEnable && gb.video.num_sprites > 0

    if winEnable || bgEnable || spriteEnable
        scy = readb(gb.mmu.io, IOScrollY)
        scx = readb(gb.mmu.io, IOScrollX)
        
        bgPixel = 0x00
        if winEnable && x + 0x07 >= wx
            bgPixel = video_mapPixel(gb, hiMapWin, loTiles, x+7-wx, scanline-wy)
        elseif bgEnable
            bgPixel = video_mapPixel(gb, hiMapBg, loTiles, (x+scx)%0x0100, (scanline+scy)%0x0100)
        end

        finalColor = video_paletteLookup(bgPixel, readb(gb.mmu.io, IOBackgroundPalette))

        if spriteEnable
            obp0 = readb(gb.mmu.io, IOObjectPalette0)
            obp1 = readb(gb.mmu.io, IOObjectPalette1)
            n = 0
            while n < gb.video.num_sprites
                spiten = gb.video.scanline_sprites[n]
                if x < spriten.x <= x+0x08
                    tileX = x + 0x08 - spriten.x
                    mirrored = spriten.attrs & 0x20 > 0
                    pixel = video_linePixel(spriten.pixels, mirrored ? 7 - tileX : tileX)
                    
                    if pixel > 0
                        hasPriority = spriten.attrs & 0x80 == 0
                        if finalColor == 0 || hasPriority
                            palette = spriten.attrs & 0x10 > 0 ? obp1 : obp0
                            finalColor = video_paletteLookup(pixel, palette)
                        end
                        break
                    end
                end
                n += 1
            end
        end

        gb.video.buffer[x+0x01, scanline+0x01] = finalColor
    end

    nothing
end

function video_update!(gb::Emulator)::Nothing
    # assert scanline <= 154
    
    scanline = UInt8(gb.video.frameprogress ÷ 456)
    lcdOn = readb(gb.mmu.io, IOLCDControl) & 0x80 > 0x00
    write!(gb.mmu.io, IOLCDY, scanline)
    
    stat = readb(gb.mmu.io, IOLCDStat)
    
    if lcdOn
        if scanline == readb(gb.mmu.io, IOLCDYCompare)
            if (stat & 0x04) == 0x00
                write!(gb.mmu.io, IOLCDStat, readb(gb.mmu.io, IOLCDStat) | 0x04)
                if stat & 0x40 > 0x00
                    write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptLCDC)
                end
            end
        else
            write!(gb.mmu.io, IOLCDStat, readb(gb.mmu.io, IOLCDStat) & ~0x04)
        end
    end
    
    lcdMode = stat & 0x03
    
    if scanline >= 144 # last 10 scanlines are vblank. don't draw anything
        if lcdMode != 1
            write!(gb.mmu.io, IOLCDStat, (stat & ~0x03) | 0x01)
            write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptVBlank)
            if readb(gb.mmu.io, IOLCDControl) & 0x80 == 0
                fill!(gb.video.buffer, 0x00)
            end
            gb.video.newframe = true
        end
    else
        scanlineProgress = gb.video.frameprogress % 456
        
        if scanlineProgress < 92
            if lcdMode != 2
                write!(gb.mmu.io, IOLCDStat, (stat & 0x03) | 0x02)
                if stat & 0x20 > 0x00
                    write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptLCDC)
                end
                video_readSprites!(gb, scanline)
                gb.video.curx = 0
            end
        elseif scanlineProgress < 160 + 92
            write!(gb.mmu.io, IOLCDStat, (stat & ~0x03) | 0x03)
            if lcdOn
                while gb.video.curx < scanlineProgress - 92
                    video_drawPixel!(gb, scanline, gb.video.curx)
                    gb.video.curx += 1
                end
            end
        else
            if lcdMode != 0
                if lcdOn
                    while gb.video.curx < 160
                        video_drawPixel!(gb, scanline, gb.video.curx) 
                        gb.video.curx += 1
                    end
                end
                write!(gb.mmu.io, IOLCDStat, (stat & ~0x03))
                if stat & 0x08 > 0x00
                    write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptLCDC)
                end
            end
        end
    end
    
    gb.video.frameprogress = (gb.video.frameprogress + 1) % 70224
    
    nothing
end

function clock_getTimerBit(control::UInt8, cycles::UInt16)::Bool
    switch = control & 0x03
    if switch == 0x00
        cycles & (0x0001 << 9) > 0
    elseif switch == 0x01
        cycles & (0x0001 << 3) > 0
    elseif switch == 0x02
        cycles & (0x0001 << 5) > 0
    elseif switch == 0x03
        cycles & (0x0001 << 7) > 0
    end
end

function clock_timerIncrement!(clock::Clock, mmu::Mmu)::Nothing
    timer = readb(mmu.io, IOTimerCounter)
    if timer == 0xff
        clock.overflow = true
    end
    write!(mmu.io, IOTimerCounter, timer + 0x01)
    nothing
end

function clock_countChange!(clock::Clock, mmu::Mmu, newval::UInt16)
    tac = readb(mmu.io, IOTimerControl)
    if tac & 0x04 > 0x00
        if !clock_getTimerBit(tac, newval) && clock_getTimerBit(tac, clock.cycles)
            clock_timerIncrement!(clock, mmu)
        end
    end
    clock.cycles = newval
    write!(mmu.io, IODivider, UInt8(newval >> 0x08))
end

function clock_increment(gb::Emulator)::Nothing
    gb.clock.loading = false
    if gb.clock.overflow
        # Delayed overflow effects
        write!(gb.mmu.io, IOInterruptFlag, readb(gb.mmu.io, IOInterruptFlag) | InterruptTIMA)
        gb.clock.overflow = false

        # modulo is being loaded in next machine cycle
        write!(gb.mmu.io, IOTimerCounter, readb(gb.mmu.io, IOTimerModulo))
        gb.clock.loading = true
    end
    
    clock_countChange!(gb.clock, gb.mmu, gb.clock.cycles + 0x04)
    
    step!(gb.dma, gb.mmu)
    
    # Video runs at 1 pixel per clock (4 per machine cycle)
    for _ ∈ 1:4
        video_update!(gb)
    end
end

function clock_updateTimerControl!(clock::Clock, mmu::Mmu, val::UInt8)::Nothing
    old = readb(mmu.io, IOTimerControl)
    write!(mmu.io, IOTimerControl, val)
    
    oldbit = (old & 0x04) > 0x00 && clock_getTimerBit(old, clock.cycles)
    newbit = (val & 0x04) > 0x00 && clock_getTimerBit(val, clock.cycles)
    
    # check for falling edge
    if oldbit && !newbit
        clock_timerIncrement!(clock, mmu)
    end
    nothing
end

function mmu_readDirect(mmu::Mmu, addr::UInt16)::UInt8
    if 0x0000 <= addr < 0x0100 && mmu.bootRomEnabled
        readb(bootrom, addr)
    elseif 0x0000 <= addr < 0x8000
        readb(mmu.cart, addr)
    elseif 0x8000 <= addr < 0xa000
        readb(mmu.video, addr - 0x8000)
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

function mmu_read!(gb::Emulator, addr::UInt16)::UInt8
    clock_increment(gb)

    if gb.dma.active && addr < 0xff80
        0xff
    else
        mmu_readDirect(gb.mmu, addr)
    end
end

function mmu_writeDirect!(mmu::Mmu, addr::UInt16, val::UInt8)::Nothing
    if addr > 0xffff
    elseif 0x0000 <= addr < 0x2000
    # TODO: Cart RAM Bank Enabled
    elseif 0x2000 <= addr < 0x6000
        write!(mmu.cart, addr, val)
    elseif 0x6000 <= addr < 0x8000
        # RTC. Not implemented.
    elseif 0x8000 <= addr < 0xa000
        # TODO: Writes to VRAM should be ignored when the LCD is being redrawn
        write!(mmu.video, addr - 0x8000, val)
    elseif 0xa000 <= addr < 0xc000
        write!(mmu.cart, addr - 0xa000, val)
    elseif 0xc000 <= addr < 0xe000
        write!(mmu.workram, addr - 0xc000, val)
    elseif 0xe000 <= addr < 0xfe00
        write!(mmu.workram, addr - 0xe000, val)
    elseif 0xfe00 <= addr < 0xfea0
        write!(mmu.oam, addr - 0xfe00, val)
    elseif 0xfea0 <= addr < 0xff00
        nothing
    elseif 0xff00 + IODivider == addr
        clock_countChange!(mmu.clock[], mmu, 0x0000)
    elseif 0xff00 + IOTimerCounter == addr
        if !mmu.clock[].loading
            write!(mmu.io, IOTimerCounter, val)
            mmu.clock[].overflow = false
        end
    elseif 0xff00 + IOTimerModulo == addr
        write!(mmu.io, IOTimerModulo, val)
        if mmu.clock[].loading
            # While loading writes are immediate
            write!(mmu.io, IOTimerCounter, val)
        end
    elseif 0xff00 + IOTimerControl == addr
        clock_updateTimerControl!(mmu.clock[], mmu, val)
    elseif 0xff00 + IOInterruptFlag == addr
        write!(mmu.io, IOInterruptFlag, val | 0xe0)
    elseif 0xff00 + IOLCDStat == addr
        write!(mmu.io, IOLCDStat, (readb(mmu.io, IOLCDStat) & 0x03) | (val & ~0x03))
    elseif 0xff00 + IOLCDY == addr
        write!(mmu.io, IOLCDY, 0x00)
    elseif 0xff00 + IOOAMDMA == addr
        if val <= 0xf1
            mmu.dma[].pendingsource = val
            mmu.dma[].delaystart = true
        end
    elseif 0xff00 + IOBootRomDisable == addr
        mmu.bootRomEnabled = false
    elseif 0xff00 <= addr < 0xff80 # Generic IO for unimplemented devices.
        write!(mmu.io, addr - 0xff00, val)
    elseif 0xff80 <= addr < 0xffff
        write!(mmu.highram, addr - 0xff80, val)
    elseif 0xffff == addr
        mmu.interrupt_enable = val
    end
    nothing
end

function mmu_write!(gb::Emulator, addr::UInt16, val::UInt8)::Nothing
    clock_increment(gb)

    if gb.dma.active && addr < 0xff00
        nothing
    else
        mmu_writeDirect!(gb.mmu, addr, val)
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
    mmu_write!(gb, cpu.SP - 0x0001, UInt8(val >> 8))
    mmu_write!(gb, cpu.SP - 0x0002, UInt8(val&0xff))

    cpu.SP -= 0x0002

    nothing
end

function Pop16!(gb::Emulator, cpu::Cpu)::UInt16

    cpu.SP += 0x0002
    lowbyteaddr = cpu.SP - 0x0002
    highbyteaddr = cpu.SP - 0x0001

    val = UInt16(mmu_read!(gb, lowbyteaddr))
    val |= UInt16(mmu_read!(gb, highbyteaddr)) << 8
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
        mmu_read!(gb, addr)
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
        mmu_write!(gb, addr, v)
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
    clock_increment(gb)
    Push16!(gb, cpu, cpu.PC)
    cpu.PC = addr
    nothing
end

function ret!(gb::Emulator, cpu::Cpu)::Nothing
    addr = Pop16!(gb, cpu)

    clock_increment(gb)
    jump!(cpu, addr)

    nothing
end

function imm8(gb::Emulator, cpu::Cpu)::UInt8
    cpu.PC += 0x0001
    mmu_read!(gb, cpu.PC - 0x0001)
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
        clock_increment(gb)
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
        mmu_write!(gb, addr, cpu.A)
    elseif opcode == 0x03
        clock_increment(gb)
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

        mmu_write!(gb, addr, val₁)
        mmu_write!(gb, addr+0x01, val₂)

    elseif opcode == 0x09
        clock_increment(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), BC(cpu)))
    elseif opcode == 0x0a
        addr = BC(cpu)
        val = mmu_read!(gb, addr)

        cpu.A = val
    elseif opcode == 0x0b
        clock_increment(gb)

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
        mmu_write!(gb, addr, cpu.A)

    elseif opcode == 0x13
        clock_increment(gb)

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
        clock_increment(gb)
        offset = imm8i(gb, cpu)
        jumprel!(cpu, offset)
    elseif opcode == 0x19
        clock_increment(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), DE(cpu)))
    elseif opcode == 0x1a
        addr = DE(cpu)
        val = mmu_read!(gb, addr)

        cpu.A = val
    elseif opcode == 0x1b
        clock_increment(gb)

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
            clock_increment(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x21
        val = imm16(gb, cpu)
        HL!(cpu, val)
    elseif opcode == 0x22
        addr = HL(cpu)
        val = cpu.A
        mmu_write!(gb, addr, val)

        val = HL(cpu) + 0x01
        HL!(cpu, val)
    elseif opcode == 0x23
        clock_increment(gb)

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
            clock_increment(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x29
        clock_increment(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), HL(cpu)))
    elseif opcode == 0x2a
        addr = HL(cpu)
        val = mmu_read!(gb, addr)

        cpu.A = val
        HL!(cpu, HL(cpu)+0x01)
    elseif opcode == 0x2b
        clock_increment(gb)

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
            clock_increment(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x31
        val = imm16(gb, cpu)
        cpu.SP = val
    elseif opcode == 0x32
        addr = HL(cpu)
        val = cpu.A
        mmu_write!(gb, addr, val)

        HL!(cpu, HL(cpu)-0x01)
    elseif opcode == 0x33
        clock_increment(gb)

        cpu.SP += 0x01
    elseif opcode == 0x34
        addr = HL(cpu)
        val = mmu_read!(gb, addr)

        res = Inc8!(cpu, val)

        mmu_write!(gb, addr, res)

    elseif opcode == 0x35
        addr = HL(cpu)
        val = mmu_read!(gb, addr)

        res = Dec8!(cpu, val)

        mmu_write!(gb, addr, res)

    elseif opcode == 0x36
        addr = HL(cpu)
        val = imm8(gb, cpu)

        mmu_write!(gb, addr, val)

    elseif opcode == 0x37
        N!(cpu, false)
        H!(cpu, false)
        C!(cpu, true)
    elseif opcode == 0x38
        offset = imm8i(gb, cpu)
        if C(cpu)
            clock_increment(gb)
            jumprel!(cpu, offset)
        end
    elseif opcode == 0x39
        clock_increment(gb)

        HL!(cpu, Add16!(cpu, HL(cpu), cpu.SP))
    elseif opcode == 0x3a
        addr = HL(cpu)
        val = mmu_read!(gb, addr)

        cpu.A = val
        HL!(cpu, HL(cpu) - 0x01)
    elseif opcode == 0x3b
        clock_increment(gb)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

        cpu.L = val
    elseif opcode == 0x6f
        cpu.L = cpu.A
    elseif opcode == 0x70
        addr = HL(cpu)
        val = cpu.B
        mmu_write!(gb, addr, val)

    elseif opcode == 0x71
        addr = HL(cpu)
        val = cpu.C
        mmu_write!(gb, addr, val)

    elseif opcode == 0x72
        addr = HL(cpu)
        val = cpu.D
        mmu_write!(gb, addr, val)

    elseif opcode == 0x73
        addr = HL(cpu)
        val = cpu.E
        mmu_write!(gb, addr, val)

    elseif opcode == 0x74
        addr = HL(cpu)
        val = cpu.H
        mmu_write!(gb, addr, val)

    elseif opcode == 0x75
        addr = HL(cpu)
        val = cpu.L
        mmu_write!(gb, addr, val)

    elseif opcode == 0x76
        cpu.Halted = true
        if !cpu.InterruptsEnabled
            cpu.HaltBug = true
        end
    elseif opcode == 0x77
        addr = HL(cpu)
        val = cpu.A
        mmu_write!(gb, addr, val)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

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
        val = mmu_read!(gb, addr)

        Cp!(cpu, cpu.A, val)
    elseif opcode == 0xbf
        Cp!(cpu, cpu.A, cpu.A)
    elseif opcode == 0xc0
        clock_increment(gb)

        if !Z(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xc1
        val = Pop16!(gb, cpu)

        BC!(cpu, val)
    elseif opcode == 0xc2
        addr = imm16(gb, cpu)

        if !Z(cpu)
            clock_increment(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xc3
        addr = imm16(gb, cpu)
        clock_increment(gb)
        jump!(cpu, addr)
    elseif opcode == 0xc4
        addr = imm16(gb, cpu)


        if !Z(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xc5
        clock_increment(gb)
        val = BC(cpu)
        Push16!(gb, cpu, val)

    elseif opcode == 0xc6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        cpu.A = Add8!(cpu, cpu.A, val, false)
    elseif opcode == 0xc7
        call!(gb, cpu, 0x0000)

    elseif opcode == 0xc8
        clock_increment(gb)

        if Z(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xc9
        ret!(gb, cpu)

    elseif opcode == 0xca
        addr = imm16(gb, cpu)

        if Z(cpu)
            clock_increment(gb)
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
        clock_increment(gb)

        if !C(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xd1
        val = Pop16!(gb, cpu)

        DE!(cpu, val)
    elseif opcode == 0xd2
        addr = imm16(gb, cpu)

        if !C(cpu)
            clock_increment(gb)
            jump!(cpu, addr)
        end
    elseif opcode == 0xd4
        addr = imm16(gb, cpu)

        if !C(cpu)
            call!(gb, cpu, addr)
        end

    elseif opcode == 0xd5
        clock_increment(gb)

        val = DE(cpu)

        Push16!(gb, cpu, val)

    elseif opcode == 0xd6
        addr = HL(cpu)
        val = imm8(gb, cpu)
        cpu.A = Sub8!(cpu, cpu.A, val, false)
    elseif opcode == 0xd7
        call!(gb, cpu, 0x0010)

    elseif opcode == 0xd8
        clock_increment(gb)

        if C(cpu)
            ret!(gb, cpu)
        end

    elseif opcode == 0xd9
        ret!(gb, cpu)

        cpu.InterruptsEnabled = true
    elseif opcode == 0xda
        addr = imm16(gb, cpu)

        if C(cpu)
            clock_increment(gb)
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

        mmu_write!(gb, addr, val)

    elseif opcode == 0xe1
        val = Pop16!(gb, cpu)

        HL!(cpu, val)
    elseif opcode == 0xe2
        addr = 0xff00 + cpu.C
        val = cpu.A
        mmu_write!(gb, addr, val)

    elseif opcode == 0xe5
        clock_increment(gb)

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
        addr = imm16(gb, cpu)
        val = cpu.A

        mmu_write!(gb, addr, val)

    elseif opcode == 0xee
        addr = HL(cpu)
        val = imm8(gb, cpu)
        Xor!(cpu, val)
    elseif opcode == 0xef
        call!(gb, cpu, 0x0028)

    elseif opcode == 0xf0
        offset = imm8(gb, cpu)
        addr = 0xff00 + offset

        val = mmu_read!(gb, addr)

        cpu.A = val
    elseif opcode == 0xf1
        val = Pop16!(gb, cpu)

        AF!(cpu, val)
    elseif opcode == 0xf2
        addr = 0xff00 + cpu.C
        val = mmu_read!(gb, addr)

        cpu.A = val
    elseif opcode == 0xf3
        cpu.InterruptsEnabled = false
        cpu.InterruptEnablePending = false
    elseif opcode == 0xf5
        clock_increment(gb)

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

        clock_increment(gb)

        HL!(cpu, addr)
        ZNHC!(cpu,
              false,
              false,
              addr & 0xf < cpu.SP & 0xf,
              addr & 0xff < cpu.SP & 0xff)
    elseif opcode == 0xf9
        clock_increment(gb)

        cpu.SP = HL(cpu)
    elseif opcode == 0xfa
        addr = imm16(gb, cpu)

        val = mmu_read!(gb, addr)

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
                    clock_increment(gb)
                    clock_increment(gb)
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

        if gb.video.newframe
            gb.video.newframe = false
            for y in 1:144
                for x in 1:160
                    pixel = gb.video.buffer[x,y]
                    gb.video.frame[y, x] = palette[(pixel & 0x03 + 0x01)]
                end
            end            
            return gb.video.frame
        end
    end
end

"""
Directly set the state of a button.
"""
function buttonstate!(gb::Emulator, b::Button, pressed::Bool)
    if pressed
        gb.buttons &= ~b
    else
        if gb.buttons & button == 0
            gb.buttons |= button
            write!(gb.mmu.io, IOInterruptFlag, read(gb.mmu.io, IOInterruptFlag) | InterruptJoypad)
        end
    end
    
    nothing
end

"""
Read an arbitrary byte of memory
"""
function read(gb::Emulator, addr::UInt16)::UInt8
    mmu_readDirect(gb.mmu, addr)
end

export Emulator, free!, loadrom!, reset!, doframe!, buttonstate!, read, ram, ram!

end # module GameBoy
