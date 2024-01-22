module Video

using OffsetArrays

using ..Component
using ..RandomAccessMemory
using ..IO
using ..Interrupts

mutable struct Sprite
    x::UInt8
    pixel0::UInt8
    pixel1::UInt8
    attrs::UInt8
    
    Sprite() = new(0, 0, 0, 0)
end

"""
Video subsystem
"""
mutable struct PPU{T}
    frameprogress::UInt
    ram::Ram
    mmu::Base.RefValue{T}

    # Bits 0-1 = color (0 = darkest, 3 = lightest)
    # Remaining Bits are zero
    buffer::Matrix{UInt8}

    # The pixels to send to SDl as AARGGBB values interpreted through the default palette
    frame::Matrix{UInt32}

    newframe::Bool

    scanline_sprites::OffsetVector{Sprite, Vector{Sprite}}
    num_sprites::UInt8
    curx::UInt8

    PPU{T}() where {T} = PPU{T}(nothing)
    PPU{T}(t::Union{Nothing, T}) where {T} = new(0,
                                             Ram(0x2000),
                                             isnothing(t) ? Ref{T}() : Ref(t),
                                             Matrix{UInt8}(undef, 160, 144),
                                             Matrix{UInt32}(undef, 144, 160),
                                             false,
                                             OffsetVector([Sprite() for _ in 1:10], OffsetArrays.Origin(0)),
                                             0,
                                             0,
                                            )
end

function Component.reset!(p::PPU)::Nothing
    Component.reset!(p.ram)
    p.newframe = false
    p.frameprogress = 0
    p.num_sprites = 0
    p.curx = 0
    nothing
end

Component.readb(p::Base.RefValue{PPU}, addr::UInt16)::UInt8 = readb(p[], addr)
Component.readb(p::PPU, addr::UInt16)::UInt8 = readb(p.ram, addr)
Component.write!(p::Base.RefValue{PPU}, addr::UInt16, val::UInt8):: Nothing = write!(p[], addr, val)
Component.write!(p::PPU, addr::UInt16, val::UInt8)::Nothing = write!(p.ram, addr, val)

function tileLineAddress(index::UInt8, y::UInt16, lowBank::Bool)::UInt16
    # Addresses relative to video ram base address
    addr = 0x0000
    if lowBank
        addr = index * 16
    else
        addr = 0x1000 + reinterpret(Int8, index) * 16
    end
    
    addr + 2y
end

function readSprites!(ppu::PPU, scanline::UInt8)::Nothing
    spriteHeight = readb(ppu.mmu[].io, IOLCDControl) & 0x04 > 0 ? 16 : 8
    
    nsprites = 0
    i::UInt16 = 0x0000
    while i < 160
        ypos = readb(ppu.mmu[].oam, i)
        xpos = readb(ppu.mmu[].oam, i+0x01)
        if 0 < ypos < 160 && xpos < 168 # on screen
            if ypos <= scanline + 0x10 < ypos + spriteHeight # in scanline
                # insert the sprite into the list, keeping the list in priority order
                inspos = nsprites
                while inspos > 0 && ppu.scanline_sprites[inspos - 1].x > xpos
                    if inspos < 10
                        ppu.scanline_sprites[inspos].x      = ppu.scanline_sprites[inspos - 1].x
                        ppu.scanline_sprites[inspos].pixel0 = ppu.scanline_sprites[inspos - 1].pixel0
                        ppu.scanline_sprites[inspos].pixel1 = ppu.scanline_sprites[inspos - 1].pixel1
                        ppu.scanline_sprites[inspos].attrs  = ppu.scanline_sprites[inspos - 1].attrs
                    end
                    inspos -= 1
                end
                if inspos < 10
                    tile = readb(ppu.mmu[].oam, i+0x02)
                    attr = readb(ppu.mmu[].oam, i+0x03)
                    if spriteHeight == 16
                        tile &= 0xfe
                    end
                    
                    tiley = scanline + 0x0010 - ypos
                    if attr & 0x40 > 0 # y flip
                        tiley = (spriteHeight - 0x0001) - tiley
                    end
                    
                    tileaddr = tileLineAddress(tile, tiley, true)

                    ppu.scanline_sprites[inspos].x = xpos;
                    ppu.scanline_sprites[inspos].pixel0 = readb(ppu, tileaddr)
                    ppu.scanline_sprites[inspos].pixel1 = readb(ppu, tileaddr + 0x01)
                    ppu.scanline_sprites[inspos].attrs = attr

                    if nsprites < 10
                        nsprites += 1
                    end
                end
            end
        end
        i += 4
    end
    
    ppu.num_sprites = nsprites

    nothing
end

function linePixel(l1::UInt8, l2::UInt8, x::UInt16)::UInt8
    (((UInt16(l1) << x) & 0x0080) >> 7) | (((UInt16(l2) << x) & 0x0080) >> 6)
end

function mapPixel(ppu::PPU, hiMap::Bool, loTiles::Bool, x::UInt16, y::UInt16)::UInt8
    tileIndex = readb(ppu, UInt16((hiMap ? 0x1c00 : 0x1800) + ((y ÷ 8)*32) + (x ÷ 8)))
    addr = tileLineAddress(tileIndex, y%0x08, loTiles)
    linePixel(readb(ppu, addr), readb(ppu, addr+0x0001), x%0x0008)
end

function paletteLookup(pixel::UInt8, palette::UInt8)
    (palette >> (pixel*2)) & 0x03
end

function drawPixel!(ppu::PPU, scanline::UInt8, x::UInt8)::Nothing
    lcdc = readb(ppu.mmu[].io, IOLCDControl)
    hiMapBg = lcdc & 0x08 > 0 
    hiMapWin = lcdc & 0x40 > 0
    bgEnable = lcdc & 0x01 > 0
    winEnable = lcdc & 0x20 > 0
    spriteEnable = lcdc & 0x02 > 0
    loTiles = lcdc & 0x10 > 0
    
    wy = readb(ppu.mmu[].io, IOWindowY)
    wx = readb(ppu.mmu[].io, IOWindowX)
    
    winEnable = winEnable && wx < 167 && wy < 144 && wy <= scanline
    spriteEnable = spriteEnable && ppu.num_sprites > 0

    if winEnable || bgEnable || spriteEnable
        scy = readb(ppu.mmu[].io, IOScrollY)
        scx = readb(ppu.mmu[].io, IOScrollX)
        
        bgPixel = 0x00
        if winEnable && x + 0x07 >= wx
            bgPixel = mapPixel(ppu, hiMapWin, loTiles, UInt16(x+0x07-wx), UInt16(scanline-wy))
        elseif bgEnable
            bgPixel = mapPixel(ppu, hiMapBg, loTiles, (UInt16(x)+scx)%0x0100, (UInt16(scanline)+scy)%0x0100)
        end

        finalColor = paletteLookup(bgPixel, readb(ppu.mmu[].io, IOBackgroundPalette))

        if spriteEnable
            obp0 = readb(ppu.mmu[].io, IOObjectPalette0)
            obp1 = readb(ppu.mmu[].io, IOObjectPalette1)
            n = 0
            while n < ppu.num_sprites
                spriten = ppu.scanline_sprites[n]
                if spriten.x <= x + 0x08 < spriten.x + 0x08
                    tileX = x + 0x08 - spriten.x
                    mirrored = spriten.attrs & 0x20 > 0
                    pixel = linePixel(spriten.pixel0, spriten.pixel1, UInt16(mirrored ? 0x07 - tileX : tileX))
                    
                    if pixel > 0
                        hasPriority = spriten.attrs & 0x80 == 0
                        if finalColor == 0 || hasPriority
                            palette = spriten.attrs & 0x10 > 0 ? obp1 : obp0
                            finalColor = paletteLookup(pixel, palette)
                        end
                        break
                    end
                end
                n += 1
            end
        end

        ppu.buffer[x+0x01, scanline+0x01] = finalColor
    end

    nothing
end

function update!(ppu::PPU)::Nothing
    # assert scanline <= 154
    
    scanline = UInt8(ppu.frameprogress ÷ 456)
    lcdOn = readb(ppu.mmu[].io, IOLCDControl) & 0x80 > 0x00
    write!(ppu.mmu[].io, IOLCDY, scanline)

    stat = readb(ppu.mmu[].io, IOLCDStat)
    
    if lcdOn
        if scanline == readb(ppu.mmu[].io, IOLCDYCompare)
            if (stat & 0x04) == 0x00
                write!(ppu.mmu[].io, IOLCDStat, readb(ppu.mmu[].io, IOLCDStat) | 0x04)
                if stat & 0x40 > 0x00
                    write!(ppu.mmu[].io, IOInterruptFlag, readb(ppu.mmu[].io, IOInterruptFlag) | InterruptLCDC)
                end
            end
        else
            write!(ppu.mmu[].io, IOLCDStat, readb(ppu.mmu[].io, IOLCDStat) & ~0x04)
        end
    end
    
    lcdMode = stat & 0x03
    
    if scanline >= 144 # last 10 scanlines are vblank. don't draw anything
        if lcdMode != 1
            write!(ppu.mmu[].io, IOLCDStat, (stat & ~0x03) | 0x01)
            write!(ppu.mmu[].io, IOInterruptFlag, readb(ppu.mmu[].io, IOInterruptFlag) | InterruptVBlank)
            if readb(ppu.mmu[].io, IOLCDControl) & 0x80 == 0
                fill!(ppu.buffer, 0x00)
            end
            ppu.newframe = true
        end
    else
        scanlineProgress = ppu.frameprogress % 456
        
        if scanlineProgress < 92
            if lcdMode != 2
                write!(ppu.mmu[].io, IOLCDStat, (stat & 0x03) | 0x02)
                if stat & 0x20 > 0x00
                    write!(ppu.mmu[].io, IOInterruptFlag, readb(ppu.mmu[].io, IOInterruptFlag) | InterruptLCDC)
                end
                readSprites!(ppu, scanline)
                ppu.curx = 0
            end
        elseif scanlineProgress < 160 + 92
            write!(ppu.mmu[].io, IOLCDStat, (stat & ~0x03) | 0x03)
            if lcdOn
                while ppu.curx < scanlineProgress - 92
                    drawPixel!(ppu, scanline, ppu.curx)
                    ppu.curx += 1
                end
            end
        else
            if lcdMode != 0
                if lcdOn
                    while ppu.curx < 160
                        drawPixel!(ppu, scanline, ppu.curx) 
                        ppu.curx += 1
                    end
                end
                write!(ppu.mmu[].io, IOLCDStat, (stat & ~0x03))
                if stat & 0x08 > 0x00
                    write!(ppu.mmu[].io, IOInterruptFlag, readb(ppu.mmu[].io, IOInterruptFlag) | InterruptLCDC)
                end
            end
        end
    end
    
    ppu.frameprogress = (ppu.frameprogress + 1) % 70224
    
    nothing
end

export PPU, update!

end # module Video