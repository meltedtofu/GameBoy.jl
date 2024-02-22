module Carts

using OffsetArrays

using ..Component

@enum MemoryBankController NoMBC MBC1 MBC2 MBC3 MBC5

function idx(mbc::MemoryBankController, active_rom_bank::Int, active_ram_bank::Int, addr::UInt16, romsize::UInt32)::Int
    if mbc == NoMBC || mbc == MBC2
        addr
    elseif addr < 0x4000
        addr
    elseif 0x4000 <= addr  < 0x8000
        cartaddr = (active_rom_bank * 0x4000) + (addr - 0x4000)
        cartaddr%romsize
    elseif 0xa000 <= addr < 0xc000
        active_ram_bank * 0x2000 + addr - 0xa000
    else
        0
    end
end

struct ExtraFeatureFlags
    battery::Bool
    ram::Bool
    rumble::Bool
    sensor::Bool
    timer::Bool

    ExtraFeatureFlags(b, ra, ru, s, t) = new(b, ra, ru, s, t)
end

"""
The place that stores game data.
Traditionally stored on removable media to swap between different games.
"""
mutable struct Cartridge
    rom::OffsetVector{UInt8, Vector{UInt8}}
    ram::OffsetVector{UInt8, Vector{UInt8}}
    romsize::UInt32
    mbc::MemoryBankController
    title::String
    features::ExtraFeatureFlags
    active_rom_bank::Int
    active_ram_bank::Int

    function Cartridge(path::String; skipChecksum::Bool=false)
        cartsize = filesize(path)
        cartbytes = Vector{UInt8}(undef, cartsize)
        open(path) do io
            readbytes!(io, cartbytes)
        end

        rom = OffsetVector(cartbytes, OffsetArrays.Origin(0))

        checksum = 0x00
        for i ∈ 0x134:0x14c
            checksum -= rom[i] - 0x01
        end

        if !skipChecksum && checksum != rom[0x14d]
            throw("Header checksum incorrect. Got $(checksum). Expected $(rom[0x14d])")
        end

        title = [rom[0x0134 + i] for i ∈ 0x00:0x0f] |> Base.Fix1(filter, b -> 0x00 < b < 0x80) |> String

        (mbc, features) = detect_cart_type(rom[0x147])

        active_rom_bank = 1
        active_ram_bank = 1
        if mbc == MBC1
            active_ram_bank = 0
        end


        romsize = detect_rom_size(rom[0x148])

        ramsize = 0
        if features.ram
            ramsize = 1 << (9 + rom[0x149]*2)
        end

        ram = OffsetVector(zeros(ramsize), OffsetArrays.Origin(0))

        new(rom,
            ram,
            romsize,
            mbc,
            title,
            features,
            active_rom_bank,
            active_ram_bank,
           )
    end
end

function Component.write!(cart::Cartridge, addr::UInt16, data::UInt8)::Nothing
    if cart.mbc == MBC1
        if 0x2000 <= addr < 0x4000
            bankno = data & 0x1f
            if bankno == 0
                bankno = 1
            end
            cart.active_rom_bank = (cart.active_rom_bank & 0xe0) | bankno
        elseif 0x4000 <= addr < 0x6000
            cart.active_rom_bank = (cart.active_rom_bank & 0x1f) | ((data & 0x03) << 5)
        elseif 0xa000 <= addr < 0xc000
            reladdr = addr - 0xa0000
            cart.ram[UInt16(cart.active_ram_bank) * 0x2000 + reladdr] = data
        end
    elseif cart.mbc == MBC2
        if 0x2000 <= addr < 0x4000
            bankno = data & 0x0f
            if bankno == 0
                bankno = 1
            end
            cart.active_rom_bank = (cart.active_rom_bank & 0xe0) | bankno
        end
    elseif cart.mbc == MBC3
        if 0x2000 <= addr < 0x4000
            bankno = data & 0x7f
            if bankno == 0
                bankno = 1
            end
            cart.active_rom_bank = bankno
        elseif 0x4000 <= addr < 0x6000
            cart.active_ram_bank = data
        elseif 0xa000 <= addr < 0xc000
            reladdr = addr - 0xa0000
            cart.ram[cart.active_ram_bank * 0x2000 + reladdr] = data
        end
    elseif cart.mbc == MBC5
        old = cart.active_rom_bank

        if 0x2000 <= addr < 0x3000
            cart.active_rom_bank = (cart.active_rom_bank & 0xff00) | UInt16(data)
        elseif 0x3000 <= addr < 0x4000
            cart.active_rom_bank = (cart.active_rom_bank & 0x00ff) | ((UInt16(data) & 0x0001) << 9)
        elseif 0x4000 <= addr < 0x6000
            cart.active_ram_bank = data & 0x0f
        elseif 0xa000 <= addr < 0xc000
            reladdr = addr - 0xa0000
            cart.ram[cart.active_ram_bank * 0x2000 + reladdr] = data
        end
    end

    nothing
end

function detect_cart_type(b::UInt8)::Tuple{MemoryBankController, ExtraFeatureFlags}
    if b == 0x00
        (NoMBC, ExtraFeatureFlags(false, false, false, false, false))
    elseif b == 0x01
        (MBC1, ExtraFeatureFlags(false, false, false, false, false))
    elseif b == 0x02
        (MBC1, ExtraFeatureFlags(false, true, false, false, false))
    elseif b == 0x03
        (MBC1, ExtraFeatureFlags(true, true, false, false, false))
    elseif b == 0x05
        (MBC2, ExtraFeatureFlags(false, false, false, false, false))
    elseif b == 0x06
        (MBC2, ExtraFeatureFlags(true, false, false, false, false))
    elseif b == 0x08
        (NoMBC, ExtraFeatureFlags(false, true, false, false, false))
    elseif b == 0x09
        (NoMBC, ExtraFeatureFlags(true, true, false, false, false))
    elseif b == 0x0b
        throw("Unimplemented MMM01")
    elseif b == 0x0c
        throw("Unimplemented MMM01+RAM")
    elseif b == 0x0d
        throw("Unimplemented MMM01+BATTERY")
    elseif b == 0x0f
        (MBC3, ExtraFeatureFlags(true, false, false, false, true))
    elseif b == 0x10
        (MBC3, ExtraFeatureFlags(true, true, false, false, true))
    elseif b == 0x11
        (MBC3, ExtraFeatureFlags(false, false, false, false, false))
    elseif b == 0x12
        (MBC3, ExtraFeatureFlags(false, true, false, false, false))
    elseif b == 0x13
        (MBC3, ExtraFeatureFlags(true, true, false, false, false))
    elseif b == 0x19
        (MBC5, ExtraFeatureFlags(false, false, false, false, false))
    elseif b == 0x1a
        (MBC5, ExtraFeatureFlags(false, true, false, false, false))
    elseif b == 0x1b
        (MBC5, ExtraFeatureFlags(true, true, false, false, false))
    elseif b == 0x1c
        (MBC5, ExtraFeatureFlags(false, false, true, false, false))
    elseif b == 0x1d
        (MBC5, ExtraFeatureFlags(false, true, true, false, false))
    elseif b == 0x1e
        (MBC5, ExtraFeatureFlags(true, true, true, false, false))
    elseif b == 0x20
        throw("Unimplemented MBC6")
    elseif b == 0x22
        throw("Unimplemented MBC7+SENSOR+RUMBLE+RAM+BATTERY")
    elseif b == 0xfc
        throw("Unimplemented Pocket Camera")
    elseif b == 0xfd
        throw("Unimplemented BANDAI TAMA5")
    elseif b == 0xfe
        throw("Unimplemented HuC3")
    elseif b == 0xff
        throw("Unimplemented HuC1+RAM+BATTERY")
    else
        throw("Unknown Cartridge Type $(b)")
    end
end

function detect_rom_size(b::UInt8)::UInt32
	if b == 0x00
		32768
	elseif b == 0x01
		65536
	elseif b == 0x02
		131072
	elseif b == 0x03
		262144
	elseif b == 0x04
		524288
	elseif b == 0x05
		1048576
	elseif b == 0x06
		2097152
	else
		32768
	end
end

function Component.readb(c::Cartridge, addr::UInt16)::UInt8
    if 0xa000 <= addr < 0xc000
        @inbounds c.ram[idx(c.mbc, c.active_rom_bank, c.active_ram_bank, addr, c.romsize)]
    else
        @inbounds c.rom[idx(c.mbc, c.active_rom_bank, c.active_ram_bank, addr, c.romsize)]
    end
end

Component.readb(cr::Ref{Cartridge}, addr::UInt16)::UInt8 = readb(cr[], addr)

Component.write!(cr::Ref{Cartridge}, addr::UInt16, data::UInt8)::Nothing = write!(cr[], addr, data)

export Cartridge

end # module Carts
