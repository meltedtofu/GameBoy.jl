module Carts

using OffsetArrays

using ..Component

@enum MemoryBankController NoMBC MBC1 MBC2 MBC3 MBC5

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
    ramg::Bool
    bank1::UInt8
    bank2::UInt8
    mbc1_mode::Bool
    rom_offset_lower::UInt
    rom_offset_upper::UInt
    ram_offset::Int
    ramb::UInt8

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
            false,
            0b0_0001,
            0b00,
            false,
            0,
            0x4000,
            0,
            0,
           )
    end
end

const ROM_BANK_SIZE = 0x4000
const RAM_BANK_SIZE = 0x2000

function rom_offsets_mbc1!(cart::Cartridge)::Nothing
    upperbits = cart.bank2 << 5
    lowerbits = cart.bank1
    if cart.mbc1_mode 
        cart.rom_offset_lower = UInt32(upperbits) * ROM_BANK_SIZE
    else
        0b00
    end
    cart.rom_offset_upper = UInt32(upperbits | lowerbits) * ROM_BANK_SIZE
    nothing
end

function rom_offsets_mbc5!(cart::Cartridge)::Nothing
    upperbits = UInt32(cart.bank2) << 8
    lowerbits = UInt32(cart.bank1)
    cart.rom_offset_lower = 0x0000
    cart.rom_offset_upper = ROM_BANK_SIZE * (upperbits | lowerbits)
    nothing
end

function ram_offset_mbc1!(cart::Cartridge)::Nothing
    if cart.mbc1_mode
        cart.ram_offset = cart.bank2 * RAM_BANK_SIZE
    else
        cart.ram_offset = 0b00
    end
    nothing
end

function Component.write!(cart::Cartridge, addr::UInt16, data::UInt8)::Nothing
    if cart.mbc == MBC1
        if 0x0000 <= addr < 0x2000
            cart.ramg = (data & 0b1111) == 0b1010
        elseif 0x2000 <= addr < 0x4000
            if (data & 0b1_1111) == 0b0_0000
                cart.bank1 = 0b0_0001
            else
                cart.bank1 = data & 0b1_1111
            end
            rom_offsets_mbc1!(cart)
        elseif 0x4000 <= addr < 0x6000
            cart.bank2 = data & 0b11
            rom_offsets_mbc1!(cart)
            ram_offset_mbc1!(cart)
        elseif 0x06000 <= addr < 0x8000
            cart.mbc1_mode = (data & 0b1) == 0b1
            rom_offsets_mbc1!(cart)
            ram_offset_mbc1!(cart)
        elseif 0xa000 <= addr < 0xc000 && cart.ramg
            cart.ram[(cart.ram_offset | addr & 0x1fff) & (length(cart.ram) - 1)] = data
        end
    elseif cart.mbc == MBC2
        if 0x0000 <= addr < 0x4000
            cart.ramg = (data & 0x0f) == 0x0a
            if data & 0b1111 == 0b0000
                cart.bank1 = 0b0001
            else
                cart.bank1 = data & 0b1111
            end
            cart.rom_offset_lower = 0x0000
            cart.rom_offset_upper = ROM_BANK_SIZE * UInt32(cart.bank1)
        elseif 0xa000 <= addr < 0xc000 && cart.ramg && length(cart.ram) > 0
            cart.ram[(cart.ram_offset | addr & 0x1fff) & (length(cart.ram) - 1)] = data & 0x0f
        end
    elseif cart.mbc == MBC3
        if 0x2000 <= addr < 0x4000
        elseif 0x4000 <= addr < 0x6000
        elseif 0xa000 <= addr < 0xc000
        end
    elseif cart.mbc == MBC5
        if 0x0000 <= addr < 0x2000
            cart.ramg = data == 0x0a
        elseif 0x2000 <= addr < 0x3000
            cart.bank1 = data
            rom_offsets_mbc5!(cart)
        elseif 0x3000 <= addr < 0x4000
            cart.bank2 = data & 0b1
            rom_offsets_mbc5!(cart)
        elseif 0x4000 <= addr < 0x6000
            cart.ramb = data & 0b1111
            cart.ram_offset = RAM_BANK_SIZE * UInt32(cart.ramb)
        elseif 0xa000 <= addr < 0xc000 && cart.ramg
            cart.ram[(cart.ram_offset | addr & 0x1fff) & (length(cart.ram) - 1)] = data
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
    elseif b == 0x07
        4194304
    elseif b == 0x08
        8388608
    elseif b == 0x09
        16777216
	else
		32768
	end
end

function Component.readb(c::Cartridge, addr::UInt16)::UInt8
    if 0x0000 <= addr < 0x4000
        @inbounds c.rom[(c.rom_offset_lower | addr & 0x3fff) & (length(c.rom) - 1)]
    elseif 0x4000 <= addr < 0x8000
        @inbounds c.rom[(c.rom_offset_upper | addr & 0x3fff) & (length(c.rom) - 1)]
    elseif 0xa000 <= addr < 0xc000
        if c.mbc == MBC1
            if length(c.ram) > 0 && c.ramg
                @inbounds c.ram[(c.ram_offset | addr & 0x1fff) & (length(c.ram) - 1)]
            else
                0xff
            end
        elseif c.mbc == MBC2
            if length(c.ram) > 0 && c.ramg
                @inbounds c.ram[(c.ram_offset | addr & 0x1fff) & (length(c.ram) - 1)]
            else
                0xff
            end
        elseif c.mbc == MBC5
            if length(c.ram) > 0 && c.ramg
                @inbounds c.ram[(c.ram_offset | addr & 0x1fff) & (length(c.ram) - 1)]
            else
                0xff
            end
        else
            0xff
        end
    else
        0xff
    end
end

Component.readb(cr::Ref{Cartridge}, addr::UInt16)::UInt8 = readb(cr[], addr)

Component.write!(cr::Ref{Cartridge}, addr::UInt16, data::UInt8)::Nothing = write!(cr[], addr, data)

export Cartridge

end # module Carts
