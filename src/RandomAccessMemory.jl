module RandomAccessMemory

using OffsetArrays

using ..Component

"""
Random Access Memory.
"""
mutable struct Ram
    bytes::OffsetVector{UInt8, Vector{UInt8}}
    
    Ram(size::UInt16) = Ram(zeros(UInt8, size))
    Ram(data::Vector{UInt8}) = new(OffsetVector(data, OffsetArrays.Origin(0)))
end

function Component.reset!(ram::Ram)::Nothing
    fill!(ram.bytes, 0x00)
    nothing
end

Component.readb(ram::Ref{Ram}, addr::UInt16)::UInt8 = readb(ram[])
Component.readb(ram::Ram, addr::UInt16)::UInt8 = ram.bytes[addr]
Component.write!(ram::Ref{Ram}, addr::UInt16, v::UInt8)::Nothing = write!(ram[], addr, v)
function Component.write!(ram::Ram, addr::UInt16, v::UInt8)::Nothing
    ram.bytes[addr] = v
    nothing
end

export Ram

end # module RAM