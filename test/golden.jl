using GameBoy
using Test

using FileIO
using Images

function Emulator_(cartridge_name::String...)::Emulator
    e = Emulator()
    loadrom!(e, joinpath(@__DIR__, "golden", "roms", cartridge_name...); skip_checksum=true)
    reset!(e)
    e
end

@testset "golden" begin
    d = joinpath(@__DIR__, "golden")
    readdir(joinpath(d)) |>
        Base.Fix1(filter, f -> endswith(f, ".jl")) .|>
        Base.Fix1(joinpath, d) .|>
        include
end
