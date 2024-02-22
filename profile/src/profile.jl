module profile

using ProfileView
using Profile
using PProf


using GameBoy

const cpuinstrs = joinpath(@__DIR__, "..", "..", "test", "golden", "roms", "blargg", "cpu_instrs.gb")

function runtherom(path)
    e = Emulator(path)
    reset!(e)
    for _ ∈ 1:60*60
        doframe!(e)
    end
    doframe!(e)
end

function pcpu()
    Profile.clear()
    @profile runtherom(cpuinstrs)
    @profview runtherom(cpuinstrs)
end

export pcpu

function palloc()
    Profile.Allocs.clear()
    Profile.Allocs.@profile runtherom(cpuinstrs)
    PProf.Allocs.pprof()
end

export palloc
end # module profile
