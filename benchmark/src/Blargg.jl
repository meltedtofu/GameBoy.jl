
function Blargg()::BenchmarkGroup
    function runtherom(path)
        e = Emulator(path)
        reset!(e)
        for _ ∈ 1:60*60
            doframe!(e)
        end
        doframe!(e)
    end

    basepath = joinpath(@__DIR__, "..", "..", "test", "golden", "roms", "blargg")
    roms = readdir(basepath) .|> r -> (r, joinpath(basepath, r))

    g = BenchmarkGroup()
    for (name, path) in roms
        g[name] = @benchmarkable $runtherom($path)
    end
    g
end