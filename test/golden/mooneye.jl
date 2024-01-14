@testset "mooneye" begin
    @testset "acceptance" begin
        testdir = joinpath(@__DIR__, "roms", "mooneye", "acceptance")

        for (root, dirs, files) in walkdir(testdir)
            roms = files |> 
                Base.Fix1(filter, f -> endswith(f, ".gb")) |> 
                Base.Fix1(filter, f -> !endswith(f, "-S.gb")) |>
                Base.Fix1(filter, f -> !endswith(f, "-sgb.gb")) |>
                Base.Fix1(filter, f -> !endswith(f, "-sgb2.gb")) |>
                Base.Fix1(filter, f -> !endswith(f, "mgb.gb")) |>
                collect
            relpath = chop(root, head=length(testdir)+1, tail=0) |> splitpath
            
            if length(roms) == 0
                continue
            end
            
            @testset "$(join(relpath))" begin
                for rom in roms
                    @testset "$rom" begin
                        e = Emulator_("mooneye", "acceptance", relpath..., rom)

                        for i ∈ 1:1000
                            doframe!(e)
                            if e.cpu.MoonEyeComplete
                                 @test e.cpu.B == 0x03 &&
                                       e.cpu.C == 0x05 &&
                                       e.cpu.D == 0x08 &&
                                       e.cpu.E == 0x0d &&
                                       e.cpu.H == 0x15 &&
                                       e.cpu.L == 0x22
                                break
                            end
                        end
                        
                        @test e.cpu.MoonEyeComplete
                    end
                end
            end
        end
    end
end