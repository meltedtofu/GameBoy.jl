@testset "blargg" begin
  @testset "cpu_instrs" begin
    e = Emulator_("blargg", "cpu_instrs.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_cpu_instrs.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
  
  @testset "halt_bug" begin
    e = Emulator_("blargg", "halt_bug.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_halt_bug.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
  
  @testset "instr_timing" begin
    e = Emulator_("blargg", "instr_timing.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_instr_timing.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
  
  @testset "mem_timing" begin
    e = Emulator_("blargg", "mem_timing.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_mem_timing.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
  
  @testset "oam_bug" begin
    e = Emulator_("blargg", "oam_bug.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_oam_bug.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
end
