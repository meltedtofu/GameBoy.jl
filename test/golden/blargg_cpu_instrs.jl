#TODO: Extract frame->image into a function
@testset "blargg cpu_instrs" begin
    e = Emulator_("blargg", "cpu_instrs.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "blargg_cpu_instrs.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
end
