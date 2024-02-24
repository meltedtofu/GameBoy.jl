@testset "acid" begin
  @testcase "dmg-acid2" begin
    e = Emulator_("acid", "dmg-acid2.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixels = doframe!(e)

    expected = load(joinpath(@__DIR__, "screens", "dmg-acid2.gb.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
  end
end