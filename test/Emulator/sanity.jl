@testset "Sanity Check" begin
    @test GameBoy.sanity_check() == 0x42
end
