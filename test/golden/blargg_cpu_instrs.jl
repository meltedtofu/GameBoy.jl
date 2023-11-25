#TODO: Extract frame->image into a function
@testset "blargg cpu_instrs" begin
    e = Emulator_("blargg", "cpu_instrs.gb")

    for _ ∈ 1:60*60
        doframe!(e)
    end

    pixelsraw = doframe!(e)
    pixelsptr = convert(Ptr{UInt32}, pixelsraw)
    pixels = Matrix{UInt32}(undef, 144, 160)
    for j in 1:144
        for i in 1:160
            pixels[j, i] = unsafe_load(pixelsptr, i + (j-1)*160)
        end
    end

    expected = load(joinpath(@__DIR__, "screens", "blargg_cpu_instrs.png"))
    @test expected == reinterpret(BGRA{N0f8}, pixels)
end
