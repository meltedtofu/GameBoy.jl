# Example code to generate a screen for a given test rom.
#@testset "blargg generate screenshots" begin
#  testroms = ["cpu_instrs.gb", "halt_bug.gb", "instr_timing.gb", "mem_timing.gb", "oam_bug.gb"]
#
#  for rom in testroms
#    e = Emulator_("blargg", rom)
#
#    for _ ∈ 1:60*60
#      doframe!(e)
#    end
#
#    pixels = doframe!(e)
#    save(File{format"PNG"}("$rom.png"), reinterpret(BGRA{N0f8}, pixels))
#  end
#end

#@testset "pokemon" begin
#    @testcase "generate screenshots" begin
#        e = Emulator_("pokeblue.gb")
#
#        for i ∈ 1:1*60*60
#            buttonstate!(e, ButtonA, i%5 == 0)
#            pixels = doframe!(e)
#            save(File{format"PNG"}(joinpath("screens", "pokeblue.$(lpad(i, 5, '0')).png")), reinterpret(BGRA{N0f8}, pixels))
#        end
#    end
#end