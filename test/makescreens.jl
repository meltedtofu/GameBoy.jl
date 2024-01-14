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

#@testset "blargg generate screenshots" begin
#   e = Emulator_("pokeblue.gb")
#   
#   for _ ∈ 1:60*60
#     doframe!(e)
#   end
#   
#   pixels = doframe!(e)
#   save(File{format"PNG"}("pokeblue.png"), reinterpret(BGRA{N0f8}, pixels))
#end