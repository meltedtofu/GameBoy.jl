using GameBoy
using Test

using FileIO
using Images

function Emulator_(cartridge_name::String...)::Emulator
    e = Emulator()
    loadrom!(e, joinpath(@__DIR__, "golden", "roms", cartridge_name...); skip_checksum=true)
    reset!(e)
    e
end

@testset "GameBoy" begin
  testdir = dirname(@__FILE__)

  for (root, dirs, files) in walkdir(testdir)
      tests = files |> Base.Fix1(filter, f -> endswith(f, ".jl") && f != "runtests.jl") |> collect
      if endswith(root, "test")
          # Top-Level tests
          for t in tests
              include(joinpath(root, t))
          end
      else
          # Nested Tests
          @testset "$(chop(root, head=length(testdir)+1, tail=0))" begin
              for t in tests
                  include(joinpath(root, t))
              end
          end
      end
  end
end
