using GameBoy
using XUnit

using FileIO
using Images

function Emulator_(cartridge_name::String...)::Emulator
    cartpath = joinpath(@__DIR__, "golden", "roms", cartridge_name...)
    e = Emulator(cartpath)
    reset!(e)
    e
end

@testset runner=ParallelTestRunner() "GameBoy" begin
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
          if length(tests) == 0
            continue
          end
          @testset "$(chop(root, head=length(testdir)+1, tail=0))" begin
              for t in tests
                  include(joinpath(root, t))
              end
          end
      end
  end
end