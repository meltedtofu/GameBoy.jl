# Performance

GameBoy.jl is focused on performance over accuracy or feature compatibility; no attempt to emulate sound for example.

There are a set of benchmarks and a set of tools to help analyze and improve performance.

# Benchmarking

The benchmarks can be found in [./benchmark](https://github.com/meltedtofu/GameBoy.jl/tree/main/benchmark).
These are a reahash of the Blargg test cases wrapped in `BenchmarkTools`.

Currently on my laptop I am seeing ~2 seconds per 1-minute real time trial, which is approximately a 30x increase over the hardware.

# Profiling


The [./profile](https://github.com/meltedtofu/GameBoy.jl/tree/main/profile) project exports two functions to help analyze performance and figure out where resources are being spent.

`pcpu` uses `Profile` and `ProfileView` to build a flame graph of time spent in each function by sampling the stack periodically during a run.

`palloc` uses `Profile` and `PProf` to build a network graph and flame graph of the memory allocations during a run.