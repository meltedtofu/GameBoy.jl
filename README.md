# GameBoy.jl

A GameBoy emulator.

| **Documentation**    | **Build Status**        | **Coverage** |
|:--------------------:|:-----------------------:|:------------:|
| [DEV](https://meltedtofu.com/GameBoy.jl) | [![](https://github.com/meltedtofu/GameBoy.jl/workflows/Runtests/badge.svg)](https://github.com/meltedtofu/GameBoy.jl/actions?query=workflows/CI) | [![codecov](https://codecov.io/gh/meltedtofu/GameBoy.jl/graph/badge.svg?token=OBTBTSO926)](https://codecov.io/gh/meltedtofu/GameBoy.jl) |

# Introduction
There are as many Game Boy emulators as ther eare users on GitHub. Why make another one? And why use Julia?

I grew up playing Pokémon and thought that it would be fun to build agents to play the game autonomously.
I also spent the majority of my career using languages built on top of LLVM from C++, to Rust, and now Julia.
Mostly switching to the language best suited to the problem domain.

Since I am primarily focused on Julia and this side project benefits from the performance and distributed computing aspects of Julia it was an easy choice.
While Julia easily integrates with C, C++, and Python building the emulator within Julia reduces friction.
Sublime, Julia LSP, and the testing/documentation infrastructure can be reused across projects.
`@code_llvm` and `@distributed` work for both the emulator and any agents I build.
Less context switching overall.

# Installation
This project is published in the [Melted Tofu Registry](https://github.com/meltedtofu/MeltedTofuRegistry). 
To use GameBoy.jl, add the registry (`]registry add https://github.com/meltedtofu/MeltedTofuRegistry.git`) and then use the usual package commands to install `]add GameBoy`.

# Quickstart

The engine is primarily targeted as an engine for game playing agents.

The emulator is driven externally by whatever application you want to build. Just call `doframe!` to emulate the next frame and receive a matrix of pixels. 

```
using GameBoy

e = Emulator("path-to-rom.gb")
for i in 1:1*60*60
    doframe!(e)
end
```
For more complex intractions (read: any interaction at all) check out the tutorials.

# License Information
This project uses the MIT license. You can find the full text in [LICENSE](https://github.com/meltedtofu/GameBoy.jl/blob/main/LICENSE).