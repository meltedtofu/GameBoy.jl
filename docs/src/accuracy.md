# Accuracy

While not the primary focus, a minimum amount of accuracy is necessary to actually play Pokémon.

Since Game Boy is a well researched platform there are many pre-existing test suites to use to evaluate accuracy of a Game Boy emulator.
GameBoy.jl uses subsets of two of them, [Blargg](https://github.com/retrio/gb-test-roms) and [Mooneye](https://github.com/Gekkio/mooneye-test-suite).

The tests can be found [here](https://github.com/meltedtofu/GameBoy.jl/tree/main/test/golden).

For Blargg we compare to a known screen output after one minute of emulation.

For Mooneye we look for the established "end" instruction (opcode 0x40) and then check the state of the registers (do they contain the fibonacci sequence?).