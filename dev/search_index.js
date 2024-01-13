var documenterSearchIndex = {"docs":
[{"location":"code/#Code","page":"Code","title":"Code","text":"","category":"section"},{"location":"code/","page":"Code","title":"Code","text":"Here lie all of the docstrings in the GameBoy.jl codebase.","category":"page"},{"location":"code/","page":"Code","title":"Code","text":"Modules = [GameBoy, GameBoy.Processor, GameBoy.Carts]","category":"page"},{"location":"code/#GameBoy.Button","page":"Code","title":"GameBoy.Button","text":"All of the buttons that can be pressed\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Clock","page":"Code","title":"GameBoy.Clock","text":"Main System Clock\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.DMA","page":"Code","title":"GameBoy.DMA","text":"Direct Memory Access metadata\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Emulator","page":"Code","title":"GameBoy.Emulator","text":"The state of an emulator.\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.IORegisters","page":"Code","title":"GameBoy.IORegisters","text":"Addresses of IO devices relative to 0xff00\n\nIntentionally skipping sound since it is unimplemented in this emulator\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Mmu","page":"Code","title":"GameBoy.Mmu","text":"Memory Mapping Unit Read and write bytes to the correct subsystem based on address.\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Ram","page":"Code","title":"GameBoy.Ram","text":"Random Access Memory.\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Video","page":"Code","title":"GameBoy.Video","text":"Video subsystem\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.buttonstate!-Tuple{Emulator, Button, Bool}","page":"Code","title":"GameBoy.buttonstate!","text":"Directly set the state of a button.\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.doframe!-Tuple{Emulator}","page":"Code","title":"GameBoy.doframe!","text":"Run one frame of emulation.\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.execute-Tuple{Button, Emulator}","page":"Code","title":"GameBoy.execute","text":"Press a button in the emulator.\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.ram!-Tuple{Emulator, Vector{UInt8}}","page":"Code","title":"GameBoy.ram!","text":"Restore cartridge RAM. Use ram to capture the state.\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.ram-Tuple{Emulator}","page":"Code","title":"GameBoy.ram","text":"Grab a copy of the cartridge RAM. This captures the save state. Use ram! to restore this state.\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.read-Tuple{Emulator, UInt16}","page":"Code","title":"GameBoy.read","text":"Read an arbitrary byte of memory\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.reset!-Tuple{Emulator}","page":"Code","title":"GameBoy.reset!","text":"Power cycle the emulator\n\n\n\n\n\n","category":"method"},{"location":"code/#GameBoy.Processor.Cpu","page":"Code","title":"GameBoy.Processor.Cpu","text":"Main processor\n\n\n\n\n\n","category":"type"},{"location":"code/#GameBoy.Carts.Cartridge","page":"Code","title":"GameBoy.Carts.Cartridge","text":"The place that stores game data. Traditionally stored on removable media to swap between different games.\n\n\n\n\n\n","category":"type"},{"location":"#GameBoy.jl","page":"Overview","title":"GameBoy.jl","text":"","category":"section"},{"location":"","page":"Overview","title":"Overview","text":"A GameBoy emulator.","category":"page"},{"location":"","page":"Overview","title":"Overview","text":"Documentation Build Status\n[DEV][docs-dev-url] [![][GHA-img]][GHA-url]","category":"page"},{"location":"","page":"Overview","title":"Overview","text":"[docs-dev-url]: https://meltedtofu.github.io/GameBoy.jl [GHA-img]: https://github.com/meltedtofu/GameBoy.jl/workflows/Runtests/badge.svg [GHA-url]: https://github.com/meltedtofu/GameBoy.jl/actions?query=workflows/CI","category":"page"}]
}
