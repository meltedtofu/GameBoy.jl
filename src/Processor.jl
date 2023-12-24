module Processor

"""
Main processor
"""
mutable struct Cpu
    A::UInt8
    F::UInt8
    B::UInt8
    C::UInt8
    D::UInt8
    E::UInt8
    H::UInt8
    L::UInt8
    SP::UInt16
    PC::UInt16
    InterruptsEnabled::Bool
    InterruptEnablePending::Bool
    Halted::Bool
    HaltBug::Bool

    function Cpu(bootRom::Bool=false)
        # See "The Cycle Accurate GB Doc"
        # https://github.com/geaz/emu-gameboy/blob/master/docs/The%20Cycle-Accurate%20Game%20Boy%20Docs.pdf
        new(0x01,
            0xB0,
            0x00,
            0x13,
            0x00,
            0xd8,
            0x01,
            0x4d,
            0xfffe,
            bootRom ? 0x0000 : 0x0100,
            false,
            false,
            false,
            false)
    end
end

export Cpu

end # module Processor
