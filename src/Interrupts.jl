module Interrupts

"""
Bit values of each interrupt in the system
"""
@enum Interrupt begin
    InterruptVBlank=0x01
    InterruptLCDC=0x02
    InterruptTIMA=0x04
    InterruptSerial=0x08
    InterruptJoypad=0x10
    InterruptMask=0x1f
end

export Interrupt

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

@exportinstances Interrupt

Base.:|(a::UInt8, b::Interrupt)::UInt8 = a | UInt8(b)

end # module Interrupts