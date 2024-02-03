module IO

"""
Addresses of IO devices relative to 0xff00

Intentionally skipping sound since it is unimplemented in this emulator
"""
@enum IORegisters begin
    IOJoypad=0x00
    IOSerialData=0x01
    IOSerialControl=0x02
    IODivider=0x04
    IOTimerCounter=0x05
    IOTimerModulo=0x06
    IOTimerControl=0x07
    IOInterruptFlag=0x0f
    IOLCDControl=0x40
    IOLCDStat=0x41
    IOScrollY=0x42
    IOScrollX=0x43
    IOLCDY=0x44
    IOLCDYCompare=0x45
    IOOAMDMA=0x46
    IOBackgroundPalette=0x47
    IOObjectPalette0=0x48
    IOObjectPalette1=0x49
    IOWindowY=0x4a
    IOWindowX=0x4b
    IOBootRomDisable=0x50
end

Base.:+(a::UInt16, b::IORegisters)::UInt16 = a + UInt16(b)
Base.to_index(reg::IORegisters) = Int(reg)

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

@exportinstances IORegisters

export IORegisters

end # module IO