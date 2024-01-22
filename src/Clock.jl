module Clock_

using ..Component
using ..IO
using ..Interrupts

"""
Main System Clock
"""
mutable struct Clock{T}
    cycles::UInt16
    overflow::Bool
    loading::Bool
    mmu::Base.RefValue{T}
    
    Clock{T}() where {T} = Clock{T}(nothing)
    Clock{T}(t::Union{Nothing, T}) where {T} = new(0, 
                                                   false, 
                                                   false, 
                                                   isnothing(t) ? Ref{T}() : Ref(t))
end

function clock_getTimerBit(control::UInt8, cycles::UInt16)::Bool
    switch = control & 0x03
    if switch == 0x00
        cycles & (0x0001 << 9) > 0
    elseif switch == 0x01
        cycles & (0x0001 << 3) > 0
    elseif switch == 0x02
        cycles & (0x0001 << 5) > 0
    elseif switch == 0x03
        cycles & (0x0001 << 7) > 0
    end
end

function clock_timerIncrement!(clock::Clock)::Nothing
    timer = readb(clock.mmu[].io, IOTimerCounter)
    if timer == 0xff
        clock.overflow = true
    end
    write!(clock.mmu[].io, IOTimerCounter, timer + 0x01)
    nothing
end

function clock_countChange!(clock::Clock, newval::UInt16)
    tac = readb(clock.mmu[].io, IOTimerControl)
    if tac & 0x04 > 0x00
        if !clock_getTimerBit(tac, newval) && clock_getTimerBit(tac, clock.cycles)
            clock_timerIncrement!(clock)
        end
    end
    clock.cycles = newval
    write!(clock.mmu[].io, IODivider, UInt8(newval >> 0x08))
end

function Component.step!(clock::Clock)::Nothing
    clock.loading = false
    if clock.overflow
        # Delayed overflow effects
        write!(clock.mmu[].io, IOInterruptFlag, readb(clock.mmu[].io, IOInterruptFlag) | InterruptTIMA)
        clock.overflow = false

        # modulo is being loaded in next machine cycle
        write!(clock.mmu[].io, IOTimerCounter, readb(clock.mmu[].io, IOTimerModulo))
        clock.loading = true
    end
    
    clock_countChange!(clock, clock.cycles + 0x04)
end


function tac!(clock::Clock, val::UInt8)::Nothing
    old = readb(clock.mmu[].io, IOTimerControl)
    write!(clock.mmu[].io, IOTimerControl, val)
    
    oldbit = (old & 0x04) > 0x00 && clock_getTimerBit(old, clock.cycles)
    newbit = (val & 0x04) > 0x00 && clock_getTimerBit(val, clock.cycles)
    
    # check for falling edge
    if oldbit && !newbit
        clock_timerIncrement!(clock)
    end
    nothing
end

function Component.reset!(c::Clock)
    c.cycles = 0
    c.overflow = false
    c.loading = false
end

div!(clock::Clock) = clock_countChange!(clock, 0x0000)

export Clock, tac!, div!

end # module Clock_