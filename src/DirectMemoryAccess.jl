module DirectMemoryAccess

using ..Component

"""
Direct Memory Access metadata
"""
mutable struct DMA
    delaystart::Bool
    pendingsource::UInt8
    source::UInt16 # source for currently running dma. 0 if not active
    active::Bool # Is OAM DMA active on current cycle?
    
    DMA() = new(false, 0, 0, false)
end

function Component.reset!(dma::DMA)::Nothing
    dma.delaystart = false
    dma.pendingsource = 0
    dma.source = 0
    active = false
    nothing
end

export DMA

end # module DirectMemoryAccess