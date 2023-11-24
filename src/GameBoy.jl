module GameBoy

const gblib = abspath(joinpath(@__DIR__, 
                               "..", 
                               "deps", 
                               "emulator", 
                               Sys.iswindows() ? "gameboy.dll" 
                                               : (Sys.isapple() ? "libgameboy.dylib" 
                                                                : "libgameboy.so")))

macro exportinstances(enum)
    eval = GlobalRef(Core, :eval)
    return :($eval($__module__, Expr(:export, map(Symbol, instances($enum))...)))
end

struct Emulator
    g::Ptr{Cvoid}
    frameStride::Int

    Emulator() = new(ccall((:gameboy_alloc, gblib), Ptr{Cvoid}, ()),
		     ccall((:gameboy_frame_stride, gblib), Int, ()),
                    )
end

@enum Button ButtonDown=0x80 ButtonUp=0x40 ButtonLeft=0x20 ButtonRight=0x10 ButtonStart=0x08 ButtonSelect=0x04 ButtonB=0x02 ButtonA=0x01 NoOp=0x00
export Button
@exportinstances Button

# Guarantee that a, b, start, select button presses are always released on the next frame.
# Without this toggle button presses can result in a stuck emulator.
function execute(suggested::Button, gb::Emulator, release::Ref{Bool})
    if suggested == ButtonA || suggested == ButtonB || suggested == ButtonStart || suggested == ButtonSelect
        execute(release[] ? NoOp : suggested, gb)
        release[] = !release[]
    else
        execute(suggested, gb)
    end
end

function execute(suggested::Button, gb::Emulator)
    buttonstate!(gb, ButtonUp, false)
    buttonstate!(gb, ButtonDown, false)
    buttonstate!(gb, ButtonLeft, false)
    buttonstate!(gb, ButtonRight, false)
    buttonstate!(gb, ButtonA, false)
    buttonstate!(gb, ButtonB, false)
    buttonstate!(gb, ButtonStart, false)
    buttonstate!(gb, ButtonSelect, false)

    if suggested == NoOp
        return
    end

    buttonstate!(gb, suggested, true)
end

export execute


function free!(gb::Emulator)
    ccall((:gameboy_free, gblib), Cvoid, (Ptr{Cvoid},), gb.g)
end

function loadrom!(gb::Emulator, path::String)
    ccall((:gameboy_load_rom, gblib), Cvoid, (Ptr{Cvoid}, Cstring), gb.g, path)
    result = ccall((:gameboy_load, gblib), Cstring, (Ptr{Cvoid},), gb.g)
    if result != C_NULL
        free!(gb)
        error(unsafe_string(result))
    end
    nothing
end

# TODO: Use `unsafe_load` to directly deal with types from julia instead of requiring helper functions in the emulator library.

function ram(gb::Emulator)::Vector{UInt8}
    len = ccall((:gameboy_cart_ram_size, gblib), UInt, (Ptr{Cvoid},), gb.g)
    cbytes = ccall((:gameboy_cart_ram, gblib), Ptr{UInt8}, (Ptr{Cvoid},), gb.g)
    deepcopy(unsafe_wrap(Vector{UInt8}, cbytes, (len)))
end

function ram!(gb::Emulator, data::Vector{UInt8})
    ccall((:gameboy_load_ram, gblib), Cvoid, (Ptr{Cvoid}, UInt, Ptr{UInt8}), gb.g, length(data), data)
    nothing
end

function reset!(gb::Emulator)
    ccall((:gameboy_reset, gblib), Cvoid, (Ptr{Cvoid}, Bool), gb.g, true)
    nothing
end

function doframe!(gb::Emulator)::Ptr{Cvoid}
    ccall((:gameboy_do_frame, gblib), Ptr{Cvoid}, (Ptr{Cvoid},), gb.g)
end

function buttonstate!(gb::Emulator, b::Button, pressed::Bool)
    ccall((:gameboy_setButtonState, gblib), Cvoid, (Ptr{Cvoid}, Cint, Bool), gb.g, b, pressed)
    nothing
end

function read(gb::Emulator, addr::UInt16)::UInt8
    ccall((:gameboy_read, gblib), UInt8, (Ptr{Cvoid}, Cint), gb.g, addr)
end

function sanity_check()::UInt8
    ccall((:gameboy_sanity_check, gblib), UInt8, ())
end

export Emulator, free!, loadrom!, reset!, doframe!, buttonstate!, read, ram, ram!

export gblib

end # module GameBoy