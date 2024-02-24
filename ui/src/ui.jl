module ui

using GameBoy

using SimpleDirectMediaLayer.LibSDL2

function runloop(rompath::String; vsync=true)::Nothing
    SDL_Init(SDL_INIT_EVERYTHING)
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "0")

    width = 160
    height = 144
    scale = 4

    window = SDL_CreateWindow("GameBoy.jl",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              width*scale, height*scale,
                              SDL_WINDOW_RESIZABLE,
                             )

    rect = Ref(SDL_Rect(0, 0, width, height))

    if vsync
        renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_PRESENTVSYNC)
    else
        renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED)
    end
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "best")
    SDL_RenderSetLogicalSize(renderer, width, height)

    emu = Emulator(rompath)

    pixelbuf = zeros(UInt32, width*height)

    frametexture = SDL_CreateTexture(renderer, 
                                     SDL_PIXELFORMAT_ARGB8888, 
                                     SDL_TEXTUREACCESS_STREAMING, 
                                     width,
                                     height
                                    )

    # TODO: sdl_connect_controller?

    stop = false
    while !stop
        keystates = SDL_GetKeyboardState(C_NULL) # UInt8 *
        if unsafe_load(keystates, convert(Int32, SDL_SCANCODE_ESCAPE)+1) > 0
            stop = true
            break
        end

        up = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_W)+1) == 0
        down = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_S)+1) == 0
        left = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_A)+1) == 0
        right = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_D)+1) == 0
        a = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_J)+1) == 0
        b  = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_K)+1) == 0
        start = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_U)+1) == 0
        select = unsafe_load(keystates, convert(Int32, SDL_SCANCODE_I)+1) == 0
        buttonstate!(emu, GameBoy.ButtonUp, up)
        buttonstate!(emu, GameBoy.ButtonDown, down)
        buttonstate!(emu, GameBoy.ButtonLeft, left)
        buttonstate!(emu, GameBoy.ButtonRight, right)
        buttonstate!(emu, GameBoy.ButtonA, a)
        buttonstate!(emu, GameBoy.ButtonB, b)
        buttonstate!(emu, GameBoy.ButtonStart, start)
        buttonstate!(emu, GameBoy.ButtonSelect, select)

        pixels = doframe!(emu)
        
        for j in 1:height
            for jj in 1:width
                pixelbuf[(j-1)*width + jj] = pixels[j, jj]
            end
        end

        SDL_SetRenderDrawColor(renderer, 0xc4, 0xcf, 0xa1, 0xff)
        SDL_RenderClear(renderer)

        SDL_UpdateTexture(frametexture, C_NULL, pixelbuf, width*4) # Assumes that UInt32 is 4 bytes wide
        SDL_RenderCopy(renderer, frametexture, C_NULL, rect)

        SDL_RenderPresent(renderer)

        event_ref = Ref{SDL_Event}()
        while Bool(SDL_PollEvent(event_ref))
            evt = event_ref[]
            if evt.type == SDL_QUIT
                stop = true
            end
        end
    end
    
    SDL_DestroyWindow(window)
    SDL_Quit()

    nothing
end


export runloop

end # module ui
