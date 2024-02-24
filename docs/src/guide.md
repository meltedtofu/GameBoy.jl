# Guide

# Installation

This project is published in the [Melted Tofu Registry](https://github.com/meltedtofu/MeltedTofuRegistry). 
To use GameBoy.jl, add the registry (`]registry add https://github.com/meltedtofu/MeltedTofuRegistry.git`) and then use the usual package commands to install `]add GameBoy`.

# Constructing an Emulator

Create an emulator by calling the `Emulator` constructor with the path to a Game Boy rom file; `e = Emulator("path-to-rom.gb")`.

# Emulating a Frame

Emulate the next frame and return the pixels; `pixels = doframe!(e)`.

# Pressing a Button

Press a button with `buttonstate!`.
Don't forget to release it on the next frame - many games wait for the release to trigger the in-game action.

# Saving a Frame as an Image

The pixels can be saved as an image with [FileIO](https://github.com/JuliaIO/FileIO.jl) and [Images](https://github.com/JuliaImages/Images.jl).
Just make sure to `reinterpret` the pixels into the correct color space!

This is how I generated the reference images for the Blargg test roms.

```
using GameBoy
using FileIO, Images

e = Emulator_("test/roms/")
for _ ∈ 1:60*60
  doframe!(e)
end

pixels = doframe!(e)
save(File{format"PNG"}("blargg_cpu_instrs.png"), reinterpret(BGRA{N0f8}, pixels))
```

# Saving many Frames as a Video

Multiple frames can be stitched together into a video with [VideoIO](https://github.com/JuliaIO/VideoIO.jl).
The `target_pix_fmt` and `AV_PIX_FMT` are selected to be compatible with Quicktime Player.
I'm sure that there is a more elegant way to convert the pixels.
Submit a PR if you figure it out!

```
using GameBoy
using VideoIO

recorder = open_video_out(joinpath("gameboy.recording.mp4"),
                          VideoIO.VIO_PIX_FMT_DEF_ELTYPE_LU[VideoIO.AV_PIX_FMT_RGB24],
                          (144,160);
                          framerate=60,
                          target_pix_fmt=VideoIO.AV_PIX_FMT_YUV420P)

frame = Matrix{RGB{N0f8}}(undef, 144, 160)

for j in 1:144
    for jj in 1:160
        frame[j, jj] = convert(RGB{N0f8}, reinterpret(ARGB32, pixels[j, jj]))
    end
end

write(recorder, frame)
close_video_out!(recorder)
```
