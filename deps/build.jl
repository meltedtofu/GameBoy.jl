
if Sys.iswindows()
    run(`zig version`; wait=true)
    cd(() -> run(`zig build-lib gameboy.c --library c -dynamic -target x86_64-windows-gnu`; wait=true), "emulator")
elseif Sys.islinux()
    run(`which clang`; wait=true)
    cd(() -> run(`clang -O3 -fPIC -shared gameboy.c -o libgameboy.so`; wait=true), "emulator")
elseif Sys.isapple()
    cd(() -> run(`clang -O3 -dynamiclib gameboy.c -o libgameboy.dylib`; wait=true), "emulator")
else
    throw("Unsupported Operating System")
end
