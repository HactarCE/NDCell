# NDCA

This is a testing ground for a cellular automaton description language for [NDCell](https://github.com/HactarCE/NDCell).

## Short-term to-do list

* [x] Compile helper functions
* [x] Calls to built-in functions
* [x] Calls to built-in methods
* [x] Vector operations
* [x] Unary `+` operator
* [x] Refactor parser
* [x] `is` expressions
* [x] cell state filters
* [x] `for` loops
* [ ] pattern masks
* [ ] patterns
* [ ] pattern matchers
* [ ] merge to `master`!

## Building on Linux or macOS

1. Download/install Cargo.
2. Download [LLVM](https://releases.llvm.org/download.html) 10.0.0 precompiled binaries and extract them somewhere.
3. Either put those LLVM binaries somewhere in your path so that they are accessible on the command line, or set an environment variable `LLVM_SYS_60_PREFIX` containing the path pointing to wherever you extracted LLVM.
4. Clone this project and build/run:

```sh
git clone https://github.com/HactarCE/NDCA
cd NDCA
cargo build # or cargo run
```

## Building on Windows

What you'll need:

* Rustup
* CMake
* Visual Studio + Visual Studio Build Tools

### Part 1 - Rust setup

1. Download/install [Rustup](https://www.rust-lang.org/tools/install).
2. Run `rustup.exe toolchain install stable-x86_64-pc-windows-msvc` to install the MSVC toolchain.
3. Run `rustup.exe default stable-msvc` to select that toolchain as the default.
4. Download/install [Build Tools for Visual Studio 2019](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019). (If you're reading this in the future, you can probably use a later version but you may have to adjust later commands if they include `Visual Studio 16 2019`.)
5. Just install Visual Studio IDE too, because we'll need that later in order to build LLVM.
6. To check if this all works, try making a new Rust project somewhere with `cargo new name-of-project`, `cd` into it, and run `cargo run`. It should compile and run successfully. If `rustc` can't find things, try rebooting.

### Part 2 - Building LLVM

Fair warning: I don't do C or C++ development so it's entirely possible that I've completely butchered the proper build process, but this is what finally worked for me.

If you don't want to build LLVM from source (and you probably don't) but you trust me, you can download everything you need from [here](https://1drv.ms/u/s!AnInQYQ5foGSi7hmxnhb-hG1mCmWAw?e=HVPNn9). (SHA256 = `676a81c273653c87682696afa0d73762cf07720d637e57439d743fe4321ea342`) Extract that somewhere and skip to step #11, using the path where you extracted it place of `C:\LLVM_solution\MinSizeRel`. I can't promise that this will work, but it's waaaay easier than compiling LLVM yourself.

1. Download/install [CMake](https://cmake.org/download/). WSL or Cygwin `cmake` might work, but I wouldn't count on it.
2. Download [LLVM](https://releases.llvm.org/download.html) 10.0.0 source code. The precompiled binaries probably won't work, but you can try.
3. Extract the LLVM source code somewhere, like `C:\LLVM_source_code`. Now you should have a bunch of folders and files directly inside `C:\LLVM_source_code` including `CMakeLists.txt`.
4. Make a new empty folder, like `C:\LLVM_solution`.
5. Run this, replacing the path names accordingly if you used different paths for things:

```bat
cd C:\LLVM_solution
cmake.exe 'C:\LLVM_source_code' -Thost=x64 -DLLVM_INCLUDE_EXAMPLES=OFF -DLLVM_INCLUDE_TESTS=OFF -DLLVM_INCLUDE_BENCHMARKS=OFF -DLLVM_ENABLE_IDE=1
```

If that doesn't work for some reason, you can try adding `-G "Visual Studio 16 2019" -A x64` on the end of the `cmake` command.

By the way, **do not use cmake-gui**. I don't think there's any way to make it use `-Thost=x64`, and I'm pretty sure that's important because otherwise you get 32-bit binaries or something.

6. Open Visual Studio and load the project at `C:\LLVM_solution`.
7. Make sure `MinSizeRel` is selected as the build configuration (it might be `Debug` by default) and build the project. This took around 30 minutes on my computer and it uses all of your CPU cores, so go eat lunch or something while you wait.
8. Now there should be a folder at `C:\LLVM_solution\MinSizeRel` that contains folders named `bin` and `lib`. If that's there, good.
9. Copy `C:\LLVM_solution\include` into `C:\LLVM_solution\MinSizeRel` so that the new `include` folder is next to `bin` and `lib`.
10. Also copy the contents of `C:\LLVM_source_code\include` into `C:\LLVM_solution\MinSizeRel\include`, merging the contents. There might be a file or two that are overwritten and that's fine.
11. Finally, make a new environment variable (system variable or user variable, doesn't matter) called `LLVM_SYS_80_PREFIX` with the value `C:\LLVM_solution\MinSizeRel`. Reboot to make sure this takes effect.

### Part 3 - Building NDCA

1. Download this project and extract it somewhere.
2. Open a terminal in the folder where you extracted NDCA (it should have `Cargo.toml` in it) and build it using `cargo build` or run it using `cargo run`.
