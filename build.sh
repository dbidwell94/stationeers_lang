#! /bin/bash

set -e

export RUSTFLAGS="--remap-path-prefix=${PWD}=. --remap-path-prefix=${HOME}/.cargo=~/.cargo"

# -- Build for Native (Linux x86-64) --
echo "Building native (Linux x86-64) executable..."
cargo build --release --target=x86_64-unknown-linux-gnu
echo "Native build complete."
echo "--------------------"

# -- Build for Windows (x86-64) --
echo "Building Windows (x86-64) dll and executable..."
cargo build --release --target=x86_64-pc-windows-gnu
echo "Windows build successful."
echo "--------------------"

echo "All builds successful"
echo "Linux executable at target/x86_64-unknown-linux-gnu/release/slang"
echo "Windows .exe at target/x86_64-pc-windows-gnu/release/slang.exe"
echo "Windows .dll at target/x86_64-pc-windows-gnu/release/slang.dll"
