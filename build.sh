#! /bin/bash

set -e

RUST_DIR="rust_compiler"
CSHARP_DIR="csharp_mod"
RELEASE_DIR="release"
METADATA_DIR="ModData"

export RUSTFLAGS="--remap-path-prefix=${PWD}=. --remap-path-prefix=${HOME}/.cargo=~/.cargo"

echo "--------------------"
cd "$RUST_DIR"
echo "Building native Rust binaries and libs"

# -- Build for Native (Linux x86-64) --
cargo build --release --target=x86_64-unknown-linux-gnu

# -- Build for Windows (x86-64) --
cargo build --release --target=x86_64-pc-windows-gnu

# -- Generate C# Headers --
cargo run --features headers --bin generate-headers

cd ..
echo "--------------------"

echo "Building C# mod"
echo "--------------------"

cd "$CSHARP_DIR"
dotnet build -c Release

cd ..
echo "--------------------"

echo "Copying Release files to output directory"
echo "--------------------"

RUST_WIN_EXE="$RUST_DIR/target/x86_64-pc-windows-gnu/release/slang.exe"
RUST_WIN_DLL="$RUST_DIR/target/x86_64-pc-windows-gnu/release/slang.dll"
RUST_LINUX_BIN="$RUST_DIR/target/x86_64-unknown-linux-gnu/release/slang"
CSHARP_DLL="$CSHARP_DIR/bin/Release/net48/StationeersSlang.dll"

# Remove the release directory if it exists so we have a fresh build dir
if [[ -d "$RELEASE_DIR" ]]; then
  rm -rd "$RELEASE_DIR"
fi

if [[ ! -d "$RELEASE_DIR" ]]; then
  mkdir "$RELEASE_DIR"
fi

# This is the windows binary
cp "$RUST_WIN_EXE" "$RELEASE_DIR/slang.exe"
# This is the linux executable
cp "$RUST_LINUX_BIN" "$RELEASE_DIR/slang"
# This is the DLL mod itself
cp "$CSHARP_DLL" "$RELEASE_DIR/StationeersSlang.dll"
# This is the rust-only compiler for use in injecting into the mod
cp "$RUST_WIN_DLL" "$RELEASE_DIR/rust_slang.dll"
# This is the whole bundled workshop release version of the mod
cp -r "$METADATA_DIR" "$RELEASE_DIR/workshop"
cp "$CSHARP_DLL" "$RELEASE_DIR/workshop/StationeersSlang.dll"
