#! /bin/bash

set -e

RUST_DIR="rust_compiler"
CSHARP_DIR="csharp_mod"
RELEASE_DIR="release"

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
RUST_LINUX_BIN="$RUST_DIR/target/x86_64-unknown-linux-gnu/release/slang"
CHARP_DLL="$CSHARP_DIR/bin/Release/net46/StationeersSlang.dll"
CHARP_PDB="$CSHARP_DIR/bin/Release/net46/StationeersSlang.pdb"

# Check if the release dir exists, if not: create it.
if [[ ! -d "$RELEASE_DIR" ]]; then
  mkdir "$RELEASE_DIR"
fi

cp "$RUST_WIN_EXE" "$RELEASE_DIR/slang.exe"
cp "$RUST_LINUX_BIN" "$RELEASE_DIR/slang"
cp "$CHARP_DLL" "$RELEASE_DIR/StationeersSlang.dll"
cp "$CHARP_PDB" "$RELEASE_DIR/StationeersSlang.pdb"
