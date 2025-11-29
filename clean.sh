echo "Cleaning build directories"

CSHARP_DIR="csharp_mod"
RUST_DIR="rust_compiler"

cd "$CSHARP_DIR"
dotnet clean

cd ..

cd "$RUST_DIR"
cargo clean

cd ..

if [[ -d "release" ]]; then
  rm -rd release
fi
