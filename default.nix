with import <nixpkgs> {};

# Nix environment to make Haskell IDE work
stdenv.mkDerivation {
  name = "advent-of-code-haskell";
  version = "1.0";
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [
    libffi
    stack
    ncurses
    zlib
    glibc
  ];
  LD_LIBRARY_PATH = "${glibc}/lib/";
}