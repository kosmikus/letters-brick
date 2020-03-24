with (import (builtins.fetchGit {
    name = "nixos-unstable-2019-10-01";
    url = https://github.com/nixos/nixpkgs/;
    rev = "05e6cc4944331e0e54ba05f5261d95102a4188d7";
  }) {}).pkgs;

# with (import <nixpkgs> {}).pkgs;

let
  ghc = haskell.packages.ghc883.ghcWithPackages
    (pkgs : with pkgs; []);
in
  stdenv.mkDerivation {
    name = "dev-brick";
    buildInputs = [ ghc zlib ncurses pkgconfig ];
    shellHook = ''
      eval $(grep export ${ghc}/bin/ghc)
      export NIX_GHC_LIBDIR="$(ghc --print-libdir)";
      export LD_LIBRARY_PATH="${zlib}/lib:${ncurses}/lib";
    '';
  }
