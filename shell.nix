{nixpkgs ? import <nixpkgs> { }, compiler ? "ghc802"}:

let
  inherit (nixpkgs) pkgs;
  # at some point in the future I'd like to know how to pass in "enableLibraryProfiling" here
  ghc = pkgs.haskell.packages.${compiler}.ghcWithHoogle (ps: with ps; [
    repa
        ]);
in
nixpkgs.haskell.lib.buildStackProject {
  name = "default-stack-shell";
  buildInputs = with pkgs; [
    git zlib haskellPackages.intero haskellPackages.hlint llvm_37 gmp stack
  ];
  LANG = "en_US.UTF-8";
  inherit ghc;
}
