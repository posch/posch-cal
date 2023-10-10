{ pkgs ? import <nixpkgs> {} }:
let
  my-ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
    ansi-terminal
    hscurses
  ]);
in pkgs.mkShell {
  buildInputs = with pkgs; [
    my-ghc
    cabal-install
    ncurses
  ];
}
