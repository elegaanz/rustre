{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [ pkgs.mdbook pkgs.mdbook-katex pkgs.mdbook-plantuml pkgs.python3 ];
}