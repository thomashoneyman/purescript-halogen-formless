{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    nixpkgs.nodejs
    nixpkgs.yarn 
    nixpkgs.stack 
  ];
}
