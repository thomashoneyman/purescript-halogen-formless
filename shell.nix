let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
  }) {};

  # 2021-03-14 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "e8a1ffafafcdf2e81adba419693eb35f3ee422f8";
    sha256 = "0bk32wckk82f1j5i5gva63f3b3jl8swc941c33bqc3pfg5cgkyyf";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-formless";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pkgs.nodejs-14_x
  ];
}
