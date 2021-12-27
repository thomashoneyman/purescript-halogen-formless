let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) {};

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "13ace3addf14dd9e93af9132e4799b7badfbe99e";
    sha256 = "1gva113kyygjhn9i92vg6cyj15vhyfhq7haq51cvp4xdz4j0q4xn";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-formless";
  buildInputs = with pursPkgs; [
    pursPkgs.purs pursPkgs.spago pursPkgs.psa pursPkgs.purs-tidy
    pkgs.nodejs-16_x
  ];
}
