let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.09.tar.gz";
  }) {};

  # 2021-03-14 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "dae91f43317fd5ff207e11ea6bf4b6130e4ba9fc";
    sha256 = "1lx6dpa8g2xa6wwhqfarw4bixibk743r0cwafmqmq6l4qjb061sa";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-formless";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pkgs.nodejs-14_x
  ];
}
