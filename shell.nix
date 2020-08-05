let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz";
  }) {};

  # 2020-08-01 nix-prefetch-git https://github.com/justinwoo/easy-purescript-nix
  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "7ff5a12af5750f94d0480059dba0ba6b82c6c452";
    sha256 = "0af25dqhs13ii4mx9jjkx2pww4ddbs741vb5gfc5ckxb084d69fq";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "halogen-formless";
  buildInputs = with pursPkgs; [
    pursPkgs.purs
    pursPkgs.spago
    pkgs.nodejs-12_x
  ];
}
