let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21.11.tar.gz";
  }) {};

  pursPkgs = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "aa72388ca0fb72ed64467f59a121db1f104897db";
    sha256 = "1j37v3ncnakhq7p4l2vqdn4li8bgwcc8cd2hk2fblxhnlglikgx2";
  }) { inherit pkgs; };

in pkgs.stdenv.mkDerivation {
  name = "halogen-formless";
  buildInputs = with pursPkgs; [
    pursPkgs.purs pursPkgs.spago pursPkgs.psa pursPkgs.purs-tidy
    pkgs.nodejs-16_x
  ];
}
