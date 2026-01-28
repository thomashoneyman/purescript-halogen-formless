{
  description = "Halogen Formless";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ purescript-overlay.overlays.default ];
        };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            git
            purs
            purescript-language-server
            purs-tidy
            spago-unstable
          ];
        };

        checks.purescript-format = pkgs.runCommand "purescript-format" { buildInputs = [ pkgs.purs-tidy ]; } ''
          ${pkgs.purs-tidy}/bin/purs-tidy check src example
          touch $out
        '';
      });
}
