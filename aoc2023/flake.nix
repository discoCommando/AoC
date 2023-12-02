{
  description = "A simple Haskell project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";  # Adjust the channel as needed
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ghc = pkgs.haskellPackages.ghc;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ ghc pkgs.haskellPackages.cabal-install pkgs.haskell-language-server ];
        };
      }
    );
}
