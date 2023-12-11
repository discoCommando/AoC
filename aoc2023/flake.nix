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
        ghc = pkgs.haskell.compiler.ghc948;
      in
      {
        # instruction for installing:
        # Install pkgs.haskell-language-server first
        # run `haskell-language-server-wrapper` to see which ghc version it was compiled with 
        # Then just install a compatible ghc version.
        devShell = pkgs.mkShell {
          buildInputs = [ ghc pkgs.haskellPackages.cabal-install pkgs.haskell-language-server pkgs.ghcid pkgs.haskellPackages.implicit-hie pkgs.hpack pkgs.treefmt     pkgs.haskellPackages.hspec-discover

           ];
        };
      }
    );
}
