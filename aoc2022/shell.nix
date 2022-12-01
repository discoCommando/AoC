{ pkgs ? import <nixpkgs-unstable> {
    config = {
      packageOverrides = pkgs: {
        haskell-language-server = pkgs.haskell-language-server.override { supportedGhcVersions = [ "924" ]; };
      };
    };
  }
}:
pkgs.mkShell {
  # nativeBuildInputs is usually what you want -- tools you need to run
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc92
    pkgs.cabal-install
    # pkgs.haskell-language-server.override {supportedGhcVersions = ["924"];}
    pkgs.haskell-language-server

    # pkgs.haskellPackages.haskell-language-server
    pkgs.hpack
    pkgs.haskellPackages.implicit-hie
    pkgs.treefmt
    pkgs.ormolu
    pkgs.nixpkgs-fmt
    pkgs.clang
    pkgs.libcxx
  ];
}
