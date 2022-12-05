{ pkgs ? import <nixpkgs-unstable> {
    config = {
      packageOverrides = pkgs: {
        haskell-language-server = pkgs.haskell-language-server.override {
          supportedGhcVersions = [ "924" ];
          # ormolu_0_5_0_1 = pkgs.ormolu_0_5_0_1.override { ghc-lib-parser = pkgs.haskellPackages.ghc-lib-parser_9_2_5_20221107; };
        };
      };
    };
  }
}:
let

  # ormolu = pkgs.stdenv.mkDerivation {
  #   name = "ormolu";
  #   # taken from https://stackoverflow.com/questions/57225745/how-to-disable-unpack-phase-to-prevent-error-do-not-know-how-to-unpack-source-a
  #   # Renamed to imply that 'src' functionality is not being used.
  #   src = pkgs.fetchzip {
  #     url = "https://github.com/tweag/ormolu/releases/download/0.5.0.1/ormolu-macOS.zip";
  #     sha256 = "sha256-jfqwNDmtoPZ1l+/Et0sBz5s5H7mX+I8Tiqe5+679eIs=";
  #     # postFetch = ''
  #     #   rm -v !(ormolu)
  #     # '';
  #     stripRoot = false;
  #   }; # this is executeable file

  #   installPhase = ''
  #     mkdir -p $out/bin
  #     mv * $out/bin
  #   '';

  #   # phases = [ "installPhase" ]; # Removes all phases except installPhase

  #   # installPhase = ''
  #   #   mkdir -p $out/bin
  #   #   # had to change \$\{executable} $executable
  #   #   cp $executable $out/bin/lamdera
  #   #   chmod a+x $out/bin/lamdera
  #   # '';
  # };
  ormolu = pkgs.fetchFromGitHub {
    owner = "tweag";
    repo = "ormolu";
    rev = "1ec862c4e8a14a07cdf08701c8acadbd038f8c4d"; # update as necessary
    # do not forget to update the hash:
    sha256 = "sha256-v+qzksjAnA3i7GtRH2Nj+DUB+EJlmc0x4yTpuXF26aA=";
  };
in
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
    pkgs.haskellPackages.record-dot-preprocessor
    pkgs.treefmt
    # ormolu
    # pkgs.ormolu
    # pkgs.haskellPackages.ormolu_0_5_0_1
    (import ormolu {
      ormoluCompiler = "ghc924";
    }).ormoluExe

    pkgs.nixpkgs-fmt
    pkgs.clang
    pkgs.libcxx
    pkgs.haskellPackages.hspec-discover
    pkgs.stack
  ];
}
