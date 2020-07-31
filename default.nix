# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? null, compiler ? null }:

let

  nixpkgs = if isNull pkgs then
    import (import ./nix/sources.nix).nixos-stable {}
  else if builtins.typeOf pkgs == "set" then
    pkgs
  else
    import (builtins.getAttr pkgs (import ./nix/sources.nix)) {};

  haskellPackagesBase = if isNull compiler then
    nixpkgs.haskellPackages
  else
    nixpkgs.haskell.packages.${compiler};

  srcRegex = [
    "^docs.*$"
    "^harg.cabal$"
    "^LICENSE$"
    "^README.md$"
    "^Setup.hs$"
    "^src.*$"
    "^test.*$"
  ];

  haskellPackages = haskellPackagesBase.override {
    overrides = self: super:
      let
        hsPkgs = import ./nix/overrides.nix {
          pkgs = nixpkgs;
          self = self;
          super = super;
        };
        src = nixpkgs.lib.sourceByRegex ./. srcRegex;
        drv = self.callCabal2nix "harg" src {};
      in
        hsPkgs // { harg = drv; };
  };

  shell = haskellPackages.shellFor {
    packages = ps: [ ps.harg ];
    buildInputs = [
      haskellPackages.ghcid
      haskellPackages.cabal-install
    ];
    withHoogle = true;
  };

in

if nixpkgs.lib.inNixShell
then shell
else haskellPackages.harg
