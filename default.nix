# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? null, compiler ? null, withHoogle ? true }:

let

  nixpkgs = if isNull pkgs then
    import (import ./nix/sources.nix).nixos-stable { }
  else if builtins.typeOf pkgs == "set" then
    pkgs
  else
    import (builtins.getAttr pkgs (import ./nix/sources.nix)) { };

  inShell = nixpkgs.lib.inNixShell;

  haskellPackagesNoHoogle = if isNull compiler then
    nixpkgs.haskellPackages
  else
    nixpkgs.haskell.packages.${compiler};

  haskellPackagesWithHoogle = haskellPackagesNoHoogle.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  haskellPackagesBase = if (inShell && withHoogle) then
    haskellPackagesWithHoogle
  else
    haskellPackagesNoHoogle;

  # src = nixpkgs.nix-gitignore.gitignoreSource [ ] ./.;
  srcRegex = [
    "^docs.*$"
    "^harg.cabal$"
    "^LICENSE$"
    "^README.md$"
    "^Setup.hs$"
    "^src.*$"
    "^test.*$"
  ];
  src = nixpkgs.lib.sourceByRegex ./. srcRegex;
  drv = haskellPackages.callCabal2nix "harg" src { };

  haskellPackages = haskellPackagesBase.override {
    overrides = self: super:
      let
        packages = import ./nix/overrides.nix {
          pkgs = nixpkgs;
          self = self;
          super = super;
        };
      in packages // { "harg" = drv; };
  };

  shell = nixpkgs.mkShell {
    inputsFrom = [ haskellPackages.harg.env ];
    buildInputs = [ haskellPackages.ghcid haskellPackages.cabal-install ];
  };

in if inShell then shell else haskellPackages.harg
