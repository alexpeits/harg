# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? null, compiler ? null, withHoogle ? true, buildDocsTest ? false }:
let
  sources = import ./nix/sources.nix;
  nixpkgs =
    if builtins.typeOf pkgs == "set" then
      pkgs
    else if compiler == "ghc865" then
      import sources."nixos-19.09" { }
    else
      import sources."nixos-20.09" { };

  haskellPackagesBase =
    if isNull compiler then
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
        extraCabal2nixOptions =
          nixpkgs.lib.optionalString buildDocsTest "-fbuilddocstest";
        src = nixpkgs.lib.sourceByRegex ./. srcRegex;
        drv =
          self.callCabal2nixWithOptions "harg" src extraCabal2nixOptions { };
      in
      hsPkgs // { harg = drv; };
  };

  shell = haskellPackages.shellFor {
    packages = ps: [ ps.harg ];
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
    withHoogle = withHoogle;
  };

in
if nixpkgs.lib.inNixShell
then shell
else haskellPackages.harg
