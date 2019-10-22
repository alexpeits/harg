# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? null, compiler ? null, withHoogle ? true }:

let

  nixpkgs = if isNull pkgs then
    import (import ./nix/sources.nix).nixos-stable { }
  else
    if builtins.typeOf pkgs == "set" then
      pkgs
    else
      import (builtins.getAttr pkgs (import ./nix/sources.nix)) {};

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

  haskellPackages = if (inShell && withHoogle) then
    haskellPackagesWithHoogle
  else
    haskellPackagesNoHoogle;

  harg-src = nixpkgs.nix-gitignore.gitignoreSource [] ./.;
  higgledy-src = nixpkgs.fetchFromGitHub {
    owner = "i-am-tom";
    repo = "higgledy";
    rev = "476d73a92e3ef6e1dc879555d751f877b0f91de8";
    sha256 = "1vg9ha3knggyh5a76678y808c29v9p1mai9bq355q7amy0icy46j";
  };

  harg = nixpkgs.haskellPackages.callCabal2nix "harg" harg-src {
    higgledy = nixpkgs.haskellPackages.callCabal2nix "higgledy" higgledy-src { };
  };

  shell = nixpkgs.mkShell {
    inputsFrom = [ harg.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='cabal new-repl' --restart=harg.cabal"
    '';
    buildInputs = [ haskellPackages.ghcid haskellPackages.cabal-install ];
  };

in if inShell then shell else harg
