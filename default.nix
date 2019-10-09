# to see ghc versions:
# nix-instantiate --eval -E "with import ./nix/nixpkgs.nix {}; lib.attrNames haskell.compiler"
{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null, withHoogle ? true }:

let

  haskellPackagesNoHoogle = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  haskellPackagesWithHoogle = haskellPackagesNoHoogle.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
    });
  };

  haskellPackages =
    if withHoogle then haskellPackagesWithHoogle else haskellPackagesNoHoogle;

  higgledy-src = pkgs.fetchFromGitHub {
    owner = "i-am-tom";
    repo = "higgledy";
    rev = "476d73a92e3ef6e1dc879555d751f877b0f91de8";
    sha256 = "1vg9ha3knggyh5a76678y808c29v9p1mai9bq355q7amy0icy46j";
  };

  harg = haskellPackages.callPackage ./nix/harg.nix {
    # haskellPackages.callCabal2nix not always working
    higgledy = pkgs.haskellPackages.callCabal2nix "higgledy" higgledy-src { };
  };

  shell = pkgs.mkShell {
    inputsFrom = [ harg.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='cabal new-repl' --restart=harg.cabal"
    '';
    buildInputs = [ haskellPackages.ghcid ];
  };

in if pkgs.lib.inNixShell then shell else harg
