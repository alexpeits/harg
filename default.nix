{ pkgs ? import <nixpkgs> {}
, compiler ? "ghc865"
, inNixShell ? pkgs.lib.inNixShell
}:

let

  nixpkgs-pinned = pkgs.fetchgit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "f5cb9bd30327caf0091cae1dfd31a4338b81e986";
    sha256 = "1aa8pck1fha6rj3n5f1pr2v3hp6rd2061aixq9cb8bi2hbq1rag5";
    fetchSubmodules = false;
  };
  nixpkgs = import nixpkgs-pinned {};

  haskellPackages = nixpkgs.haskell.packages.${compiler};

  higgledy-src = nixpkgs.fetchgit {
    url = "http://github.com/i-am-tom/higgledy";
    sha256 = "1vg9ha3knggyh5a76678y808c29v9p1mai9bq355q7amy0icy46j";
    rev = "476d73a92e3ef6e1dc879555d751f877b0f91de8";
    fetchSubmodules = true;
  };

  harg-src = nixpkgs.nix-gitignore.gitignoreSource [] ./.;

  # haskellPackages.callCabal2nix not working
  harg = (pkgs.haskellPackages.callCabal2nix "harg" harg-src {}).override {
    higgledy = pkgs.haskellPackages.callCabal2nix "higgledy" higgledy-src {};
  };

  shell = nixpkgs.mkShell {
    inputsFrom = [ harg.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='cabal new-repl'"
    '';
    buildInputs = [
      haskellPackages.cabal-install
      haskellPackages.ghc
      haskellPackages.ghcid
      haskellPackages.hlint
    ];
  };

in

if inNixShell then shell else harg
