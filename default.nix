# to see ghc versions:
# nix-instantiate --eval -E "with import <nixpkgs> {}; lib.attrNames haskell.compiler"
{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? null }:

let

  haskellPackages = if isNull compiler then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  harg = haskellPackages.callPackage ./nix/harg.nix {
    higgledy = haskellPackages.callPackage ./nix/higgledy.nix { };
  };

  shell = pkgs.mkShell {
    inputsFrom = [ harg.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='cabal new-repl' --restart=harg.cabal"
    '';
    buildInputs = [
      haskellPackages.ghc
      haskellPackages.cabal-install
      haskellPackages.ghcid
    ];
  };

in if pkgs.lib.inNixShell then shell else harg
