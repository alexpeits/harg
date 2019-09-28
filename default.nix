{ pkgs ? import ./nix/nixpkgs.nix {} }:

let

  harg = pkgs.haskellPackages.callPackage ./nix/harg.nix {
    higgledy = pkgs.haskellPackages.callPackage ./nix/higgledy.nix {};
  };

  shell = pkgs.mkShell {
    inputsFrom = [ harg.env ];
    shellHook = ''
      alias ghcid-orig="$(which ghcid)"
      alias ghcid="ghcid -a --command='cabal new-repl' --restart=harg.cabal"
    '';
    buildInputs = [
      pkgs.haskellPackages.ghc
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
    ];
  };

in

if pkgs.lib.inNixShell then shell else harg
