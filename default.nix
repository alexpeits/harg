let
  pkgs = import <nixpkgs> {};
in
rec {
  harg = pkgs.haskellPackages.callPackage ./harg.nix {
    higgledy = pkgs.haskellPackages.callPackage ./nix/higgledy.nix {};
  };

  shell = pkgs.mkShell {
    inputsFrom = [ harg.env ];
    buildInputs = [
      pkgs.haskellPackages.ghcid
    ];
  };
}
