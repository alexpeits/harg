let
  pkgs = import <nixpkgs> {};
in
rec {
  origin = pkgs.haskellPackages.callPackage ./origin.nix {
    higgledy = pkgs.haskellPackages.callPackage ./nix/higgledy.nix {};
  };

  shell = pkgs.mkShell {
    inputsFrom = [ origin.env ];
    buildInputs = [
      pkgs.haskellPackages.ghcid
    ];
  };
}
