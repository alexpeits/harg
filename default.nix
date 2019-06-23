let
  # pkgs = import ./pinned-nixpkgs.nix {}
  pkgs = import <nixpkgs> {};
in
rec {
  configuration = pkgs.haskellPackages.callPackage ./configuration.nix {
    higgledy = pkgs.haskellPackages.callPackage ./nix/higgledy.nix {};
  };

  shell = pkgs.mkShell {
    inputsFrom = [ configuration.env ];
    buildInputs = [
      pkgs.haskellPackages.ghcid
    ];
  };
}
