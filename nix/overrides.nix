{ pkgs, self, super }:

let

  noTest = pkgs.haskell.lib.dontCheck;
  noHaddock = pkgs.haskell.lib.dontHaddock;

  higgledy-src = pkgs.fetchFromGitHub {
    owner = "i-am-tom";
    repo = "higgledy";
    rev = "476d73a92e3ef6e1dc879555d751f877b0f91de8";
    sha256 = "1vg9ha3knggyh5a76678y808c29v9p1mai9bq355q7amy0icy46j";
  };
  higgledy = self.callCabal2nix "higgledy" higgledy-src { };

in { higgledy = noTest (noHaddock higgledy); }
