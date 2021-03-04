{ pkgs, self, super }:
let
  noTest = pkgs.haskell.lib.dontCheck;
  noHaddock = pkgs.haskell.lib.dontHaddock;

  barbies-src = pkgs.fetchFromGitHub {
    owner = "jcpetruzza";
    repo = "barbies";
    rev = "e451aa06587f4d2b80ce1a4e834c510233ce53e2";
    sha256 = "0l2v5av1pbj8zhmfdqyq088hnzgbdpkyn2j0fa05a9yp6bq2jvxr";
  };
  barbies = self.callCabal2nix "barbies" barbies-src { };

  higgledy-src = pkgs.fetchFromGitHub {
    owner = "i-am-tom";
    repo = "higgledy";
    rev = "74848cfe2af6f2021d92fc19e182973122d547e9";
    sha256 = "1al2sla1qn06nynxq23p5366ihrdckgw4q31vnn2cm3q1mwpxs01";
  };
  higgledy = self.callCabal2nix "higgledy" higgledy-src { };

  generic-lens-pkgs-src = pkgs.fetchFromGitHub {
    owner = "kcsongor";
    repo = "generic-lens";
    rev = "a9a6fc3fe9c9a9cc79d31448db118413d9ca885e";
    sha256 = "14wz6ng88f9027cfpn51shb24i6013zqqm8czm8xzhnllaz5zf40";
  };
  generic-lens-src = "${generic-lens-pkgs-src}/generic-lens";
  generic-lens = self.callCabal2nix "generic-lens" generic-lens-src { };
  generic-lens-core-src = "${generic-lens-pkgs-src}/generic-lens-core";
  generic-lens-core = self.callCabal2nix "generic-lens-core" generic-lens-core-src { };

  optics-src = pkgs.fetchFromGitHub {
    owner = "well-typed";
    repo = "optics";
    rev = "39739125da7e9e38631a659f4e8c65f660483cf1";
    sha256 = "1hfpn3khjjdkv0rdrd8yd9syis757760vsglvvws9wbr3553spg1";
  };
  indexed-profunctors-src = "${optics-src}/indexed-profunctors";
  indexed-profunctors = self.callCabal2nix "indexed-profunctors" indexed-profunctors-src { };

in
{
  barbies = noTest (noHaddock barbies);
  higgledy = noTest (noHaddock higgledy);
  generic-lens = noTest (noHaddock generic-lens);
  generic-lens-core = noTest (noHaddock generic-lens-core);
  indexed-profunctors = noTest (noHaddock indexed-profunctors);
}
