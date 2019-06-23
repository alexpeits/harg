{ mkDerivation, barbies, base, doctest, fetchgit, generic-lens
, hspec, lens, QuickCheck, stdenv
}:
mkDerivation {
  pname = "higgledy";
  version = "0.2.1.0";
  src = fetchgit {
    url = "https://github.com/i-am-tom/higgledy";
    sha256 = "0d14j8j7kri4j8sjsjw3x370q8jlaibihalzg3ryng7p59194nsh";
    rev = "ce88cc5f0ba1233f7ff3e6535acfc6b14e1ece78";
    fetchSubmodules = false;
  };
  libraryHaskellDepends = [ barbies base generic-lens QuickCheck ];
  testHaskellDepends = [
    barbies base doctest hspec lens QuickCheck
  ];
  homepage = "https://github.com/i-am-tom/higgledy";
  description = "Partial types as a type constructor";
  license = stdenv.lib.licenses.mit;
}
