{ mkDerivation, barbies, base, doctest, fetchgit, generic-lens
, hspec, lens, markdown-unlit, QuickCheck, stdenv
}:
mkDerivation {
  pname = "higgledy";
  version = "0.3.0.0";
  src = fetchgit {
    url = "http://github.com/i-am-tom/higgledy";
    sha256 = "1vg9ha3knggyh5a76678y808c29v9p1mai9bq355q7amy0icy46j";
    rev = "476d73a92e3ef6e1dc879555d751f877b0f91de8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ barbies base generic-lens QuickCheck ];
  testHaskellDepends = [
    barbies base doctest hspec lens QuickCheck
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/i-am-tom/higgledy";
  description = "Partial types as a type constructor";
  license = stdenv.lib.licenses.mit;
}
