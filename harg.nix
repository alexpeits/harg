{ mkDerivation, lib, markdown-unlit, base, aeson, bytestring, text, barbies,
  higgledy, optparse-applicative, stdenv
}:
mkDerivation {
  pname = "harg";
  version = "0.1.0.0";
  src = lib.sourceByRegex ./. [
    "src(.*)?"
    "test(.*)?"
    "Example.hs"
    "README.lhs"
    "README.md"
    "harg.cabal"
    "LICENSE"
  ];
  isLibrary = true;
  libraryHaskellDepends = [
    aeson barbies base bytestring higgledy optparse-applicative text
  ];
  testHaskellDepends = [
    barbies base higgledy optparse-applicative
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/alexpeits/harg#readme";
  license = stdenv.lib.licenses.bsd3;
}
