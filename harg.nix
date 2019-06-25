{ mkDerivation, barbies, base, higgledy, hpack, lib, markdown-unlit,
  optparse-applicative, stdenv, validation
}:
mkDerivation {
  pname = "harg";
  version = "0.1.0.0";
  src = lib.sourceByRegex ./. [
    "app(.*)?"
    "src(.*)?"
    "test(.*)?"
    "Example.hs"
    "README.lhs"
    "README.md"
    "harg.cabal"
    "package.yaml"
  ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    barbies base higgledy optparse-applicative validation
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    barbies base higgledy optparse-applicative validation
  ];
  testHaskellDepends = [
    barbies base higgledy optparse-applicative validation
  ];
  testToolDepends = [ markdown-unlit ];
  preConfigure = "hpack";
  homepage = "https://github.com/alexpeits/harg#readme";
  license = stdenv.lib.licenses.bsd3;
}
