{ mkDerivation, barbies, base, higgledy, lib, markdown-unlit,
  optparse-applicative, stdenv
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
  isExecutable = true;
  libraryHaskellDepends = [
    barbies base higgledy optparse-applicative
  ];
  executableHaskellDepends = [
    barbies base higgledy optparse-applicative
  ];
  testHaskellDepends = [
    barbies base higgledy optparse-applicative
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/alexpeits/harg#readme";
  license = stdenv.lib.licenses.bsd3;
}
