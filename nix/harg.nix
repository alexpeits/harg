{ mkDerivation, aeson, barbies, base, bytestring, directory
, higgledy, markdown-unlit, optparse-applicative, split, stdenv
, text, yaml
}:
mkDerivation {
  pname = "harg";
  version = "0.4.0.0";
  sha256 = "0il36pwzmkc1dj39kybyp6hv4s7d60dl5rx1fkfm23dcg6a9r0dc";
  libraryHaskellDepends = [
    aeson barbies base bytestring directory higgledy
    optparse-applicative split text yaml
  ];
  testHaskellDepends = [
    aeson barbies base higgledy optparse-applicative
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://github.com/alexpeits/harg";
  description = "Haskell program configuration using higher kinded data";
  license = stdenv.lib.licenses.bsd3;
}
