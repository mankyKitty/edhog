{ mkDerivation, base, bytestring, hedgehog, lens, process, stdenv
, text, typed-process, unix
}:
mkDerivation {
  pname = "fp-eedee";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring hedgehog lens process text typed-process unix
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
