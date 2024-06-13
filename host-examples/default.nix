{ mkDerivation, base, dependent-map, dependent-sum, doctest, lens
, mtl, QuickCheck, ref-tf, reflex, stdenv, transformers
}:
mkDerivation {
  pname = "reflex-host-examples";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base dependent-map dependent-sum lens mtl ref-tf reflex
    transformers
  ];
  testHaskellDepends = [ base doctest QuickCheck ];
  license = stdenv.lib.licenses.bsd3;
}
