{ stdenv, gambit, openssl }:

stdenv.mkDerivation {
  name = "taskwarrior-hooks";
  src = ./.;

  buildInputs = [ gambit openssl ];
  makeFlags = [ "prefix=${placeholder "out"}" ];
}
