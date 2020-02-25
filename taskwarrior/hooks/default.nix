{ stdenv, gambit, openssl }:

stdenv.mkDerivation {
  name = "taskwarrior-hooks";
  src = ./.;

  postPatch = ''
    make clean
  '';

  buildInputs = [ gambit openssl ];
  makeFlags = [ "prefix=${placeholder "out"}" ];
}
