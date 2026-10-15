{ lib
, stdenv
, libX11
}:

stdenv.mkDerivation {
  name = "sigil";
  src = ./.;
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ libX11 ];
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "sigil";
}
