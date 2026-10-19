{ lib
, stdenv
, libX11
}:

stdenv.mkDerivation {
  name = "sigils";
  src = ./.;
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ libX11 ];
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "sigils";
}
