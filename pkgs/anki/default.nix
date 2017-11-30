{ stdenv, fetchurl, undmg }:
stdenv.mkDerivation rec {
  version = "2.0.48";
  name = "anki-${version}";
  src = fetchurl {
    url = "https://apps.ankiweb.net/downloads/current/anki-${version}.dmg";
    sha256 = "5593a8dbd8059bd1fcd1c68d68b9c14dd74a67ae368cfe3925207c17a857c6e1";
  };
  buildInputs = [ undmg ];
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    undmg < $src
    mkdir -p $out/Applications
    mv Anki.app $out/Applications/
  '';

  meta = {
    license = stdenv.lib.licenses.unfree;
    platforms = stdenv.lib.platforms.darwin;
  };
}
