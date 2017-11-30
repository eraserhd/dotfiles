{ stdenv, fetchurl, undmg }:
stdenv.mkDerivation rec {
  version = "6.13.1_455785";
  name = "evernote-${version}";
  src = fetchurl {
    url = "https://cdn1.evernote.com/mac-smd/public/Evernote_RELEASE_${version}.dmg";
    sha256 = "939331ec1071af7b6eb6a5f3b4ed60c1a8b1dd1d568462cb464d78fd3a28f66d";
  };
  buildInputs = [ undmg ];
  installPhase = ''
    mkdir -p "$out/Applications/Evernote.app"
    cp -aR . "$out/Applications/Evernote.app/"
  '';

  meta = {
    license = stdenv.lib.licenses.unfree;
    platforms = stdenv.lib.platforms.darwin;
  };
}
