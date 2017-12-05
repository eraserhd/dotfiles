{ stdenv, fetchurl, cpio, xar }:
stdenv.mkDerivation rec {
  version = "20170524";
  name = "mactex-${version}";
  src = fetchurl {
    url = "http://mirror.ctan.org/systems/mac/mactex/mactex-${version}.pkg";
    sha256 = "0caf76027c9e0534a0b636f2b880ace4a0463105a7ad5774ccacede761be8c2d";
  };
  buildInputs = [ cpio xar ];
  builder = builtins.toFile "builder.sh" ''
    source $stdenv/setup
    xar -xf $src
    mkdir -p $out/ $out/Applications/
    gzip -dc root.pkg/Payload |cpio -i
    mv usr/local/texlive/2017/* $out/
    gzip -dc local.pkg/Payload |( cd $out/ && cpio -i )
    gzip -dc applications.pkg/Payload |( cd $out/Applications/ && cpio -i )
    ( cd $out/bin/; for p in x86_64-darwin/*; do ln -sf ./$p ./; done )
  '';

  meta = {
    license = stdenv.lib.licenses.unfree;
    platforms = stdenv.lib.platforms.darwin;
  };
}

