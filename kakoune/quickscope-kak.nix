{ stdenv, fetchurl, python3 }:

stdenv.mkDerivation rec {
  pname = "quickscope-kak";
  version = "9bc5d55a541373aa2a";

  src = fetchurl {
    name = "quickscope.kak";
    url = "https://nest.pijul.com/voroskoi/quickscope.kak:master/9bc5d55a541373aa2a?raw";
    sha256 = "1rwh9f2mmna62ismw05ala3za8ilhksbahmv62v05w2gcpx4bsms";
  };

  pysrc = fetchurl {
    name = "quickscope.py";
    url = "https://nest.pijul.com/voroskoi/quickscope.kak:master/9bc5d55a541373aa4c?raw";
    sha256 = "1qkwimydm0d8jh90zi9vd4v0ii0shl7nwivz4ziswzyln2503icy";
  };

  unpackPhase = ":";
  buildInputs = [ python3 ];

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins/
    cp $src $out/share/kak/autoload/plugins/quickscope.kak
    cp $pysrc $out/share/kak/autoload/plugins/quickscope.py
    substituteInPlace $out/share/kak/autoload/plugins/quickscope.kak \
      --replace 'python3' '${python3}/bin/python3'
  '';
}
