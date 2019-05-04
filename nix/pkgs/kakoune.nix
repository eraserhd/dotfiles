{ stdenv, fetchFromGitHub, ncurses, asciidoc, docbook_xsl, libxslt, pkgconfig }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "kakoune-${version}";
  version = "eraserhd";
  src = fetchFromGitHub {
    repo = "kakoune";
    owner = "eraserhd";
    rev = "eraserhd";
    sha256 = "1skbp65272nx9llh2m9bc1hgwvp5wf4cwqszawp4m06a3dcgb4gm";
  };
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ ncurses asciidoc docbook_xsl libxslt ];
  makeFlags = [ "debug=no" ];

  postPatch = ''
    export PREFIX=$out
    cd src
    sed -ie 's#--no-xmllint#--no-xmllint --xsltproc-opts="--nonet"#g' Makefile
  '';

  preConfigure = ''
    export version="v${version}"
  '';

  meta = {
    homepage = http://kakoune.org/;
    description = "A vim inspired text editor (eraserhd's fork)";
    license = licenses.publicDomain;
    maintainers = with maintainers; [ vrthra ];
    platforms = platforms.unix;
  };
}
