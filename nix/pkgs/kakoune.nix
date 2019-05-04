{ stdenv, fetchFromGitHub, ncurses, asciidoc, docbook_xsl, libxslt, pkgconfig }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "kakoune-${version}";
  version = "eraserhd";
  src = fetchFromGitHub {
    repo = "kakoune";
    owner = "eraserhd";
    rev = "eraserhd";
    sha256 = "1szmzxb36k6ay5mhr2ipninmx6bxpskzzbdg2b49wciqqccx6gj0";
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
