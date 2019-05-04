{ stdenv, fetchFromGitHub, ncurses, asciidoc, docbook_xsl, libxslt, pkgconfig }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "kakoune-${version}";
  version = "eraserhd";
  src = fetchFromGitHub {
    repo = "kakoune";
    owner = "mawww";
    rev = "92972bed4fb4ff6dffd32169bc437de34acac6a9";
    sha256 = "1cn32qyp0zki392200zxzp0mjikalrc92g1anav3xwplh1zlv1ks";
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
