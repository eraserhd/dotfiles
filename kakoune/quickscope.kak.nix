{ stdenv, fetchgit, lua5_3 }:

stdenv.mkDerivation rec {
  pname = "quickscope-kak";
  version = "9bc5d55a541373aa2a";

  src = fetchgit {
    url = "https://git.sr.ht/~voroskoi/quickscope.kak";
    rev = "30e7b51d32e1683d79312060614af8ce18eb8e08";
    sha256 = "1k92g1aqj9lh1vdljadkcdjzbq3vxzxiqb80lny99bb5h55v6ghm";
  };

  buildInputs = [ lua5_3 ];

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins/
    cp quickscope.* $out/share/kak/autoload/plugins/
    # substituteInPlace does not like the pipe
    sed -e 's,[|] *lua,|${lua5_3}/bin/lua,' -e 's,2>&1,,' quickscope.kak >$out/share/kak/autoload/plugins/quickscope.kak
  '';
}
