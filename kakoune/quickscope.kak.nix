{ stdenv, fetchgit, lua5_3 }:

stdenv.mkDerivation rec {
  pname = "quickscope-kak";
  version = "unstable-2020-11-16";

  src = fetchgit {
    url = "https://git.sr.ht/~voroskoi/quickscope.kak";
    rev = "ee4784147a0389766b87e07f58443a1bdf76e713";
    sha256 = "0y1g3zpa2ql8l9rl5i2w84bka8a09kig9nq9zdchaff5pw660mcx";
  };

  buildInputs = [ lua5_3 ];

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins/
    cp quickscope.* $out/share/kak/autoload/plugins/
    # substituteInPlace does not like the pipe
    sed -e 's,[|] *lua,|${lua5_3}/bin/lua,' quickscope.kak >$out/share/kak/autoload/plugins/quickscope.kak
  '';
}
