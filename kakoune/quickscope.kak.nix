{ stdenv, fetchgit, lua5_3 }:

stdenv.mkDerivation rec {
  pname = "quickscope-kak";
  version = "unstable-2020-11-16";

  src = fetchgit {
    url = "https://git.sr.ht/~eraserhd/quickscope.kak";
    rev = "28898f971d7534123305425bb1133d6560485f70";
    sha256 = "0y1g3zpa2ql8l9rl5i2w84bka8a09kig9nq9zdchaff5pw660mcx";
  };

  buildInputs = [ lua5_3 ];

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins/
    cp quickscope.* $out/share/kak/autoload/plugins/
    # substituteInPlace does not like the pipe
    sed -e 's,[|] *lua,|${lua5_3}/bin/lua,' -e 's,2>&1,,' quickscope.kak >$out/share/kak/autoload/plugins/quickscope.kak
  '';
}
