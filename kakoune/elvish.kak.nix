{ stdenv, fetchgit }:
stdenv.mkDerivation {
  name = "elvish.kak";
  version = "2020-02-25";
  src = fetchgit {
    url = "https://gitlab.com/notramo/elvish.kak.git";
    rev = "769ce05013dbe1df036fd86dfa254a341636f0af";
    sha256 = "0nnnbg3y058f953vfanrxpprqrmwnskqzwr88m7i9ii7ffiac54k";
  };

  installPhase = ''
    mkdir -p $out/share/kak/autoload/plugins
    cp -r elvish.kak $out/share/kak/autoload/plugins
  '';

  meta = with stdenv.lib; {
    description = "Kakoune language file for Elvish scripts";
    homepage = "https://gitlab.com/notramo/elvish.kak";
    license = licenses.mit;
    maintainers = with maintainers; [ eraserhd ];
    platform = platforms.all;
  };
}

