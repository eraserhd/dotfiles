{ stdenv, gerbil, gambit, libyaml, zlib }:

stdenv.mkDerivation {
  name = "yabai-config";
  src = ./.;

  buildInputs = [ gerbil gambit libyaml zlib ];

  installPhase = ''
    mkdir -p $out/bin
    GERBIL_PATH=$out gxi build.ss
  '';

  meta = with stdenv.lib; {
    platforms = platforms.all;
    maintainers = [ maintainers.eraserhd ];
  };
}
