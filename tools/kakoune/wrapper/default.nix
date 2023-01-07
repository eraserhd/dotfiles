{ bash, kakoune, stdenv }:

stdenv.mkDerivation {
  name = "kakouneWrapper";
  buildInputs = [ bash kakoune ];
  src = ./wrapper.sh;

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src $out/bin/kak
    substituteInPlace $out/bin/kak \
        --subst-var-by bash "${bash}" \
        --subst-var-by kakoune "${kakoune}"
  '';
}
