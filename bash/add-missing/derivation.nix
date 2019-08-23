{ stdenv, bash, ... }:

stdenv.mkDerivation {
  pname = "add-missing";
  version = "2019.08.23";
  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    substitute add-missing.sh $out/bin/add-missing \
      --subst-var-by bash '${bash}'
    chmod a+x $out/bin/add-missing
  '';
}
