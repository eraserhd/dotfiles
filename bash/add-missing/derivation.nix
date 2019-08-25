{ stdenv, bash, git, ... }:

stdenv.mkDerivation {
  pname = "add-missing";
  version = "2019.08.23";
  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    substitute add-missing.sh $out/bin/add-missing \
      --subst-var-by bash '${bash}' \
      --subst-var-by git '${git}'
    chmod a+x $out/bin/add-missing
  '';
}
