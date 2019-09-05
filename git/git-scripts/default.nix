{ stdenv, bash, git }:

stdenv.mkDerivation {
  name = "local-git-scripts-2019.09.05";
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin
    cp git-cleanup git-fork git-l $out/bin/
    substituteInPlace $out/bin/git-l \
      --subst-var-by bash '${bash}' \
      --subst-var-by git '${git}'
    substituteInPlace $out/bin/git-cleanup \
      --subst-var-by bash '${bash}' \
      --subst-var-by git '${git}'
  '';
}
