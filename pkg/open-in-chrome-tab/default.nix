{ stdenv, bash, reattach-to-user-namespace, ... }:

stdenv.mkDerivation {
  name = "open-in-chrome-tab";
  src = ./.;

  installPhase = ''
    mkdir -p $out/bin
    cp ./open-in-chrome-tab.sh $out/bin/open-in-chrome-tab
    substituteInPlace $out/bin/open-in-chrome-tab \
      --subst-var-by reattach-to-user-namespace '${reattach-to-user-namespace}'
  '';
}
