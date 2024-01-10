{ stdenv
, bash
, kakoune
}:

stdenv.mkDerivation {
  name = "kak-scrollback-pager";
  src = ./.;
  installPhase = ''
    mkdir -p $out/bin
    substitute kak-scrollback-pager.bash $out/bin/kak-scrollback-pager \
      --subst-var-by bash '${bash}' \
      --subst-var-by kakoune '${kakoune}'
  '';
}
