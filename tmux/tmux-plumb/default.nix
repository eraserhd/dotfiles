{ stdenv, ... }:

stdenv.mkDerivation {
  pname = "tmux-plumb";
  version = "0.1.0";
  src = ./.;

  installPhase = ''
    target=$out/share/tmux-plugins/plumb/
    mkdir -p $target
    for file in plumb.tmux scripts; do
      cp -r $file $target
    done
  '';
}
