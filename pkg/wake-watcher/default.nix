{ stdenv }:

stdenv.mkDerivation {
  name = "wake-watcher";
  src = ./.;
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "wake-watcher";
}
