{ stdenv }:

stdenv.mkDerivation {
  name = "monitor-follows-mouse";
  src = ./.;
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "monitor-follows-mouse";
}
