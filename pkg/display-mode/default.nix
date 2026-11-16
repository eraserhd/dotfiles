{ stdenv }:

stdenv.mkDerivation {
  name = "display-mode";
  src = ./.;
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "display-mode";
}
