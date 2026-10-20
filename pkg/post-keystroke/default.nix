{ stdenv }:

stdenv.mkDerivation {
  name = "post-keystroke";
  src = ./.;
  makeFlags = [ "PREFIX=$(out)" ];
  meta.mainProgram = "post-keystroke";
}
