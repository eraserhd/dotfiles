{ stdenv, bash, git, babashka, makeWrapper }:

stdenv.mkDerivation {
  name = "local-git-scripts-2019.09.05";
  src = ./.;
  nativeBuildInputs = [ babashka makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cp git-cleanup git-l git-browse-link $out/bin/
    substituteInPlace $out/bin/git-cleanup \
      --subst-var-by bash '${bash}' \
      --subst-var-by git '${git}'
    substituteInPlace $out/bin/git-l \
      --subst-var-by bash '${bash}' \
      --subst-var-by git '${git}'

    # Not a #! script: macOS cannot run one whose interpreter is a script, as
    # bin/bb is.
    install -D show-pr $out/share/show-pr/show-pr.bb
    makeWrapper ${babashka}/bin/bb $out/bin/show-pr --add-flags $out/share/show-pr/show-pr.bb
  '';
  doInstallCheck = true;
  installCheckPhase = ''
    bb show-pr-test.bb
  '';
}
