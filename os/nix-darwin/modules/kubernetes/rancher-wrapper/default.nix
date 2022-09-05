{ stdenv }:

stdenv.mkDerivation {
  name = "rancher-wrapper";
  src = ./.;
  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    commands=(
      docker
      docker-buildx
      docker-compose
      docker-credential-ecr-login
      docker-credential-none
      docker-credential-osxkeychain
      helm
      kubectl
      kuberlr
      nerdctl
      rdctl
    )
    for command in "''${commands[@]}"; do
      (
        printf '#!/usr/bin/env bash\n'
        printf 'exec ~/.rd/bin/%s "$@"\n' "$command"
      )>"$out/bin/$command"
    done
    chmod +x $out/bin/*
    runHook postInstall
  '';
}
