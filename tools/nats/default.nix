{ pkgs, ... }:

let
  manifest = pkgs.stdenv.mkDerivation {
    name = "nats-k8s-manifest";
    src = ./manifest;
    buildPhase = ":";
    installPhase = ''
      mkdir $out
      cp *.yaml $out/
    '';
  };
in {
  config = {
    environment.systemPackages = with pkgs; [
      natscli
    ];

    services.k3s.manifests = [ manifest ];
  };
}
