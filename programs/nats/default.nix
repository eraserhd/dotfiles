{ pkgs, ... }:


let
  nats-manifest = pkgs.writeText "test.yaml" ''
    apiVersion: v1
    kind: ConfigMap
    metadata:
      name: a-test
    data:
      foo: "bar"
  '';
in {
  config = {
    environment.systemPackages = with pkgs; [
      natscli
    ];

    services.k3s.manifests = [ nats-manifest ];
  };
}
