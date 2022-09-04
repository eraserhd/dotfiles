{ pkgs, ... }:


let
  nats-manifest = pkgs.writeText "test.yaml" ''
    apiVersion: helm.cattle.io/v1
    kind: HelmChart
    metadata:
      name: nats
      namespace: kube-system
    spec:
      repo: https://nats-io.github.io/k8s/helm/charts/
      chart: nats
      targetNamespace: default
      valuesContent: |-
        nats:
          image: nats:2.8.4-alpine
          jetstream:
            enabled: true
          #externalAccess: true
        memStorage:
          enabled: true
          size: 128Mi
        fileStorage:
          enabled: true
          size: 128Mi
          storageDiretory: /data/
  '';
in {
  config = {
    environment.systemPackages = with pkgs; [
      natscli
    ];

    services.k3s.manifests = [ nats-manifest ];
  };
}
