{ lib, config, ... }:

with lib;
let
  manifests = config.services.k3s.manifests;
in {
  options = {
    services.k3s.manifests = mkOption {
      type = types.listOf types.package;
      default = [];
      description = ''
        Manifest packages to set up in /var/lib/rancher/k3s/server/manifests before boot.
      '';
    };
  };

  config = {
    nixpkgs.overlays = [
      (self: super: {
        k3s = super.symlinkJoin {
          name = "k3s";
          paths = [ super.k3s ];
          buildInputs = [ super.rsync ];
          postBuild = ''
            mv $out/bin/k3s $out/bin/.k3s-wrapped-with-manifests
            cat >$out/bin/k3s <<EOF
            #!/bin/bash
            ${concatMapStringsSep "\n" (path: "rsync -q '${path}'/* /var/lib/rancher/k3s/server/manifests/") manifests}
            exec $out/bin/.k3s-wrapped-with-manifests "\$@"
            EOF
            chmod +x $out/bin/k3s
            patchShebangs $out/bin/k3s
          '';
        };
      })
    ];
  };
}
