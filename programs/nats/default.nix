{ pkgs, lib, options, config, ... }:

with lib;
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
  options = {
    plugbench.clipboard.enable = mkEnableOption "clipboard";
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        natscli
        plumber-pluggo
      ];

      services.k3s.manifests = [ manifest ];
    }

    (mkIf config.plugbench.clipboard.enable
     (if (builtins.hasAttr "launchd" options)
      then {
        launchd.user.agents.clipboard = {
          script = ''
            ${pkgs.nats-clipboard}/bin/clipboard
          '';
          serviceConfig = {
            KeepAlive = true;
          };
        };
      }
      else {
        assertions = [{
          assertion = false;
          message = "plugbench.clipboard is not available on NixOS yet";
        }];
      }))

  ];
}
