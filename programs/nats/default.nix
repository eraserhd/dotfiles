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
    local.plumber.enable = mkEnableOption "plumber";
    local.clipboard.enable = mkEnableOption "clipboard";
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        natscli
        nats-plumber
      ];

      services.k3s.manifests = [ manifest ];
    }

    (mkIf config.local.plumber.enable
     (if (builtins.hasAttr "launchd" options)
      then {
        launchd.user.agents.plumber = {
          script = ''
            ${pkgs.nats-plumber}/bin/plumber
          '';
          serviceConfig = {
            KeepAlive = true;
          };
        };
      }
      else {
        assertions = [{
          assertion = false;
          message = "local.plumber is not available on NixOS yet";
        }];
      }))

    (mkIf config.local.clipboard.enable
     (if (builtins.hasAttr "launchd" options)
      then {
        launchd.user.agents.clipboard = {
          script = ''
            ${pkgs.nats-plumber}/bin/clipboard
          '';
          serviceConfig = {
            KeepAlive = true;
          };
        };
      }
      else {
        assertions = [{
          assertion = false;
          message = "local.clipboard is not available on NixOS yet";
        }];
      }))
  ];
}
