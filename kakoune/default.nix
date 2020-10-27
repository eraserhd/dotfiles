{ config, options, pkgs, lib, ... }:

with lib;
let
  my-kak-plumb = pkgs.kakounePlugins.kak-plumb.override {
    plan9port = pkgs.plan9port-wrapper;
  };
in {
  config = mkMerge [
    {
      nixpkgs.overlays = [
        (self: super: {
          kakoune = super.kakoune.override {
            plugins = with pkgs.kakounePlugins; [
              case-kak
              kak-ansi
              kak-fzf
              my-kak-plumb
              parinfer-rust
              quickscope-kak
              rep
            ];
          };
          kakouneWrapper = super.callPackage ./wrapper {};
        })
      ];

      environment.systemPackages = with pkgs; [
        pythonPackages.editorconfig
        kakouneWrapper
      ];

      environment.variables.EDITOR = "${pkgs.kakouneWrapper}/bin/kak";

      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".config/kak/kakrc".source = ./kakrc;
      };
    }
    (mkIf config.local.plan9.terminal.enable
     (if (builtins.hasAttr "systemd" options)
      then {
        systemd.user.services.plumber-kakoune-client = {
          description = "Open Kakoune for 'edit' plumbs";
          wantedBy = [ "default.target" ];
          unitConfig.ConditionUser = "!@system";
          serviceConfig = {
            Restart = "always";
            RestartSec = 35;
          };
          script = ''
            exec ${my-kak-plumb}/bin/edit-client
          '';
        };
      }
      else {
        launchd.agents.plumber-kakoune-client = {
          script = ''
            export XDG_RUNTIME_DIR="${config.environment.variables.XDG_RUNTIME_DIR}"
            exec ${my-kak-plumb}/bin/edit-client
          '';
          serviceConfig = {
            RunAtLoad = true;
            KeepAlive = true;
          };
        };
      })
    )
  ];
}
