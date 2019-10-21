{ config, options, pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      nixpkgs.overlays = [
        (self: super: {
          kakoune = super.wrapKakoune self.kakoune-unwrapped {
            configure = {
              plugins = with self.kakounePlugins; [
                kak-ansi
                kak-plumb
                parinfer-rust
              ];
            };
          };
          kakouneWrapper = super.callPackage ./wrapper {};
        })
      ];

      environment.systemPackages = [ pkgs.kakouneWrapper ];

      environment.variables.EDITOR = "${pkgs.kakouneWrapper}/bin/kak";
      environment.variables.kak_opt_kakfs = "$XDG_RUNTIME_DIR/kakfs/kakoune";

      home-manager.users.jfelice = { pkgs, ... }: {
        home.file.".config/kak/kakrc".source = ./kakrc;
      };
    }
    (mkIf config.local.plan9.cpu.enable
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
            export NAMESPACE="$XDG_RUNTIME_DIR/plan9/srv"
            exec ${pkgs.kakounePlugins.kak-plumb}/bin/edit-client
          '';
        };
      }
      else {
      })
    )
  ];
}
