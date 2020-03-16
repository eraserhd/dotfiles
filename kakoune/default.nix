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
                case-kak
                kak-ansi
                kak-plumb
                kak-proof
                parinfer-rust
              ]
              # Remove this hack when Graal is on MacOS
              ++ lib.optional (!pkgs.stdenv.isDarwin) rep;
            };
          };
          kakouneWrapper = super.callPackage ./wrapper {};
        })
      ];

      environment.systemPackages = with pkgs; [
        kakouneWrapper
        kakounePlugins.kak-proof # For testing
      ];

      environment.variables.EDITOR = "${pkgs.kakouneWrapper}/bin/kak";

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
            exec ${pkgs.kakounePlugins.kak-plumb}/bin/edit-client
          '';
        };
      }
      else {
      })
    )
  ];
}
