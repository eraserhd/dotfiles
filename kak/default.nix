{ config, pkgs, lib, ... }:

{
  config = {
    nixpkgs.overlays = [
      (self: super: {
        kakoune-unwrapped = super.kakoune-unwrapped.overrideAttrs (oldAttrs: {
          src = super.pkgs.fetchFromGitHub (builtins.fromJSON (builtins.readFile ./version.json));
        });
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

    systemd.user.services.plumber-kakoune-client = lib.mkIf (config.local.plan9.cpu.enable) {
      description = "Open Kakoune for 'edit' plumbs";
      wantedBy = [ "default.target" ];
      unitConfig.ConditionUser = "!@system";
      path = with pkgs; [ kakoune-unwrapped plan9port ];
      serviceConfig = {
        Restart = "always";
        RestartSec = 35;
      };
      script = ''
        set -e
        export NAMESPACE="$XDG_RUNTIME_DIR/plan9/srv"
        9 9p read plumb/edit |while true; do
          read src
          read dst
          read wdir
          read type
          read attr
          read ndata
          read -N $ndata data

          printf '
            evaluate-commands -try-client %%opt{jumpclient} %%{
              edit -existing "%s"
            }
          ' "$data" |kak -p kakoune
        done
      '';
    };
  };
}
