{ config, pkgs, ... }:

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
  };
}
