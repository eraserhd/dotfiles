{ options, pkgs, ... }:

let
  yabai-config = pkgs.callPackage ./config {};
in {
  config = (if (builtins.hasAttr "yabai" options.services)
  then {
    environment.systemPackages = [ yabai-config ];
    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      extraConfig = "exec ${pkgs.zsh}/bin/zsh -l -c '${yabai-config}/bin/yabai-config --init'";
    };

    # Here we should enable 'Switch to Desktop 1 = ^1, Switch to Desktop 2 = ^2, ...' once
    # nix-darwin has support for symbolic hotkeys.
  }
  else {
  });
}
