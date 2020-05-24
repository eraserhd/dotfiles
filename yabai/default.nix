{ options, pkgs, ... }:

{
  config = (if (builtins.hasAttr "yabai" options.services)
  then {
    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      extraConfig = "exec ${pkgs.zsh}/bin/zsh -l -c 'yabai-config'";
    };
  }
  else {
  });
}
