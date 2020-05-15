{ pkgs, ... }:

{
  config = {
    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      extraConfig = "exec ${pkgs.zsh}/bin/zsh -l -c 'yabai-config'";
    };
  };
}
