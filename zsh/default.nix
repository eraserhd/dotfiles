{ config, pkgs, ... }:

let
  shellPackage = config.local.loginShell.package;
in {
  config = {
    local.loginShell.package = pkgs.zsh;
    programs.zsh.enable = true;
    programs.zsh.enableCompletion = true;
  };
}
