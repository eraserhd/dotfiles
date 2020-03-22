{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.programs.broot;
in {
  options = {
    programs.broot.enable = mkEnableOption "broot";
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.broot ];
    environment.interactiveShellInit = ''
      if [ -n "$ZSH_VERSION" ]; then
        eval "$(${pkgs.broot}/bin/broot --print-shell-function zsh)"
      elif [ -n "$BASH_VERSION" ]; then
        eval "$(${pkgs.broot}/bin/broot --print-shell-function bash)"
      fi
    '';
  };
}
