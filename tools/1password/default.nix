{ config, pkgs, lib, ... }:

with lib;
let
  enable = !config.local.no1password;
in {
  options = {
    local.no1password = mkEnableOption "No 1Password";
  };

  config = mkIf enable {
    nixpkgs.config.allowUnfree = true;
    programs._1password.enable = true;
    # Don't cross-compile electron apps.
    programs._1password-gui.enable = pkgs.stdenv.buildPlatform.system == pkgs.stdenv.hostPlatform.system;
  };
}
