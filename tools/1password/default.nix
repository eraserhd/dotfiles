{ config, pkgs, lib, ... }:

with lib;
{
  config = {
    nixpkgs.config.allowUnfree = true;
    programs._1password.enable = true;
    # Don't cross-compile electron apps.
    programs._1password-gui.enable = pkgs.stdenv.buildPlatform.system == pkgs.stdenv.hostPlatform.system;
  };
}
