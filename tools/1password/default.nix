{ config, pkgs, lib, ... }:

with lib;
{
  config = {
    nixpkgs.config.allowUnfree = true;
    programs._1password.enable = true;
    programs._1password-gui.enable = true;
  };
}
