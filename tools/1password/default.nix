{ config, pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
      environment.systemPackages = with pkgs; [
        _1password-cli
        _1password-gui
      ];
    }
  ];
}
