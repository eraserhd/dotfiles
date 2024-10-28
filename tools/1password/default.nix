{ config, pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
      environment.systemPackages = with pkgs; [ _1password-cli ];
    }
    (mkIf config.local.kits.workstation.enable {
      homebrew.casks = mkIf pkgs.stdenv.isDarwin [ "1password" ];
      environment.systemPackages = mkIf (!pkgs.stdenv.isDarwin) [ pkgs._1password-gui ];
    })
  ];
}
