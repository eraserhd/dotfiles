{ pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      nixpkgs.config.allowUnfree = true;
      environment.systemPackages = with pkgs; [ _1password ];
    }
    (mkIf pkgs.stdenv.isDarwin {
      old-homebrew.casks = [ "1password" ];
    })
  ];
}
