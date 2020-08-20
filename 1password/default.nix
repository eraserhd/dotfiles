{ pkgs, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [ _1password ];
    }
    (mkIf pkgs.stdenv.isDarwin {
      homebrew.casks = [ "1password" ];
    })
  ];
}
