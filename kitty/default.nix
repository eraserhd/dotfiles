{ lib, pkgs, ... }:

with lib;
{
  config = mkIf (!pkgs.stdenv.isDarwin) {

    # Need the terminal type
    environment.systemPackages = [ pkgs.kitty ];
  };
}
