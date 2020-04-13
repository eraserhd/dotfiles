{ lib, pkgs, ... }:

{
  config = mkIf (!config.isDarwin) {

    # Need the terminal type
    environment.systemPackages = [ pkgs.kitty ];
  };
}
