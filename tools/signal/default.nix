{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.signal-desktop ];
  };
}
