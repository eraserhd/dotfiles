{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.mutagen ];
  };
}
