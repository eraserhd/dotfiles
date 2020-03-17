{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs._1password ];
  };
}
