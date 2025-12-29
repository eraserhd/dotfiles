{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [ pkgs.tomat ];
  };
}
