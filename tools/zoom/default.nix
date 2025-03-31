{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.zoom-us
    ];
  };
}
