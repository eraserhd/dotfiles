{ pkgs, ... }:

{
  config = {
    environment.systemPackages = [
      pkgs.xorg.xev
    ];
  };
}
