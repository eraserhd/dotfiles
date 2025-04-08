{ pkgs, ... }:
{
  config = {
    environment.systemPackages = [
      pkgs.discord
    ];
  };
}
