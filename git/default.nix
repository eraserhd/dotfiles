{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      gitFull
      gitAndTools.hub
    ];
  };
}
