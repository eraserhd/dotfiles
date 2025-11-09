{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [
      distrobox
    ];
  };
}
