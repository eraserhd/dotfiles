{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ chromium ];
  };
}
