{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ tightvnc ];
  };
}
