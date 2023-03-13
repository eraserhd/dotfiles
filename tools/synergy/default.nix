{ options, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ synergy ];
  };
}
