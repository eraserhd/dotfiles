{ pkgs, ... }:
{
  config = {
    environment.systemPackages = with pkgs; [ openscad ];
  };
}