{ pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ plan9port ];
  };
}
