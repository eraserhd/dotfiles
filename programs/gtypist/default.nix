{ config, pkgs, ... }:

{
  config = {
    environment.systemPackages = with pkgs; [ gtypist ];
  };
}
