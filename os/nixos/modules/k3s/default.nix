{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.k3s;
in {
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ k3s kubectl ];
    services.k3s = {
      role = "server";
    };
  };
}
