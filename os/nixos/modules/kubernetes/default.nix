{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.kubernetes;
in {
  options = {
    local.kubernetes.enable = mkEnableOption "kubernetes";
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ k3s ];
    services.k3s = {
      enable = true;
      role = "server";
    };
  };
}
