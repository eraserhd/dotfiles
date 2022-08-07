{ config, options, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.kubernetes;
in {
  options = {
    local.kubernetes.enable = mkEnableOption "kubernetes";
  };
  config = mkIf cfg.enable
    (if (builtins.hasAttr "launchd" options)
     then {
       # Darwin
       environment.systemPackages = with pkgs; [ minikube ];
     }
     else {
       # NixOS
       environment.systemPackages = with pkgs; [ k3s ];
       services.k3s = {
         enable = true;
         role = "server";
       };
     });
}
