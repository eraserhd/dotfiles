{ config, lib, pkgs, ... }:

with lib; {
  config = mkIf config.local.kits._3d-printing.enable {
    environment.systemPackages = with pkgs; [
      meshlab
    ];
  };
}
