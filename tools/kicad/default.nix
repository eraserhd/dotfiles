{ pkgs, lib, config, ... }:

with lib;
let
  cfg = config.local.kits.pcbs;
in {
  config = mkIf cfg.enable {
    #FIXME: The build seems to be non-deterministically broken
    # environment.systemPackages = [ pkgs.kicad ];
  };
}
