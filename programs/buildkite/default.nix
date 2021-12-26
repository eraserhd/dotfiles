{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.local.buildkite;
in {
  options = {
    local.buildkite.enable = mkEnableOption "buildkite";
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      buildkite-agent
      buildkite-cli
    ];
  };
}
