{ config, lib, ... }:

with lib;
let
  cfg = config.local.plan9.terminal;
in {
  config = mkIf cfg.enable {
  };
}
