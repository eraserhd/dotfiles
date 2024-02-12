{ config, lib, ... }:

with lib;
let
  cfg = config.local.kits.brain;
in {
  options = {
    local.kits.brain.enable = mkEnableOption "brain";
  };
}
