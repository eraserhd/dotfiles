{ config, lib, pkgs, ... }:

with lib;
{
  options = {
    local.systemDisplayName = mkOption {
      type = types.str;
      description = ''
        System name to display in prompts (can be different from hostname).
      '';
    };
  };
}
