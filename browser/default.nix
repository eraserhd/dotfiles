{ lib, pkgs, ... }:

with lib;
{
  options = {
    local.browserCommand = mkOption {
      description = "Command to open a URL in a browser";
      type = types.str;
      default = "open";
    };
  };
}
