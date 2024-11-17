{ lib, ... }:

with lib;
{
  options = {
    local.networking.nameservers = mkOption {
      type = types.listOf types.str;
      default = [];
      example = [ "2620:119:53::53" "208.67.222.222" ];
      description = ''
        The list of preferred nameservers.
      '';
    };

    local.networking.respite-wifi.enable = mkEnableOption "Respite Wi-Fi";
  };
}
