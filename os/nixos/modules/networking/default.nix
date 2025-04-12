{ config, lib, ... }:

with lib;
{
  config = mkIf config.local.networking.respite-wifi.enable {
      networking.domain = "eraserhead.net";
      networking.wireless.enable = true;
  };
}
