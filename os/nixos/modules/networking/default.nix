{ config, lib, ... }:

with lib;
{
  config = mkIf config.local.networking.respite.enable {
      networking.domain = "eraserhead.net";
  };
}
