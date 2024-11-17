{ config, lib, ... }:

with lib;
{
  config = mkMerge [
    {
      networking.nameservers = config.local.networking.nameservers;
    }
    (mkIf config.local.networking.respite-wifi.enable {
      networking.domain = "eraserhead.net";
      wireless.enable = true;
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        nssmdns6 = true;
        publish = {
          enable = true;
          addresses = true;
          domain = true;
          hinfo = true;
          workstation = true;
        };
      };
    })
  ];
}
