{ pkgs, lib, config, options, ... }:

with lib;
{
  config = mkIf config.local.kits.develop.enable
    (if (builtins.hasAttr "launchd" options)
     then {}
     else {
       virtualisation.libvirtd.enable = true;
       programs.virt-manager.enable = true;
       users.extraGroups.libvirt.members = [ "jfelice" ];
     });
}
