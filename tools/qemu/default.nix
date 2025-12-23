{ pkgs, lib, config, ... }:

with lib;
{
  config = mkIf config.local.kits.develop.enable {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.extraGroups.libvirt.members = [ "jfelice" ];
  };
}
