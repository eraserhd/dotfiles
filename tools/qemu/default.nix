{ pkgs, ... }:

{
  config = {
    virtualisation.libvirtd.enable = true;
    programs.virt-manager.enable = true;
    users.extraGroups.libvirt.members = [ "jfelice" ];
  };
}
