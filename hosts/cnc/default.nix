{ config, lib, pkgs, modulePath, ... }:

with lib;
{
  # Raspberry Pi 3B, 1Gig RAM
  raspberry-pi-nix.board = "bcm2711";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "usb_storage" "usbhid" ];
    tmp.cleanOnBoot = true;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
  };

  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [
    pkgs.linux-firmware
  ];
  hardware.graphics.enable = true;

  #hardware.raspberry-pi.config = {
  #  all.base-dt-params = {
  #    # Not even sure this is in the right place
  #    force_turbo = {
  #      value = 1;
  #      enable = true;
  #    };
  #  };
  #};

  networking = {
    hostName = "cnc";
    firewall.enable = false;
    wireless = {
      interfaces = [ "wlan0" ];
    };
  };

  environment.systemPackages = with pkgs; [
    pinentry
    libraspberrypi
    raspberrypi-eeprom
  ];

  programs.gnupg.agent = {
    enable = true;
    enableExtraSocket = false;
  };

  services.openssh.enable = true;

  local.services.X11.enable = true;
  services.xserver = {
    displayManager.autoLogin = {
      enable = true;
      user = "jfelice";
    };
  };

  programs.ssh.startAgent = true;

  nixpkgs.config.allowUnfree = true;

  users.mutableUsers = false;
  users.users = {
    jfelice = {
      uid = 904137886;
      group = "twou";
      hashedPassword = "$6$ivLP1KZ08UiUOZT9$xVmR1e.Gw5NEmFWdgIcTLFmybGrh71Vt01I/bIpFajZuX7j7M5C0EURIiEQQJyt4ORM9DPrUsIXfbOU/SEFF4/";
      isNormalUser = true;
      home = "/home/jfelice";
      extraGroups = [ "docker" "wheel" "dialout" ];
      openssh.authorizedKeys.keys = config.local.authorizedKeys.jfelice;
    };
    alex = {
      isNormalUser = true;
      home = "/home/alex";
      extraGroups = [ "docker" "wheel" "dialout" ];
      openssh.authorizedKeys.keys = config.local.authorizedKeys.alex;
    };
    root.openssh.authorizedKeys.keys = config.local.authorizedKeys.jfelice;
  };
  users.groups.twou.gid = 151928526;

  security.sudo.extraConfig = ''
    jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
    alex     ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  home-manager.verbose = true;
  home-manager.users.jfelice.home.stateVersion = "22.05";

  system.stateVersion = "25.05";

  local.networking.respite-wifi.enable = true;
  local.sendOutgoingMailWithSES.enable = true;

  local.kits.workstation.enable = true;
  local.kits.pcbs.enable = false;
  local.kits.cnc.enable = true;
}
