{ config, lib, pkgs, ... }:

with lib;
{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/95594027-89fd-4f8c-966e-da035aa59b87";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/3624-3954";
    fsType = "vfat";
  };
  swapDevices = [{
    device = "/dev/disk/by-uuid/05c1d59b-9cfc-4767-b8e3-4d4f99859411";
  }];

  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];
  hardware.graphics.enable = true;

  nix.settings.max-jobs = 2;
  nix.settings.cores = 10;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;

  local.systemDisplayName = "crunch";

  networking = {
    domain = "eraserhead.net";
    hostName = "crunch";
    firewall.enable = false;
    wireless = {
      enable = true;
      interfaces = [ "wlp5s0" ];
    };
    #defaultGateway = {
    #  address = "10.156.1.1";
    #  interface = "wlp5s0";
    #};
    interfaces.wlp5s0 = {
      useDHCP = true;
      #ipv6.addresses = [
      #  {
      #    address = networkParams.ip;
      #    prefixLength = 64;
      #  }
      #];
      #ipv4.addresses = [
      #  {
      #    address = "10.156.1.42";
      #    prefixLength = 24;
      #  }
      #];
    };
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    pinentry
  ];

  documentation.dev.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableExtraSocket = false;
  #  enableSSHSupport = true;
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      workstation = true;
    };
  };

  services.openssh = {
    enable = true;
    ports = [ 22 443 ];
    extraConfig = ''
      ClientAliveCountMax 3
      ClientAliveInterval 10
      StreamLocalBindUnlink yes
    '';
  };

  virtualisation.docker.enable = true;

  # Allow OpenOCD to access Fiddy Plus
  services.udev.extraRules = ''
    ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
  '';

  local.bluetooth.enable = true;
  local.services.X11.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.displayManager.sessionCommands = ''
    barriers --log /tmp/barrier.log --no-tray --debug INFO --name crunch --disable-crypto --disable-client-cert-checking -c ${../../tools/barrier/barrier.conf} --address :24800
    xset s off
    xset -dpms
    xset s noblank
  '';

  programs.ssh.startAgent = true;

  nix.nixPath = [
    "nixos-config=/home/jfelice/src/dotfiles/machines/crunch/default.nix"
  ];

  nixpkgs.config.allowUnfree = true;

  users.mutableUsers = false;
  users.users = {
    jfelice = {
      uid = 904137886;
      group = "twou";
      hashedPassword = "$6$ivLP1KZ08UiUOZT9$xVmR1e.Gw5NEmFWdgIcTLFmybGrh71Vt01I/bIpFajZuX7j7M5C0EURIiEQQJyt4ORM9DPrUsIXfbOU/SEFF4/";
      isNormalUser = true;
      home = "/home/jfelice";
      extraGroups = [ "docker" "wheel" ];
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
  '';

  home-manager.verbose = true;

  system.stateVersion = "21.05";
  home-manager.users.jfelice.home.stateVersion = "22.05";

  local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";

  local.kits.brain.enable = true;
  local.kits.workstation.enable = true;
  local.kits._3d-printing.enable = true;
  local.kits.pcbs.enable = true;
}
