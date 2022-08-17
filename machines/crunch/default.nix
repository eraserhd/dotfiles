{ config, lib, pkgs, ... }:

with lib;
let
  networkParams = importJSON ./ip.json;
in {
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

  hardware.video.hidpi.enable = lib.mkDefault true;
  hardware.enableRedistributableFirmware = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [ pkgs.wireless-regdb ];

  nix.settings.max-jobs = 2;
  nix.settings.cores = 10;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  local.systemDisplayName = "crunch";

  networking = {
    domain = "eraserhead.net";
    hostName = "crunch";
    firewall.enable = false;
    wireless = {
      enable = true;
      interfaces = [ "wlp65s0" ];
    };
    defaultGateway = {
      address = "10.156.1.1";
      interface = "wlp65s0";
    };
    interfaces.wlp65s0 = {
      useDHCP = false;
      proxyARP = true;
      ipv6.addresses = [
        {
          address = networkParams.ip;
          prefixLength = 64;
        }
      ];
      ipv4.addresses = [
        {
          address = "10.156.1.42";
          prefixLength = 24;
        }
      ];
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
    pinentryFlavor = "tty";
  #  enableSSHSupport = true;
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
    reflector = true;
    publish.enable = true;
    publish.addresses = true;
    publish.hinfo = true;
    publish.workstation = true;
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

  local.kubernetes.enable = true;

  local.scheduledJobs.h_nexus = {
    period = "15min";
    path = [ pkgs.bash pkgs.git ];
    script = ''
      /home/jfelice/src/h_nexus/update.sh
    '';
  };

  #local.bluetooth.enable = true;
  #local.services.X11.enable = true;
  #services.xserver.videoDrivers = [ "nvidia" ];

  nix.nixPath = [
    "nixos-config=/home/jfelice/src/dotfiles/machines/crunch/default.nix"
  ];

  nixpkgs.config.allowUnfree = true;

  users.mutableUsers = false;
  users.users = {
    jfelice = {
      uid = 904137886;
      group = "twou";
      hashedPassword = "$6$Cxax2Zmac07km0Va$uxfyX2/DsdPPc6VAfF8tuNDafQ.FTlvGRc2h.1Xoh00HSUxVCl8nGkVpwDo9fysg3IHOxiYnKh.o/1qK8CWmy.";
      isNormalUser = true;
      home = "/home/jfelice";
      extraGroups = [ "docker" "wheel" ];
      openssh.authorizedKeys.keys = config.local.authorizedKeys.jfelice;
    };
    alex = {
      isNormalUser = true;
      home = "/home/alex";
      extraGroups = [ "docker" "wheel" ];
      openssh.authorizedKeys.keys = config.local.authorizedKeys.alex;
    };
    root.openssh.authorizedKeys.keys = config.local.authorizedKeys.jfelice;
  };
  users.groups.twou.gid = 151928526;

  security.sudo.extraConfig = ''
    jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  fileSystems."/home/jfelice/src" = {
    device = "/srv/exports/src";
    options = [ "bind" ];
  };

  home-manager.verbose = true;

  system.stateVersion = "21.05";
  home-manager.users.jfelice.home.stateVersion = "22.05";

  local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";
}
