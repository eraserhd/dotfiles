{ config, pkgs, ... }:

{
  imports = [ ../../common.nix ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/5fcc6180-13bf-4dc9-a8a0-fabf45c50a03";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/A464-04ED";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/54fc28ec-2594-49a5-9de7-fbb6bab34070"; }
    ];

  nix.maxJobs = 1;
  nix.buildCores = 20;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  local.systemDisplayName = "crunch";

  networking = {
      hostName = "crunch";
      defaultGateway = {
        address = "10.0.0.1";
        interface = "wlp65s0";
      };
      interfaces.wlp65s0 = {
        useDHCP = false;
        ipv4 = {
          addresses = [ {
            address = "10.0.0.2";
            prefixLength = 24;
          } ];
          routes = [
            { address = "10.0.0.0"; prefixLength = 24; }
          ];
        };
      };
      nameservers = [ "8.8.8.8" "8.8.4.4" ];
      firewall.enable = true;
      wireless.enable = true;
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    glib
  ];

  documentation.dev.enable = true;

  services.openssh = {
    enable = true;
    ports = [ 22 443 ];
    extraConfig = ''
      ClientAliveCountMax 3
      ClientAliveInterval 10
      StreamLocalBindUnlink yes
    '';
  };

  local.bluetooth.enable = true;
  local.services.X11.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  nix.nixPath = [
    "nixpkgs=/home/jfelice/src/dotfiles/nixpkgs"
    "nixos-config=/home/jfelice/src/dotfiles/machines/crunch/default.nix"
  ];

  nixpkgs.config.allowUnfree = true;

  users.users = {
    jfelice = {
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

  security.sudo.extraConfig = ''
    jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  home-manager.verbose = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03";

  local.plan9.cpu.enable = true;
  #local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";
}
