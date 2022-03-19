{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/95594027-89fd-4f8c-966e-da035aa59b87";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/3624-3954";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/05c1d59b-9cfc-4767-b8e3-4d4f99859411"; }
    ];

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

  networking.useDHCP = false;

  networking = {
    hostName = "crunch";
    nameservers = [
      "2620:119:35::35"
      "2620:119:53::53"
      "208.67.222.222"
      "208.67.220.220"
    ];
    firewall.enable = false;
    wireless = {
      enable = true;
      interfaces = [ "wlp65s0" ];
    };
    interfaces.wlp65s0 = {
      useDHCP = true;
      ipv6.addresses = [
        {
          address = "2600:1700:ad40:f7e0::42";
          prefixLength = 64;
        }
      ];
    };
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    k3s
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
    publish.enable = true;
    publish.addresses = true;
    publish.hinfo = true;
    publish.workstation = true;
  };

  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      min protocol = SMB2
      ea support = yes
      vfs objects = fruit streams_xattr
      fruit:metadata = stream
      fruit:model = MacSambda
      fruit:veto_appledouble = no
      fruit:posix_rename = yes
      fruit:zero_file_id = yes
      fruit:wipe_intentionally_left_blank_rfork = yes
      fruit:delete_empty_adfiles = yes
      workgroup = WORKGROUP
      server string = crunch
      netbios name = crunch
      guest account = nobody
      map to guest = never
      log level = 1
    '';
    shares = {
      src = {
        path = "/home/jfelice/src";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "jfelice";
        "force group" = "users";
      };
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
  services.k3s = {
    enable = true;
    role = "server";
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

  security.sudo.extraConfig = ''
    jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  home-manager.verbose = true;

  system.stateVersion = "21.05";

  local.plan9.cpu.enable = true;
  #local.sendOutgoingMailWithSES.enable = true;
  #local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";
}
