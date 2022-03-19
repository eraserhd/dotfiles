{ config, lib, pkgs, ... }:

with lib;
let
  networkParams = importJSON ./ip.json;
in {
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

  networking = {
    domain = "eraserhead.net";
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
      proxyARP = true;
      ipv6.addresses = [
        {
          address = networkParams.ip;
          prefixLength = 64;
        }
      ];
    };
    interfaces.enp6s0 = {
      useDHCP = false;
      proxyARP = true;
      ipv4.addresses = [
        {
          address = "192.168.1.61";
          prefixLength = 30;
        }
      ];
      ipv6.addresses = [
        {
          address = "2600:1700:ad40:f7e8::42";
          prefixLength = 112;
        }
      ];
    };
  };

  boot.kernel.sysctl = {
    "net.ipv4.conf.all.forwarding" = true;
    "net.ipv6.conf.all.forwarding" = true;

    "net.ipv6.conf.all.accept_ra" = 0;
    "net.ipv6.conf.all.autoconf" = 0;
    "net.ipv6.conf.all.use_tempaddr" = 0;

    "net.ipv6.conf.wlp65s0.accept_ra" = 2;
    "net.ipv6.conf.wlp65s0.autoconf" = 1;
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
      server string = ${config.networking.hostName}
      netbios name = ${config.networking.hostName}
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

  services.dhcpd4 = {
    enable = true;
    authoritative = true;
    interfaces = [ "enp6s0" ];
    extraConfig = ''
      subnet 192.168.1.60 netmask 255.255.255.252 {
        range 192.168.1.62 192.168.1.62;
        option subnet-mask 255.255.255.252;
        option broadcast-address 192.168.1.63;
        option routers 192.168.1.61;
        option domain-name-servers 208.67.222.222, 208.67.220.220;
        option domain-name "${config.networking.domain}";
      }
    '';
  };
  services.radvd = {
    enable = true;
    config = ''
      interface enp6s0 {
        AdvSendAdvert on;
        prefix 2600:1700:ad40:f7e8::/64 { };
      };
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
  local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";
}
