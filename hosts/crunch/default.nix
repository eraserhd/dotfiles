{ config, lib, pkgs, ... }:

with lib;
{
  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "thunderbolt" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  #boot.kernelPackages = pkgs.linuxPackages_6_16; # for NVIDIA

  # For building Raspberry Pi images
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/root";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-label/SYSTEM";
    fsType = "vfat";
  };
  swapDevices = [{
    device = "/.swapfile";
    size = 128 * 1024;
  }];

  hardware.enableRedistributableFirmware = true;
  # hardware.cpu.amd.updateMicrocode = true;
  hardware.enableAllFirmware = true;
  hardware.firmware = [
    pkgs.wireless-regdb
    pkgs.linux-firmware
  ];

  nix.settings.max-jobs = 2;
  nix.settings.cores = 10;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 6;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmp.cleanOnBoot = true;

  networking = {
    hostName = "crunch";
    firewall.enable = false;
    #defaultGateway = {
    #  address = "10.156.1.1";
    #  interface = "wlp5s0";
    #};
    interfaces.enp7s0 = {
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

  documentation.dev.enable = true;

  services.printing = {
    enable = true;
    drivers = with pkgs; [
      hplip
      epson-escpr
      epson-escpr2
    ];
  };
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  hardware.printers = {
    ensurePrinters = [
      {
        name = "Laserjet";
        model = "HP/hp-laserjet_200_color_m251-ps.ppd.gz";
        description = "HP Laserjet 200 M251nw (324AEC)";
        deviceUri = "dnssd://HP%20LaserJet%20200%20color%20M251nw%20(324AEC)._ipp._tcp.local/?uuid=434e4431-4837-3733-3536-fc15b4324aec";
      }
      {
        name = "Epson";
        model = "epson-inkjet-printer-escpr2/Epson-XP-15000_Series-epson-escpr2-en.ppd";
        description = "Epson XP-15000 Series";
        deviceUri = "dnssd://EPSON%20XP-15000%20Series._ipp._tcp.local/?uuid=cfe92100-67c4-11d4-a45f-dccd2fd241db";
      }
    ];
    ensureDefaultPrinter = "Laserjet";
  };

  programs.gnupg.agent = {
    enable = true;
    enableExtraSocket = false;
  #  enableSSHSupport = true;
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

  services.ollama = {
    enable = true;
  };

  virtualisation.docker.enable = true;

  # Allow OpenOCD to access Fiddy Plus
  services.udev.extraRules = ''
    ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6010", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
  '';

  local.bluetooth.enable = true;
  local.services.X11.enable = true;
  services.xserver = {
    videoDrivers = [ "nvidia" ];
    xrandrHeads = [
      {
        output = "HDMI-0";
        primary = true;
      }
      {
        output = "DP-0";
      }
    ];
    screenSection = ''
      Option         "nvidiaXineramaInfoOrder" "HDMI-0"
      Option         "metamodes" "HDMI-0: nvidia-auto-select +0+0, DP-0: nvidia-auto-select +3840+0"
    '';
  };
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [ nvidia-vaapi-driver ];
  };
  hardware.nvidia = {
    open = true;
    package = config.boot.kernelPackages.nvidiaPackages.latest;
  };
  services.xserver.displayManager.sessionCommands = ''
    xset s off
    xset -dpms
    xset s noblank
  '';

  security.rtkit.enable = true; # Recommended for pipewire??
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.extraConfig.bluetoothEnhancements = {
      services.pipewire.wireplumber.extraConfig.bluetoothEnhancements = {
        "monitor.bluez.properties" = {
          "bluez5.enable-sbc-xq" = true;
          "bluez5.enable-msbc" = true;
          "bluez5.enable-hw-volume" = true;
          "bluez5.roles" = [ "hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag" ];
          "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
        };
      };
    };
  };

  programs.ssh.startAgent = true;

  nixpkgs.config.allowUnfree = true;

  users.mutableUsers = false;
  users.users = {
    jfelice = {
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

  security.sudo.extraConfig = ''
    jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
  '';

  home-manager.verbose = true;

  system.stateVersion = "24.05";
  home-manager.users.jfelice.home.stateVersion = "22.05";

  local.networking.respite-wifi.enable = true;
  local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
  local.tmux.paneZeroCommand = "weechat";

  local.kits.brain.enable = true;
  local.kits.collaboration.enable = true;
  local.kits.develop.enable = true;
  local.kits.thinking.enable = true;
  local.kits.workstation.enable = true;
  local.kits._3d-printing.enable = true;
  local.kits.pcbs.enable = true;
  local.kits.cnc.enable = true;

  plugbench.plumber.enable = true;
}
