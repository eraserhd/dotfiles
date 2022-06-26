{ config, lib, pkgs, modulesPath, ... }:

with lib;
{
  imports = [
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  config = {
    boot.initrd.availableKernelModules = [ "virtio_pci" "ahci" "sr_mod" "virtio_blk" "virtio_gpu" ];
    boot.initrd.kernelModules = [ ];
    boot.kernelModules = [ "kvm-amd" "kvm-intel" "kvm_amd" "kvm_intel" ];
    boot.extraModulePackages = [ ];

    # Needed for https://github.com/NixOS/nixpkgs/issues/58959
    boot.supportedFilesystems = lib.mkForce [ "brtfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];

    #fileSystems."/" = {
    #  device = "/dev/disk/by-label/nixos";
    #  fsType = "ext4";
    #};
    #fileSystems."/boot" = {
    #  device = "/dev/disk/by-label/boot";
    #  fsType = "vfat";
    #};
    fileSystems."/home/jfelice/src" = {
      device = "hostsrc";
      fsType = "9p";
      options = [
        "trans=virtio"
        "version=9p2000.L"
      ];
    };
    swapDevices = [{
      device = "/dev/disk/by-label/swap";
    }];

    hardware.enableRedistributableFirmware = true;
    hardware.enableAllFirmware = true;
    hardware.firmware = [ pkgs.wireless-regdb ];

    nix.settings.max-jobs = 2;
    nix.settings.cores = 2;

    #boot.loader.grub.enable = true;
    #boot.loader.grub.version = 2;
    boot.cleanTmpDir = true;

    local.systemDisplayName = config.networking.hostName;

    networking.useDHCP = lib.mkDefault true;
    networking = {
      domain = "eraserhead.net";
      hostName = "parasite";
      firewall.enable = false;
      interfaces.enp0s2 = {
        useDHCP = true;
      };
    };

    time.timeZone = "America/New_York";

    environment.systemPackages = with pkgs; [
      _9pfs
      pinentry

      # vncserver, etc.
      tigervnc
      xorg.xinit
    ];

    local.services.X11.enable = true;
    services.xserver.autorun = false;
    services.xserver.displayManager.startx.enable = true;

    documentation.dev.enable = true;

    programs.gnupg.agent = {
      enable = true;
      enableExtraSocket = false;
      pinentryFlavor = "tty";
    };

    virtualisation.docker.enable = true;

    nix.nixPath = [
      "nixos-config=/home/jfelice/src/dotfiles/machines/parasite/default.nix"
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
      root.openssh.authorizedKeys.keys = config.local.authorizedKeys.jfelice;
    };
    users.groups.twou.gid = 151928526;

    security.sudo.extraConfig = ''
      jfelice  ALL=(ALL:ALL) NOPASSWD: ALL
    '';

    home-manager.verbose = true;

    system.stateVersion = "22.05";
    home-manager.users.jfelice.home.stateVersion = "22.05";

    local.plan9.cpu.enable = true;
    local.tmux.paneZeroCommand = "weechat";
  };
}
