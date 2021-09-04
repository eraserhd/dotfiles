{ config, lib, pkgs, ... }:

{
  imports = [ ../../common.nix ];

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

  nix.maxJobs = 1;
  nix.buildCores = 20;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 15;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.cleanTmpDir = true;

  local.systemDisplayName = "crunch";

  networking.useDHCP = false;
  networking.interfaces.wlp65s0.useDHCP = true;

  networking = {
    hostName = "crunch";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall.enable = false;
    wireless = {
      enable = true;
      interfaces = [ "wlp65s0" ];
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

  #local.bluetooth.enable = true;
  #local.services.X11.enable = true;
  #services.xserver.videoDrivers = [ "nvidia" ];

  nix.nixPath = [
    "nixos-config=/home/jfelice/src/dotfiles/machines/crunch/default.nix"
  ];

  nixpkgs.config.allowUnfree = true;

  users.mutableUsers = false;
  users.users = let
    keys = [
"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDWF7+WLN7pSk+u/TVjJgwSgGbINq0TWbsHDQI29LNvnbsiMfqXPAUY91zXnaKI/jala3fMH9K96amMhMRRp5qt8X3paJCkTKzPpegxOVNopMOebX9uWf+1BcdUllk4ejNBjeS2dQ63MfpjZ3S5cFeji5G8gUqOLgqZohnPlITs4WU7YEOaIs/1xP/8iKw8MIYr00AUEYqQzq/ucX6ea0ACTHlrw7o0qfqFXWeL4PKaKbad9CwyMXpifAaAoGSW7p2X+bFUFx1uMr01rT+QT8s8+j/u/sVDteAkjMGHDXyMVYmEm1Zc8oa2fUw3OuDk8x/G4YRqZIXkKbCQWAb/5Alb"
"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC0/PrHaQ5nXnhmeT8ABrxe/to5e8bQz0Cn7N5uXD7JGBc65DdlLP0vPhhQH9GDUHtv+WX3AmkYpIlnUYFeXc1/sLoLbkAQQK8cZuUdx0MEQGWa4EYxIX9FLDrkmIOvs4o2QjQiWIIDmExbk62lNm2CKaQerMiy/m93vRkhpkGnTdnzPafs3DLAMbu4Za+WQGm6kJRdKaCxNw11Rl3cMeixkScwlOpjYIKEs0NbtkKwknAti7gdaxpcoGenjWOsj90GTClCgNZ8jjYQr+3Uo/5FOBlIvhU0S3ctjW7vG4Yww68aSMUrCDIiY3HawccN35X9uT1B6hpu3WpGqg/sz4YV"
    ];
  in {
    jfelice = {
      isNormalUser = true;
      home = "/home/jfelice";
      extraGroups = [ "docker" "wheel" ];
      openssh.authorizedKeys.keys = keys;
    };
    alex = {
      isNormalUser = true;
      home = "/home/alex";
      extraGroups = [ "docker" "wheel" ];
      #openssh.authorizedKeys.keys = config.local.authorizedKeys.alex;
    };
    root.openssh.authorizedKeys.keys = keys;
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
