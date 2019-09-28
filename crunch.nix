# Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./common.nix
      ./home-manager/nixos
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.cleanTmpDir = true;

  local.systemDisplayName = "crunch";

  networking = {
      hostName = "crunch";
      useDHCP = false;
      interfaces.enp4s0.ipv4 = {
        addresses = [ {
          address =  "10.0.0.2";
          prefixLength = 24;
        } ];
        routes = [
          { address = "10.0.0.0"; prefixLength = 24; }
          { address = "0.0.0.0"; prefixLength = 0; via = "10.0.0.1"; }
        ];
      };
      nameservers = [ "8.8.8.8" "8.8.4.4" ];
      firewall.enable = true;
  };

  time.timeZone = "America/New_York";

  environment.systemPackages = with pkgs; [
    git
    git-crypt
    gnupg
    manpages
    zlib # Hack to get binary rep working
  ];

  documentation.dev.enable = true;
  virtualisation.docker.enable = true;

  services.openssh = {
    enable = true;
    ports = [ 22 443 ];
    extraConfig = ''
      ClientAliveCountMax 3
      ClientAliveInterval 10
      StreamLocalBindUnlink yes
    '';
  };

  nix.nixPath = [
    "nixpkgs=/home/jfelice/src/dotfiles/nixpkgs"
    "nixpkgs-overlays=/home/jfelice/src/dotfiles/overlays"
    "nixos-config=/home/jfelice/src/dotfiles/crunch.nix"
  ];

  nixpkgs.config.allowUnfree = true;

  users.users = {
    jfelice = {
      isNormalUser = true;
      extraGroups = [ "docker" "wheel" ]; # Enable ‘sudo’ for the user.
      openssh.authorizedKeys.keys = config.local.authorizedKeys;
    };
    root.openssh.authorizedKeys.keys = config.local.authorizedKeys;
  };

  home-manager.verbose = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03";

  local.plan9.cpu.enable = true;
  local.sendOutgoingMailWithSES.enable = true;
  local.updateDNS.enable = true;
}
