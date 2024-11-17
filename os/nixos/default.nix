{ options, config, inputs, lib, pkgs, ... }:

with lib;
{
  imports = [
    ./modules
  ];

  options = {
    local.bluetooth.enable = mkEnableOption "Bluetooth";
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        pciutils
      ];

      nix.nixPath = [
        "nixpkgs=${inputs.nixpkgs}"
      ];
    }
    (mkIf config.local.bluetooth.enable {
      hardware.bluetooth = {
        enable = true;
        powerOnBoot = true;
        settings = {
          General = {
            Experimental = true;
          };
        };
      };
      #hardware.pulseaudio.enable = true;
      #hardware.pulseaudio.package = pkgs.pulseaudioFull;
      # for bluetoothctl
      environment.systemPackages = with pkgs; [
        bluez
        #pulseaudioFull
      ];
    })
  ];
}
