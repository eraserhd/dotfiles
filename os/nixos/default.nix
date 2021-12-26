{ options, config, lib, pkgs, ... }:

with lib;
{
  imports = [
    ./modules
  ];

  options = {
    local.bluetooth.enable = mkEnableOption "Bluetooth";
  };

  config = mkMerge [
    (mkIf (!pkgs.stdenv.isDarwin) {
      environment.systemPackages = with pkgs; [
        pciutils
      ];
    })
    (mkIf config.local.bluetooth.enable
     (if (builtins.hasAttr "hardware" options)
      then {
        hardware.bluetooth.enable = true;
        hardware.pulseaudio.enable = true;
        hardware.pulseaudio.package = pkgs.pulseaudioFull;
        # for bluetoothctl
        environment.systemPackages = with pkgs; [
          bluez
          pulseaudioFull
        ];
      }
      else {
        assertions = [{
          assertion = !config.local.bluetooth.enable;
          message = "local.bluetooth only supported on Linux";
        }];
      }))
  ];
}
