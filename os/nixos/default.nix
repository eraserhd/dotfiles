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

      time.timeZone = "America/New_York";
    }
    (mkIf config.local.bluetooth.enable {
      hardware.bluetooth = {
        enable = true;
        powerOnBoot = true;
        settings = {
          General = {
            Enable = "Source,Sink,Media,Socket";
            Experimental = true;
            KernelExperimental = true;
            FastConnectable = true;
            MultiProfile = "multiple";
            JustWorksRepairing = "always";
          };
          Policy = {
            AutoEnable = true;
          };
        };
        package = pkgs.bluez5-experimental;
      };
      # for bluetoothctl
      environment.systemPackages = with pkgs; [
        bluez
      ];
    })
  ];
}
