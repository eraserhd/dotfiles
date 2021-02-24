{ config, lib, ... }:

# This is a stub module for NixOS so the same config modules can be used as
# on nix-darwin.

with lib;
{
  options.homebrew = {
    enable = mkEnableOption "Homebrew";

    taps = mkOption {
      type = with types; listOf str;
      default = [];
    };

    extraConfig = mkOption {
      type = types.str;
      default = "";
    };

    casks = mkOption {
      type = with types; listOf str;
      default = [];
    };

    formulae = mkOption {
      type = with types; listOf str;
      default = [];
    };
  };

  config = {
    assertions = [
      {
        assertion = !config.homebrew.enable;
        message = "Cannnot enable Homebrew support on NixOS";
      }
    ];
  };
}
