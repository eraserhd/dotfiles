{ config, lib, ... }:

with lib;
{
  options.homebrew = {
    enable = mkEnableOption "Homebrew";

    taps = mkOption {
      type = with types; listOf str;
      default = [];
    };

    cask_args.appdir = mkOption {
      type = with types; nullOr path;
      default = null;
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
