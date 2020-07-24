{ lib, config, ... }:

with lib;
let
  cfg = config.homebrew;


in {
  options.homebrew = {
    enable = mkEnableOption "Homebrew";

    taps = mkOption {
      description = "taps to retrieve formulae from";
      type = with types; listOf str;
    };

    cask_args.appdir = mkOption {
      description = "path for Mac OS application bundles";
      type = with types; nullOr path;
      default = null;
      example = ''???
      '';
    };

    casks = mkOption {
      description = "casks to be installed";
      type = with types; listOf str;
    };

    formulae = mkOption {
      description = "formulae to be installed";
      type = with types; listOf str;
    };
  };

  config = mkIf cfg.enable {
    homebrew.taps = [ "homebrew/bundle" ];
  };
}
