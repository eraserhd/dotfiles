{ lib, config, pkgs, ... }:

with lib;
let
  cfg = config.old-homebrew;

  caskAppdirStatement = if (isNull cfg.cask_args.appdir)
                        then ""
                        else "cask_args appdir: '${cfg.cask_args.appdir}'";

  statements = word: values: (concatMapStrings (value: "${word} '${value}'\n") values);

  brewfile = pkgs.writeTextFile {
    name = "Brewfile";
    text = ''
      ${caskAppdirStatement}

      ${statements "tap" cfg.taps}

      ${statements "cask" cfg.casks}

      ${statements "brew" cfg.formulae}
    '';
  };

in {
  options.old-homebrew = {
    enable = mkEnableOption "Homebrew";

    taps = mkOption {
      description = "taps to retrieve formulae from";
      type = with types; listOf str;
      example = "homebrew/cask-fonts";
    };

    cask_args.appdir = mkOption {
      description = "path for Mac OS application bundles";
      type = with types; nullOr path;
      default = null;
      example = "/Applications";
    };

    casks = mkOption {
      description = "casks to be installed";
      type = with types; listOf str;
      default = [];
      example = "spotify";
    };

    formulae = mkOption {
      description = "formulae to be installed";
      type = with types; listOf str;
      default = [];
      example = "openscad";
    };
  };

  config = mkIf cfg.enable {
    old-homebrew.taps = [ "homebrew/bundle" ];

    system.activationScripts.extraUserActivation.text = ''
      PATH="/usr/local/bin:$PATH" HOMEBREW_NO_AUTO_UPDATE=1 brew bundle --no-lock --file="${brewfile}"
    '';
  };
}
