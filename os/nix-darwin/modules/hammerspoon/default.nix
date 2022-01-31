{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.hammerspoon;

  cliSymlink = pkgs.writeTextFile {
    name = "hammerspoon-cli";
    executable = true;
    destination = "/bin/hs";
    text = ''
      #!/bin/bash
      exec "/Applications/Hammerspoon.app/Contents/Frameworks/hs/hs" "$@"
    '';
  };
in {
  options.services.hammerspoon = {
    enable = mkEnableOption "Hammerspoon";

    enableCommandLine = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Whether to add `hs` symlink to the path for access to the command-line.
      '';
    };
  };

  config = mkIf cfg.enable {
    homebrew.casks = [ "hammerspoon" ];

    environment.systemPackages = mkIf cfg.enableCommandLine [
      cliSymlink
    ];
  };
}
