{ lib, pkgs, ... }:

with lib;
{
  options = {
    local.browserCommand = mkOption {
      description = "Command to open a URL in a browser";
      type = types.str;
      default = "open";
    };
  };

  config = {
    nixpkgs.overlays = [
      (self: super: {
        open-in-chrome-tab = super.callPackage ../pkg/open-in-chrome-tab {};
      })
    ];

    environment.systemPackages = mkIf pkgs.stdenv.isDarwin [ pkgs.open-in-chrome-tab ];
  };
}
