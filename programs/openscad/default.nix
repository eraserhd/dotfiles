{ pkgs, options, config, lib, ... }:

with lib;
let
  cfg = config.local.kits._3d-printing;
in {
  config = mkIf cfg.enable
    (if (builtins.hasAttr "homebrew" options)
     then {
       homebrew.casks = [ "openscad" ];
     }
     else {
       environment.systemPackages = with pkgs; [ openscad ];
     });
}
