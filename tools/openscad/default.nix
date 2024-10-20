{ pkgs, options, config, lib, ... }:

with lib;
let
  cfg = config.local.kits._3d-printing;

in {
  config = mkIf cfg.enable
    (if (builtins.hasAttr "launchd" options)
     then {
       homebrew.casks = [ "openscad" ];
       environment.systemPackages = let
         openscad-wrapper = pkgs.writeScriptBin "openscad" ''
           readonly OPENSCAD=/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD
           exec "$OPENSCAD" "$@"
         '';
       in [ openscad-wrapper ];
     }
     else {
       environment.systemPackages = [ pkgs.openscad-unstable ];
     });
}
