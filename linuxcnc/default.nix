{ options, pkgs, ... }:

let
  linuxcnc = pkgs.callPackage ./linuxcnc.nix {};
in {
  config = if (builtins.hasAttr "systemd" options)
  then {
    environment.systemPackages = [ linuxcnc ];
  }
  else {
  };
}
