{ options, pkgs, ... }:

{
  config = (if (builtins.hasAttr "launchd" options)
  then {
    homebrew.casks = [ "anki" ];
  }
  else {
    environment.systemPackages = [ pkgs.anki ];
  });
}
