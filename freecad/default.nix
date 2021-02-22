{ options, pkgs, ... }:

{
  config = (if (builtins.hasAttr "old-homebrew" options)
  then {
    old-homebrew.casks = [ "freecad" ];
  }
  else {
    environment.systemPackages = [ pkgs.freecad ];
  });
}
