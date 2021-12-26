{ options, pkgs, ... }:

{
  config = (if (builtins.hasAttr "homebrew" options)
  then {
    homebrew.casks = [ "freecad" ];
  }
  else {
    environment.systemPackages = [ pkgs.freecad ];
  });
}
