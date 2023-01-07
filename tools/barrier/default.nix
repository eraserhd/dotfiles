{ options, pkgs, ... }:

{
  config =
    (if (builtins.hasAttr "launchd" options)
     then {
       homebrew.casks = [ "barrier" ];
     }
     else {
       environment.systemPackages = with pkgs; [ barrier ];
     });
}
