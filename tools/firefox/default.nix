{ pkgs, options, ... }:

{
  config = if (builtins.hasAttr "launchd" options)
  then {
    # currently, this is manual
    # homebrew.casks = [ "firefox" ];
  }
  else {
    environment.systemPackages = with pkgs; [ firefox ];
  };
}
