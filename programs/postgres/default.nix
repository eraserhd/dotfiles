{ lib, options, ... }:

with lib;
{
  config = (if (builtins.hasAttr "launchd" options)
  then {
    system.activationScripts.extraUserActivation.text = ''
      rm -f ~/.pgpass
      cp -ap ${toString ./pgpass.conf} ~/.pgpass
      chmod 600 ~/.pgpass
    '';
  }
  else {
    # FIXME: NixOS activation as above
  });
}
