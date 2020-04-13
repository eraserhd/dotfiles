{ options, pkgs, ... }:

{
  config = if (builtins.hasAttr "skhd" options.services)
  then {
    services.skhd.enable = true;
    services.skhd.skhdConfig = ''

      :: ctrlw @

      ctrl - w ; ctrlw
      ctrlw < escape ; default
      ctrlw < h : yabai -m window --focus west ; skhd -k escape
      ctrlw < j : yabai -m window --focus south ; skhd -k escape
      ctrlw < k : yabai -m window --focus north ; skhd -k escape
      ctrlw < l : yabai -m window --focus east ; skhd -k escape
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
