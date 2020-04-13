{ options, pkgs, ... }:

{
  config = if (builtins.hasAttr "skhd" options.services)
  then {
    services.skhd.enable = true;
    services.skhd.skhdConfig = ''
      :: ctrlw @
      :: swap @

      ctrl - w ; ctrlw
      ctrlw < escape ; default

      ctrlw < h : yabai -m window --focus west ; skhd -k escape
      ctrlw < j : yabai -m window --focus south ; skhd -k escape
      ctrlw < k : yabai -m window --focus north ; skhd -k escape
      ctrlw < l : yabai -m window --focus east ; skhd -k escape

      ctrlw < r : yabai -m window --focus last ; skhd -k escape

      ctrlw < 0x1D : yabai -m window --focus first ; skhd -k escape

      ctrlw < s ; swap
      swap < escape ; default

      swap < h : yabai -m window --swap west ; skhd -k escape
      swap < j : yabai -m window --swap south ; skhd -k escape
      swap < k : yabai -m window --swap north ; skhd -k escape
      swap < l : yabai -m window --swap east ; skhd -k escape

      swap < r : yabai -m window --swap last ; skhd -k escape
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
