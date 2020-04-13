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

      ctrlw < r : yabai -m window --focus last ; skhd -k escape

      ctrlw < 0x1D : yabai -m window --focus first ; skhd -k escape
      ctrlw < 0x1E : \
        yabai -m window --focus first ; \
        for (( i = 0; i < 1; i++ )); do \
          yabai -m window --focus next ; \
        done ; \
        skhd -k escape
      ctrlw < 0x1F : \
        skhd -k escape ; \
        yabai -m window --focus first ; \
        for (( i = 0; i < 2; i++ )); do \
          yabai -m window --focus next ; \
        done
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
