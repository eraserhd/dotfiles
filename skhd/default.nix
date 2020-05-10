{ options, pkgs, ... }:

{
  config = if (builtins.hasAttr "skhd" options.services)
  then {
    services.skhd.enable = true;
    services.skhd.skhdConfig = ''
      :: ctrlw @
      :: swap @
      :: keycommand @

      ctrl - w ; ctrlw

      ctrlw < escape ; default
      ctrlw < f14 ; default

      ctrlw < h : yabai -m window --focus west ; skhd -k escape
      ctrlw < j : yabai -m window --focus south ; skhd -k escape
      ctrlw < k : yabai -m window --focus north ; skhd -k escape
      ctrlw < l : yabai -m window --focus east ; skhd -k escape

      ctrlw < p : yabai -m window --focus recent ; skhd -k escape
      ctrlw < r : yabai -m window --focus last ; skhd -k escape

      ctrlw < 0x1D : yabai -m window --focus $(yabai-window-number 0) ; skhd -k escape
      ctrlw < 1 : yabai -m window --focus $(yabai-window-number 1) ; skhd -k escape
      ctrlw < 2 : yabai -m window --focus $(yabai-window-number 2) ; skhd -k escape
      ctrlw < 3 : yabai -m window --focus $(yabai-window-number 3) ; skhd -k escape
      ctrlw < 4 : yabai -m window --focus $(yabai-window-number 4) ; skhd -k escape
      ctrlw < 5 : yabai -m window --focus $(yabai-window-number 5) ; skhd -k escape
      ctrlw < 6 : yabai -m window --focus $(yabai-window-number 6) ; skhd -k escape
      ctrlw < 7 : yabai -m window --focus $(yabai-window-number 7) ; skhd -k escape
      ctrlw < 8 : yabai -m window --focus $(yabai-window-number 8) ; skhd -k escape
      ctrlw < 9 : yabai -m window --focus $(yabai-window-number 9) ; skhd -k escape

      ctrlw < c : yabai -m display --focus 1 ; skhd -k escape
      ctrlw < shift - c : \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai -m window --space coding ; \
        yabai -m window --focus $window_id ; \
        skhd -k escape
      ctrlw < m : yabai -m display --focus 1 ; skhd -k escape
      ctrlw < shift - m : \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai -m window --space meeting ; \
        yabai -m window --focus $window_id ; \
        skhd -k escape
      ctrlw < b : yabai -m display --focus 2 ; skhd -k escape
      ctrlw < shift - b : \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai -m window --space browse ; \
        yabai -m window --focus $window_id ; \
        yabai -m window --grid 1:1:0:0:1:1 ; \
        skhd -k escape

      ctrlw < 0x2B : kitty @ --to unix:/Users/jfelice/.run/kitty send-text \
        --match=title:kak_repl_window '\x10\x0d'

      ctrlw < 0x18 : yabai -m space --balance
      ctrlw < 0x2C : yabai -m window --toggle split

      ctrlw < s ; swap
      swap < escape ; default

      swap < h : yabai -m window --swap west ; skhd -k escape
      swap < j : yabai -m window --swap south ; skhd -k escape
      swap < k : yabai -m window --swap north ; skhd -k escape
      swap < l : yabai -m window --swap east ; skhd -k escape

      swap < p : yabai -m window --swap recent ; skhd -k escape
      swap < r : yabai -m window --swap last ; skhd -k escape

      swap < 0x1D : yabai -m window --swap $(yabai-window-number 0) ; skhd -k escape
      swap < 1 : yabai -m window --swap $(yabai-window-number 1) ; skhd -k escape
      swap < 2 : yabai -m window --swap $(yabai-window-number 2) ; skhd -k escape
      swap < 3 : yabai -m window --swap $(yabai-window-number 3) ; skhd -k escape
      swap < 4 : yabai -m window --swap $(yabai-window-number 4) ; skhd -k escape
      swap < 5 : yabai -m window --swap $(yabai-window-number 5) ; skhd -k escape
      swap < 6 : yabai -m window --swap $(yabai-window-number 6) ; skhd -k escape
      swap < 7 : yabai -m window --swap $(yabai-window-number 7) ; skhd -k escape
      swap < 8 : yabai -m window --swap $(yabai-window-number 8) ; skhd -k escape
      swap < 9 : yabai -m window --swap $(yabai-window-number 9) ; skhd -k escape

      f13 ; keycommand
      keycommand < f14 ; default
      keycommand < h : yabai -m window --focus west
      keycommand < j : yabai -m window --focus south
      keycommand < k : yabai -m window --focus north
      keycommand < l : yabai -m window --focus east
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
