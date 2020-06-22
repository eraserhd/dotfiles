{ options, pkgs, ... }:

{
  config = if (builtins.hasAttr "skhd" options.services)
  then {
    services.skhd.enable = true;
    services.skhd.skhdConfig = ''
      :: ctrlw @
      :: swap @
      :: warp @
      :: keycommand @

      ctrl - w ; ctrlw

      ctrlw < escape ; default
      ctrlw < f14 ; default

      ctrlw < h : yabai -m space --focus code ; yabai -m window --focus west ; skhd -k escape
      ctrlw < j : yabai -m space --focus code ; yabai -m window --focus south ; skhd -k escape
      ctrlw < k : yabai -m space --focus code ; yabai -m window --focus north ; skhd -k escape
      ctrlw < l : yabai -m space --focus code ; yabai -m window --focus east ; skhd -k escape

      ctrlw < p : yabai -m space --focus code ; yabai -m window --focus recent ; skhd -k escape
      ctrlw < r : kitty @ --to unix:/Users/jfelice/.run/kitty focus-window \
        --match=title:kak_repl_window ; skhd -k escape
      ctrlw < shift - r : kitty @ --to unix:/Users/jfelice/.run/kitty focus-window \
        --match=title:shell_window ; skhd -k escape

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

      ctrlw < c : yabai -m space --focus code ; skhd -k escape
      ctrlw < shift - c : \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai -m window --space code ; \
        yabai -m window --focus $window_id ; \
        skhd -k escape
      ctrlw < m : yabai -m space --focus meeting ; skhd -k escape
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
        --match=title:kak_repl_window '\x10\x0d' ; skhd -k escape

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

      ctrlw < i ; warp
      warp < escape ; default
      warp < h : yabai -m window --warp west ; skhd -k escape
      warp < j : yabai -m window --warp south ; skhd -k escape
      warp < k : yabai -m window --warp north ; skhd -k escape
      warp < l : yabai -m window --warp east ; skhd -k escape
      warp < 0x1D : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 0) ; skhd -k escape
      warp < 1 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 1) ; skhd -k escape
      warp < 2 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 2) ; skhd -k escape
      warp < 3 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 3) ; skhd -k escape
      warp < 4 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 4) ; skhd -k escape
      warp < 5 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 5) ; skhd -k escape
      warp < 6 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 6) ; skhd -k escape
      warp < 7 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 7) ; skhd -k escape
      warp < 8 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 8) ; skhd -k escape
      warp < 9 : yabai -m window --space code ; yabai -m window --warp $(yabai-window-number 9) ; skhd -k escape

      cmd + shift + alt + ctrl - k ; keycommand
      keycommand < escape ; default
      keycommand < h : yabai -m space --focus code ; yabai -m window --focus west
      keycommand < j : yabai -m space --focus code ; yabai -m window --focus south
      keycommand < k : yabai -m space --focus code ; yabai -m window --focus north
      keycommand < l : yabai -m space --focus code ; yabai -m window --focus east

      keycommand < n : notification --activate
      keycommand < m : notification --menu
      keycommand < i : notification --close
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
