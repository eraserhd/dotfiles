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

      :: ctrlww : skhd -k 'ctrl - w'
      ctrlw < ctrl - w ; ctrlww
      ctrlw < w ; ctrlww
      ctrlw < 0x2F ; ctrlww
      ctrlww < ctrl - w -> ; default

      ctrlw < h : skhd -k escape ; yabai -m window --focus west
      ctrlw < j : skhd -k escape ; yabai -m window --focus south
      ctrlw < k : skhd -k escape ; yabai -m window --focus north
      ctrlw < l : skhd -k escape ; yabai -m window --focus east

      ctrlw < p : skhd -k escape ; \
        yabai -m space --focus code ; \
        yabai -m window --focus recent
      ctrlw < r : skhd -k escape ; \
        kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:kak_repl_window
      ctrlw < shift - r : skhd -k escape ; \
        kitty @ --to unix:/Users/jfelice/.run/kitty focus-window --match=title:shell_window

      ctrlw < 0x1D : skhd -k escape ; yabai -m window --focus $(yabai-window-number 0)
      ctrlw < 1 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 1)
      ctrlw < 2 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 2)
      ctrlw < 3 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 3)
      ctrlw < 4 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 4)
      ctrlw < 5 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 5)
      ctrlw < 6 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 6)
      ctrlw < 7 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 7)
      ctrlw < 8 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 8)
      ctrlw < 9 : skhd -k escape ; yabai -m window --focus $(yabai-window-number 9)

      ctrlw < c : skhd -k escape ; yabai-focus-space code
      ctrlw < shift - c : skhd -k escape ; \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai-focus-space code ; \
        yabai -m window --focus $window_id
      ctrlw < f : skhd -k escape ; yabai-focus-space focus
      ctrlw < shift - f : skhd -k escape ; \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai-focus-space focus ; \
        yabai -m window --focus $window_id
      ctrlw < b : skhd -k escape ; yabai-focus-space browse
      ctrlw < shift - b : \
        skhd -k escape ; \
        window_id=$(yabai -m query --windows --window |jq .id) ; \
        yabai-focus-space browse ; \
        yabai -m window --focus $window_id ; \
        yabai -m window --grid 1:1:0:0:1:1

      ctrlw < 0x2B : skhd -k escape ; \
        kitty @ --to unix:/Users/jfelice/.run/kitty send-text --match=title:kak_repl_window '\x10\x0d'

      ctrlw < 0x18 : skhd -k esacpe ; yabai -m space --balance
      ctrlw < 0x2C : skhd -k esacpe ; yabai -m window --toggle split

      ctrlw < s ; swap
      swap < escape ; default

      swap < h : skhd -k escape ; yabai -m window --swap west
      swap < j : skhd -k escape ; yabai -m window --swap south
      swap < k : skhd -k escape ; yabai -m window --swap north
      swap < l : skhd -k escape ; yabai -m window --swap east

      swap < p : skhd -k escape ; yabai -m window --swap recent
      swap < r : skhd -k escape ; yabai -m window --swap last

      swap < 0x1D : skhd -k escape ; yabai -m window --swap $(yabai-window-number 0)
      swap < 1 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 1)
      swap < 2 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 2)
      swap < 3 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 3)
      swap < 4 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 4)
      swap < 5 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 5)
      swap < 6 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 6)
      swap < 7 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 7)
      swap < 8 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 8)
      swap < 9 : skhd -k escape ; yabai -m window --swap $(yabai-window-number 9)

      ctrlw < i ; warp
      warp < escape ; default
      warp < h : skhd -k escape ; \
        yabai -m window --warp west ; \
        yabai -m space code --balance
      warp < j : skhd -k escape ; \
        yabai -m window --warp south ; \
        yabai -m space code --balance
      warp < k : skhd -k escape ; \
        yabai -m window --warp north ; \
        yabai -m space code --balance
      warp < l : skhd -k escape ; \
        yabai -m window --warp east ; \
        yabai -m space code --balance
      warp < 0x1D : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 0) ; \
        yabai -m space code --balance
      warp < 1 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 1) ; \
        yabai -m space code --balance
      warp < 2 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 2) ; \
        yabai -m space code --balance
      warp < 3 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 3) ; \
        yabai -m space code --balance
      warp < 4 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 4) ; \
        yabai -m space code --balance
      warp < 5 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 5) ; \
        yabai -m space code --balance
      warp < 6 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 6) ; \
        yabai -m space code --balance
      warp < 7 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 7) ; \
        yabai -m space code --balance
      warp < 8 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 8) ; \
        yabai -m space code --balance
      warp < 9 : skhd -k escape ; \
        yabai -m window --space code ; \
        yabai -m window --warp $(yabai-window-number 9) ; \
        yabai -m space code --balance

      cmd + shift + alt + ctrl - k ; keycommand
      keycommand < escape ; default
      keycommand < h : yabai-focus-space code ; yabai -m window --focus west
      keycommand < j : yabai-focus-space code ; yabai -m window --focus south
      keycommand < k : yabai-focus-space code ; yabai -m window --focus north
      keycommand < l : yabai-focus-space code ; yabai -m window --focus east

      keycommand < n : notification --activate
      keycommand < m : notification --menu
      keycommand < i : notification --close
    '';
    environment.systemPackages = [ pkgs.skhd ];
  }
  else {
  };
}
