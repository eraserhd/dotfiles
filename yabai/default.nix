{ ... }:

{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".yabairc" = {
        executable = true;
        text = ''
          #!/usr/bin/env sh

          # bar settings
          yabai -m config status_bar                   off
          yabai -m config status_bar_text_font         "Helvetica Neue:Bold:12.0"
          yabai -m config status_bar_icon_font         "FontAwesome:Regular:12.0"
          yabai -m config status_bar_background_color  0xff202020
          yabai -m config status_bar_foreground_color  0xffa8a8a8
          yabai -m config status_bar_space_icon_strip  I II III IV V VI VII VIII IX X
          yabai -m config status_bar_power_icon_strip   
          yabai -m config status_bar_space_icon        
          yabai -m config status_bar_clock_icon        

          # global settings
          yabai -m config mouse_follows_focus          off
          yabai -m config focus_follows_mouse          off
          yabai -m config window_placement             second_child
          yabai -m config window_topmost               off
          yabai -m config window_opacity               off
          yabai -m config window_opacity_duration      0.0
          yabai -m config window_shadow                on
          yabai -m config window_border                off
          yabai -m config window_border_placement      inset
          yabai -m config window_border_width          4
          yabai -m config window_border_radius         -1.0
          yabai -m config active_window_border_topmost off
          yabai -m config active_window_border_color   0xff775759
          yabai -m config normal_window_border_color   0xff505050
          yabai -m config insert_window_border_color   0xffd75f5f
          yabai -m config active_window_opacity        1.0
          yabai -m config normal_window_opacity        0.90
          yabai -m config split_ratio                  0.50
          yabai -m config auto_balance                 off
          yabai -m config mouse_modifier               fn
          yabai -m config mouse_action1                move
          yabai -m config mouse_action2                resize

          # general space settings
          yabai -m config layout                       bsp
          yabai -m config top_padding                  3
          yabai -m config bottom_padding               3
          yabai -m config left_padding                 3
          yabai -m config right_padding                3
          yabai -m config window_gap                   3

          # Laptop window
          yabai -m config --space 1 layout float
          yabai -m space 1 --label browse

          # Monitor
          yabai -m space 2 --label coding
          yabai -m space 3 --label meeting

          # Rules
          yabai -m rule --add label=kitty app=kitty space=2

          yabai -m rule --add label=Music app=Music space=1
          yabai -m rule --add label=Spotify app=Spotify space=1

          # Things not to manage
          yabai -m rule --add label=preferences app='System Preferences' manage=off

          echo "yabai configuration loaded.."
        '';
      };
    };
  };
}
