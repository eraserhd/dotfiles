{ config, lib, pkgs, ... }:

with lib;
let
  environment = user: let
    fixVariableReferences = s:
      let
        result = builtins.match "(.*)\\$([a-zA-Z0-9_]+)(.*)" s;
      in
        if (builtins.isNull result)
        then s
        else (fixVariableReferences "${builtins.elemAt result 0}\${${builtins.elemAt result 1}}${builtins.elemAt result 2}");
    vars = config.environment.variables // {
      HOME = (builtins.getAttr user config.users.users).home;
    } // (if (hasAttr "systemPath" config.environment)
    then {
      PATH = config.environment.systemPath;
    }
    else {
    });

    directives = map (name: "env ${name}=${fixVariableReferences (getAttr name vars)}") (attrNames vars);
  in
    concatStringsSep "\n" directives;
in {
  config = {
    environment.systemPackages = with pkgs; [ kitty ];
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/kitty/macos-launch-services-cmdline".text = let
        homeDirectory = config.users.users.jfelice.home;
      in ''
        --listen-on=unix:/Users/jfelice/.run/kitty --single-instance --directory=${homeDirectory}/src
      '';
      home.file.".config/kitty/window_position.py".source = ./window_position.py;
      home.file.".config/kitty/kitty.conf".text = ''
        font_family mononoki
        font_size   12.0
        # symbol_map U+E0A0-U+E0A3,U+E0C0-U+E0C7 PowerlineSymbols
        # box_drawing_scale 0.001, 1, 1.5, 2

        # cursor #cccccc
        cursor_text_color #c782ea

        scrollback_lines 5000
        map shift+ctrl+h launch --stdin-add-formatting --stdin-source=@screen_scrollback --cwd=current --type=overlay kak-scrollback-pager

        window_resize_step_cells 1
        window_resize_step_lines 1

        # window_border_width 1.0
        # draw_minimal_borders yes
        window_margin_width 5.0
        # active_border_color #00ff00
        # inactive_border_color #cccccc
        # bell_border_color #ff5a00
        # inactive_text_alpha 1.0
        # hide_window_decorations no
        # resize_debounce_time 0.1
        # resize_draw_strategy static
        # resize_in_steps no

        #: Tab bar
        # tab_bar_edge bottom
        # tab_bar_margin_width 0.0
        # tab_bar_style fade
        # tab_bar_min_tabs 2
        # tab_switch_strategy previous
        # tab_fade 0.25 0.5 0.75 1
        # tab_separator " ┇"
        # tab_title_template "{title}"
        # active_tab_title_template none
        # active_tab_foreground   #000
        # active_tab_background   #eee
        # active_tab_font_style   bold-italic
        # inactive_tab_foreground #444
        # inactive_tab_background #999
        # inactive_tab_font_style normal
        # tab_bar_background none

        #: Color scheme

        foreground #bfc7d5
        background #14161f
        # background_opacity 1.0
        # background_image none
        # background_image_layout tiled
        # background_image_linear no
        # dynamic_background_opacity no
        # background_tint 0.0
        # dim_opacity 0.75
        # selection_foreground #000000
        # selection_background #fffacd

        #: black
        color0 #292d3e
        color8 #292d3e

        #: red
        color1 #ff5370
        color9 #ff5370

        #: green
        color2  #c3e88d
        color10 #c3e88d

        #: yellow
        color3  #ffcb6b
        color11 #ffcb6b

        #: blue
        color4  #82b1ff
        color12 #82b1ff

        #: magenta
        color5  #c792ea
        color13 #c792ea

        #: cyan
        color6  #89ddff
        color14 #89ddff

        #: white
        color7  #bfc7d5
        color15 #bfc7d5


        # mark1_foreground black
        # mark1_background #98d3cb
        # mark2_foreground black
        # mark2_background #f2dcd3
        # mark3_foreground black
        # mark3_background #f274bc

        # shell .
        # editor .
        # close_on_child_death no
        allow_remote_control yes
        ${environment "jfelice"}

        # update_check_interval 24
        # startup_session none
        # clipboard_control write-clipboard write-primary
        # term xterm-kitty

        #: OS specific tweaks

        # macos_titlebar_color system
        macos_option_as_alt yes
        # macos_hide_from_tasks no
        # macos_quit_when_last_window_closed no
        # macos_window_resizable yes
        macos_thicken_font 0.5
        # macos_traditional_fullscreen no
        # macos_show_window_title_in all
        # macos_custom_beam_cursor no
        # linux_display_server auto

        #: Keyboard shortcuts

        # kitty_mod ctrl+shift
        # clear_all_shortcuts no
        
        # kitten_alias hints hints --hints-offset=0

        # map kitty_mod+c  copy_to_clipboard
        # map cmd+c        copy_to_clipboard
        # map kitty_mod+v  paste_from_clipboard
        # map cmd+v        paste_from_clipboard
        # map kitty_mod+s  paste_from_selection
        # map shift+insert paste_from_selection
        # map kitty_mod+o  pass_selection_to_program

        #: Scrolling

        # map kitty_mod+up        scroll_line_up
        # map alt+cmd+page_up     scroll_line_up
        # map cmd+up              scroll_line_up
        # map kitty_mod+k         scroll_line_up
        # map kitty_mod+down      scroll_line_down
        # map kitty_mod+j         scroll_line_down
        # map alt+cmd+page_down   scroll_line_down
        # map cmd+down            scroll_line_down
        # map kitty_mod+page_up   scroll_page_up
        # map cmd+page_up         scroll_page_up
        # map kitty_mod+page_down scroll_page_down
        # map cmd+page_down       scroll_page_down
        # map kitty_mod+home      scroll_home
        # map cmd+home            scroll_home
        # map kitty_mod+end       scroll_end
        # map cmd+end             scroll_end
        # map kitty_mod+h         show_scrollback

        #: Window management

        # map kitty_mod+enter new_window
        # map cmd+enter   new_window
        # map kitty_mod+n new_os_window
        # map cmd+n       new_os_window
        # map kitty_mod+w close_window
        # map shift+cmd+d close_window
        # map kitty_mod+] next_window
        # map kitty_mod+[ previous_window
        # map kitty_mod+f move_window_forward
        # map kitty_mod+b move_window_backward
        # map kitty_mod+` move_window_to_top
        # map kitty_mod+r start_resizing_window
        # map cmd+r       start_resizing_window
        # map kitty_mod+1 first_window
        # map cmd+1       first_window
        # map kitty_mod+2 second_window
        # map cmd+2       second_window
        # map kitty_mod+3 third_window
        # map cmd+3       third_window
        # map kitty_mod+4 fourth_window
        # map cmd+4       fourth_window
        # map kitty_mod+5 fifth_window
        # map cmd+5       fifth_window
        # map kitty_mod+6 sixth_window
        # map cmd+6       sixth_window
        # map kitty_mod+7 seventh_window
        # map cmd+7       seventh_window
        # map kitty_mod+8 eighth_window
        # map cmd+8       eighth_window
        # map kitty_mod+9 ninth_window
        # map cmd+9       ninth_window
        # map kitty_mod+0 tenth_window

        #: Tab management

        # map kitty_mod+right next_tab
        # map ctrl+tab        next_tab
        # map shift+cmd+]     next_tab
        # map kitty_mod+left  previous_tab
        # map shift+ctrl+tab  previous_tab
        # map shift+cmd+[     previous_tab
        # map kitty_mod+t     new_tab
        # map cmd+t           new_tab
        # map kitty_mod+q     close_tab
        # map cmd+w           close_tab
        # map kitty_mod+.     move_tab_forward
        # map kitty_mod+,     move_tab_backward
        # map kitty_mod+alt+t set_tab_title
        # map shift+cmd+i     set_tab_title

        #: Font sizes

        # map cmd+shift+equal     change_font_size all +2.0
        # map kitty_mod+equal     change_font_size all +2.0
        # map cmd+plus            change_font_size all +2.0
        # map kitty_mod+minus     change_font_size all -2.0
        # map cmd+minus           change_font_size all -2.0
        # map kitty_mod+backspace change_font_size all 0
        # map cmd+0               change_font_size all 0

        #: Select and act on visible text

        # map kitty_mod+e kitten hints
        # map kitty_mod+p>f kitten hints --type path --program -
        # map kitty_mod+p>shift+f kitten hints --type path
        # map kitty_mod+p>l kitten hints --type line --program -
        # map kitty_mod+p>w kitten hints --type word --program -
        # map kitty_mod+p>h kitten hints --type hash --program -
        # map kitty_mod+p>n kitten hints --type linenum

        #: Miscellaneous

        # map kitty_mod+f11    toggle_fullscreen
        # map kitty_mod+f10    toggle_maximized
        # map kitty_mod+u      kitten unicode_input
        # map kitty_mod+f2     edit_config_file
        # map kitty_mod+escape kitty_shell window
        # map kitty_mod+a>m    set_background_opacity +0.1
        # map kitty_mod+a>l    set_background_opacity -0.1
        # map kitty_mod+a>1    set_background_opacity 1
        # map kitty_mod+a>d    set_background_opacity default
        # map kitty_mod+delete clear_terminal reset active
      '';
    };
  };
}
