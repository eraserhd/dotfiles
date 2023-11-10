{ lib, config, pkgs, ... }:

with lib;
{
  config = {
    home-manager.users.jfelice = { pkgs, ... }: {
      home.file.".config/i3/config".text = let
        homeDirectory = config.users.users.jfelice.home;

        directions = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
        };
        sigils = [
          "a" "b" "c" "d" "e" "f" "g" "i" "m" "n" "o" "p" "q" "r" "s" "t" "u"
          "v" "w" "x" "y" "z"
        ];

        eachDir = f: (concatStringsSep "\n" (attrValues (mapAttrs f directions))) + "\n";
        i3-quote = text: "\"" + (replaceStrings ["\""] ["\\\\\""] text)  + "\"";
        i3-exec = commands: "exec " + (i3-quote "${./sigils.clj} i3-exec ${escapeShellArg commands}");

        dirFocus = eachDir (key: dir: "bindsym ${key} focus ${dir} ; mode \"default\"");
        sigilFocus = concatMapStringsSep "\n" (sigil: "bindsym ${sigil} exec ${./sigils.clj} i3-exec '[id=$(${sigil})] focus' ; mode \"default\"") sigils;

        dirSwaps = eachDir (key: dir:
                            "bindsym $swap_key+${key} mark swapee ; " +
                            "focus ${dir} ; " +
                            "swap with mark swapee ; " +
                            "focus ${dir} ; " +
                            "mode \"default\"");
        sigilSwaps = concatMapStringsSep "\n" (sigil:
                                               "bindsym $swap_key+${sigil} " +
                                               (i3-exec ("mark swapee; " +
                                                         "[id=$(${sigil})] swap with mark swapee; " +
                                                         "[con_mark=swapee] focus; " +
                                                         "mode \"default\"")))
                                               sigils;
      in ''
        focus_follows_mouse no

        # *sigh* we actually always want to warp, but no option :(
        mouse_warping output

        set $ctrlw ctrlw
        set $swap_key Mod1

        bindsym Control+w mode "$ctrlw"

        mode "$ctrlw" {
          ${sigilFocus}
          ${dirFocus}

          ${dirSwaps}
          ${sigilSwaps}

          bindsym Shift+semicolon exec i3-input -F '%s' -P 'Command: ' ; mode "default"

          # bindsym Shift+r [con_mark="R"] focus ; mode "default"
          # bindsym r [con_mark="r"] focus ; mode "default"

          # These affect the next window, so we need to make one somehow?
          bindsym minus split vertical ; mode "default"
          bindsym Shift+backslash split horizontal ; mode "default"

          bindsym slash layout toggle split ; mode "default"

          bindsym Escape mode "default"
        }

        # i3 config file (v4)
        #
        # Please see https://i3wm.org/docs/userguide.html for a complete reference!

        set $mod Mod4

        # Font for window titles. Will also be used by the bar unless a different font
        # is used in the bar {} block below.
        font pango:monospace 8

        # This font is widely installed, provides lots of unicode glyphs, right-to-left
        # text rendering and scalability on retina/hidpi displays (thanks to pango).
        #font pango:DejaVu Sans Mono 8

        # The combination of xss-lock, nm-applet and pactl is a popular choice, so
        # they are included here as an example. Modify as you see fit.

        # xss-lock grabs a logind suspend inhibit lock and will use i3lock to lock the
        # screen before suspend. Use loginctl lock-session to lock your screen.
        exec --no-startup-id xss-lock --transfer-sleep-lock -- i3lock --nofork

        # NetworkManager is the most popular way to manage wireless networks on Linux,
        # and nm-applet is a desktop environment-independent system tray GUI for it.
        exec --no-startup-id nm-applet

        # Use pactl to adjust volume in PulseAudio.
        set $refresh_i3status killall -SIGUSR1 i3status
        bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ +10% && $refresh_i3status
        bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume @DEFAULT_SINK@ -10% && $refresh_i3status
        bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute @DEFAULT_SINK@ toggle && $refresh_i3status
        bindsym XF86AudioMicMute exec --no-startup-id pactl set-source-mute @DEFAULT_SOURCE@ toggle && $refresh_i3status

        # Use Mouse+$mod to drag floating windows to their wanted position
        floating_modifier $mod

        # start a terminal
        bindsym $mod+Return exec ${pkgs.kitty}/bin/kitty --listen-on=unix:$XDG_RUNTIME_DIR/kitty --single-instance --directory=${homeDirectory}/src

        # kill focused window
        bindsym $mod+Shift+q kill

        # start dmenu (a program launcher)
        bindsym $mod+d exec dmenu_run
        # There also is the (new) i3-dmenu-desktop which only displays applications
        # shipping a .desktop file. It is a wrapper around dmenu, so you need that
        # installed.
        # bindsym $mod+d exec --no-startup-id i3-dmenu-desktop

        # enter fullscreen mode for the focused container
        bindsym $mod+f fullscreen toggle

        # change container layout (stacked, tabbed, toggle split)
        bindsym $mod+s layout stacking
        # bindsym $mod+w layout tabbed
        bindsym $mod+e layout toggle split

        # toggle tiling / floating
        bindsym $mod+Shift+space floating toggle

        # change focus between tiling / floating windows
        bindsym $mod+space focus mode_toggle

        # focus the parent container
        bindsym $mod+p focus parent

        # focus the child container
        bindsym $mod+slash focus child

        # Define names for default workspaces for which we configure key bindings later on.
        # We use variables to avoid repeating the names in multiple places.
        set $ws1 "1"
        set $ws2 "2"
        set $ws3 "3"
        set $ws4 "4"
        set $ws5 "5"
        set $ws6 "6"
        set $ws7 "7"
        set $ws8 "8"
        set $ws9 "9"
        set $ws10 "10"

        # switch to workspace
        bindsym $mod+1 workspace number $ws1
        bindsym $mod+2 workspace number $ws2
        bindsym $mod+3 workspace number $ws3
        bindsym $mod+4 workspace number $ws4
        bindsym $mod+5 workspace number $ws5
        bindsym $mod+6 workspace number $ws6
        bindsym $mod+7 workspace number $ws7
        bindsym $mod+8 workspace number $ws8
        bindsym $mod+9 workspace number $ws9
        bindsym $mod+0 workspace number $ws10

        # move focused container to workspace
        bindsym $mod+Shift+1 move container to workspace number $ws1
        bindsym $mod+Shift+2 move container to workspace number $ws2
        bindsym $mod+Shift+3 move container to workspace number $ws3
        bindsym $mod+Shift+4 move container to workspace number $ws4
        bindsym $mod+Shift+5 move container to workspace number $ws5
        bindsym $mod+Shift+6 move container to workspace number $ws6
        bindsym $mod+Shift+7 move container to workspace number $ws7
        bindsym $mod+Shift+8 move container to workspace number $ws8
        bindsym $mod+Shift+9 move container to workspace number $ws9
        bindsym $mod+Shift+0 move container to workspace number $ws10

        # reload the configuration file
        bindsym $mod+Shift+c reload
        # restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
        bindsym $mod+Shift+r restart
        # exit i3 (logs you out of your X session)
        bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -B 'Yes, exit i3' 'i3-msg exit'"

        # resize window (you can also use the mouse for that)
        mode "resize" {
                # These bindings trigger as soon as you enter the resize mode

                # Pressing left will shrink the window’s width.
                # Pressing right will grow the window’s width.
                # Pressing up will shrink the window’s height.
                # Pressing down will grow the window’s height.
                bindsym h resize shrink width 10 px or 10 ppt
                bindsym j resize grow height 10 px or 10 ppt
                bindsym k resize shrink height 10 px or 10 ppt
                bindsym l resize grow width 10 px or 10 ppt

                # back to normal: Enter or Escape or $mod+r
                bindsym Return mode "default"
                bindsym Escape mode "default"
                bindsym $mod+r mode "default"
        }

        bindsym $mod+r mode "resize"

        # Start i3bar to display a workspace bar (plus the system information i3status
        # finds out, if available)
        bar {
                status_command i3status
        }
      '';
    };

  };
}
