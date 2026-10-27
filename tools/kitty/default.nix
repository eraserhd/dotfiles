{ config, lib, pkgs, options, ... }:

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
      # Causes us to correctly load the environment for the new login shell
      __ETC_PROFILE_DONE="";
      __ETC_ZSHENV_SOURCED="";
      __ETC_ZSHRC_SOURCED="";
      __NIXOS_SET_ENVIRONMENT_DONE="";
    });
  in
    mapAttrs (name: value: fixVariableReferences value) vars;

  homeDirectory = config.users.users.jfelice.home;

  # hacky
  isMacOS =  builtins.hasAttr "launchd" options;

  # kitty-popup NAME COMMAND [ARG...] runs COMMAND in a new window of the
  # running kitty, for the window manager to float by NAME (AeroSpace matches
  # the title, xmonad the class, which macOS has none of).  It opens at its
  # final size, since programs like broot miss resizes which arrive while they
  # are starting, and kitty brings it to the front.
  kittyPopup = pkgs.writeShellApplication {
    name = "kitty-popup";
    text = ''
      name=$1
      shift
      exec ${pkgs.kitty}/bin/kitty --single-instance ${optionalString (!isMacOS) "--class=\"$name\""} --title="$name" \
        -o remember_window_size=no \
        -o initial_window_width=160c \
        -o initial_window_height=45c \
        -o confirm_os_window_close=0 \
        "$@"
    '';
  };
in {
  config = {
    local.terminalPopupCommand = "${kittyPopup}/bin/kitty-popup";

    environment.systemPackages = with pkgs; [
      kitty
      kittyPopup
      local.kak-scrollback-pager
    ];

    # This seems to block on a lot of systems, including OpenWRT and CNC.
    #environment.interactiveShellInit = ''
    #  ssh() {
    #    kitty +kitten ssh "$@"
    #  }
    #'';

    home-manager.users.jfelice = { pkgs, ... }: {
      programs.kitty = {
        enable = true;
        font = {
          name = "mononoki-Regular";
          size = (if isMacOS
                  then 13
                  else 10);
        };
        settings = {
          bold_font = "mononoki-Bold";
          italic_font = "mononoki-Italic";
          bold_italic_font = "mononoki-BoldItalic";
          # The prompt's triangle.  Otherwise it falls back to a bitmap
          # font, which can't be scaled to fit, so it's clipped.
          symbol_map = mkIf (!isMacOS) "U+1405 DejaVu Sans";

          allow_remote_control = true;
          listen_on = "unix:\${XDG_RUNTIME_DIR}/kitty";

          scrollback_lines = "5000";

          window_resize_step_cells = 1;
          window_resize_step_lines = 1;
          window_margin_width = "5.0";

          macos_option_as_alt = true;

          shell_integration = "enabled";
          clipboard_control = "write-clipboard read-clipboard";

          foreground = "#bfc7d5";
          background = "#14161f";

          paste_actions = "quote-urls-at-prompt";

          confirm_os_window_close = 0;

          cursor_text_color = "#c782ea";

          #: black
          color0 = "#292d3e";
          color8 = "#292d3e";

          #: red
          color1 = "#ff5370";
          color9 = "#ff5370";

          #: green
          color2 = "#c3e88d";
          color10 = "#c3e88d";

          #: yellow
          color3 = "#ffcb6b";
          color11 = "#ffcb6b";

          #: blue
          color4 = "#82b1ff";
          color12 = "#82b1ff";

          #: magenta
          color5 = "#c792ea";
          color13 = "#c792ea";

          #: cyan
          color6 = "#89ddff";
          color14 = "#89ddff";

          #: white
          color7 = "#bfc7d5";
          color15 = "#bfc7d5";
        };
        # Not sure why these work different on MacOS than Linux
        extraConfig = mkIf isMacOS ''
          modify_font cell_height -2px
          modify_font baseline +2px
          modify_font cell_width 85%
        '';
        keybindings = {
          "shift+ctrl+g" = "launch --stdin-add-formatting --stdin-source=@screen_scrollback --cwd=current --type=overlay ${pkgs.local.kak-scrollback-pager}/bin/kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count";
          "shift+ctrl+h" = "launch --stdin-add-formatting --stdin-source=@last_cmd_output --cwd=current --type=overlay ${pkgs.local.kak-scrollback-pager}/bin/kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count";
        };
        environment = environment "jfelice";
      } // (if pkgs.stdenv.hostPlatform.isDarwin
            then {
              darwinLaunchOptions = [
                "--single-instance"
                "--directory=${homeDirectory}/src"
              ];
            }
            else {}
      );
    };
  };
}
