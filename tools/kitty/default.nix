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
    });
  in
    mapAttrs (name: value: fixVariableReferences value) vars;

  homeDirectory = config.users.users.jfelice.home;

  # hacky
  isMacOS =  builtins.hasAttr "launchd" options;
in {
  config = {
    environment.systemPackages = with pkgs; [
      kitty
      local.kak-scrollback-pager
    ];
    environment.interactiveShellInit = ''
      ssh() {
        kitty +kitten ssh "$@"
      }
    '';

    programs.zsh.interactiveShellInit = ''
      if [ -n "$KITTY_INSTALLATION_DIR" ]; then
        autoload -Uz -- "$KITTY_INSTALLATION_DIR"/shell-integration/zsh/kitty-integration
        kitty-integration
        unfunction kitty-integration
      fi
    '';

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

          allow_remote_control = true;

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
      } // (if pkgs.stdenv.isDarwin
            then {
              darwinLaunchOptions = [
                "--listen-on=unix:${homeDirectory}/.run/kitty"
                "--single-instance"
                "--directory=${homeDirectory}/src"
              ];
            }
            else {}
      );
    };
  };
}
