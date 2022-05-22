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
  in
    mapAttrs (name: value: fixVariableReferences value) vars;

  homeDirectory = config.users.users.jfelice.home;
in {
  config = {
    environment.systemPackages = with pkgs; [ kitty ];
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
          name = "Input Mono Narrow Light";
          size = 11;
        };
        settings = {
          bold_font = "Input Mono Narrow Medium";
          italic_font = "Input Mono Narrow Light Italic";
          bold_italic_font = "Input Mono Narrow Medium Italic";

          allow_remote_control = true;

          scrollback_lines = "5000";

          window_resize_step_cells = 1;
          window_resize_step_lines = 1;
          window_margin_width = "5.0";

          macos_option_as_alt = true;
          macos_thicken_font = "0.5";

          foreground = "#bfc7d5";
          background = "#14161f";

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
        keybindings = {
          "shift+ctrl+g" = "launch --stdin-add-formatting --stdin-source=@screen_scrollback --cwd=current --type=overlay kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count";
          "shift+ctrl+h" = "launch --stdin-add-formatting --stdin-source=@last_cmd_output --cwd=current --type=overlay kak-scrollback-pager @scrolled-by @cursor-x @cursor-y @line-count";
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
